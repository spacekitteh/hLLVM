{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards, TypeFamilies #-}

module Llvm.Pass.Visualization where
import Data.Maybe
import qualified Data.Set as Ds
import qualified Data.Map as Dm
import qualified Data.List as L

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Hir.Data
import Llvm.Hir.Composer
import Llvm.Hir.Cast
import Llvm.Hir.Internalization
import Llvm.Query.HirCxt
import Llvm.Query.Conversion
import Llvm.Query.HirType
import Llvm.Hir.Print
import Control.Monad (liftM,foldM)

{- 

This pass inserts code to printout the operands and results of store
and and load instructions at runtime. It should be the last pass
before the native code generation, any tranformation running after
this will skew the printout. The pass currently support visualizing
nodes that are open on both ends.  It can be exanded to supporting
nodes of other shapes if the needs arise.

-}

data VisualPlugin a = VisualPlugin { 
  -- the prefix added before each visualization string, this might be 
  -- needed to avoid naming collision. 
  visPrefix :: Maybe String
  -- the functions whose instructions should be visualized. 
  -- If the set does not exist, all functions are visualized
  , includedFunctions :: Maybe (Ds.Set GlobalId) 
  , visFunctions :: [FunctionDeclare]
  , captureCinsts :: Cinst -> Ds.Set String -> Ds.Set String                                     
  , visNodeOO :: TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O]
  }

sampleVisualPlugin :: VisualPlugin a
sampleVisualPlugin = 
  VisualPlugin { visPrefix = Just ".visual_"
               , includedFunctions = Nothing
               , visFunctions = []
               , captureCinsts = \comp f -> case comp of
                 I_store{..} -> Ds.insert (render $ printIr comp) f
                 I_load{..} -> Ds.insert (render $ printIr comp) f
                 I_getelementptr{..} -> Ds.insert (render $ printIr comp) f
                 _ -> f
               , visNodeOO = \te mp node -> case node of
                    (Cnode cinst _)  -> case Dm.lookup (render $ printIr cinst) mp of
                      Nothing -> [Comment $ Cstring $ render $ printIr (typeof te cinst), node]
                      Just x -> 
                        case cinst of
                          I_store{..} -> [Comment $ Cstring $ render $ printIr x, node]
                          I_load{..} -> [Comment $ Cstring $ render $ printIr (typeof te cinst), node]
                          I_getelementptr{..} -> [Comment $ Cstring $ render $ printIr (typeof te cinst), node]
                    _ -> [node]
               }

type Visualized = Ds.Set String 


emptyVisualized :: Visualized
emptyVisualized = Ds.empty 

visLattice :: H.DataflowLattice Visualized
visLattice = H.DataflowLattice
              { H.fact_name = "Visualized Instructions"
              , H.fact_bot = emptyVisualized
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
            where
              j = Ds.union old new 
              ch = H.changeIf (j /= old)

bwdScan :: H.FuelMonad m => (Cinst -> Ds.Set String -> Ds.Set String) -> H.BwdPass m (Node a) Visualized
bwdScan collectString = 
  let bwdTran :: (Node a) e x -> H.Fact x Visualized -> Visualized
      bwdTran n@(Tnode _ _) f = let bs = H.successors n
                                in foldl (\p l -> p `Ds.union` (fromMaybe emptyVisualized $ H.lookupFact l f)) 
                                   emptyVisualized bs
      bwdTran (Lnode _) f = f
      bwdTran (Pnode _ _) f = f
      bwdTran (Mnode _ _) f = f
      bwdTran (Comment _) f = f
      bwdTran (Enode _) f = f
      bwdTran n@(Cnode comp _) f = collectString comp f
  in H.BwdPass { H.bp_lattice = visLattice
               , H.bp_transfer = H.mkBTransfer bwdTran
               , H.bp_rewrite = H.noBwdRewrite
               }

scanDefine :: (CheckpointMonad m, FuelMonad m) => VisualPlugin a -> TlDefine a -> m Visualized
scanDefine visualPlugin (TlDefine fn entry graph) = 
  do { (_, a, b) <- H.analyzeAndRewriteBwd (bwdScan (captureCinsts visualPlugin)) (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyVisualized (H.lookupFact entry a))
     }
  
type VisualIds = Dm.Map String Int
                    
allocateVisualIds :: Dm.Map GlobalId Visualized -> VisualIds
allocateVisualIds mp = 
  let sl = Ds.toList $ Ds.unions (Dm.elems mp)
  in fst $ foldl (\(p,idx) e -> (Dm.insert e idx p, idx+1)) (Dm.empty,0) sl


scanModule :: (CheckpointMonad m, FuelMonad m) => VisualPlugin a -> Module a -> m VisualIds
scanModule visPlugin (Module l) = 
  do { l0 <- mapM (\x -> case x of
                      ToplevelDefine def@(TlDefine fn _ _) ->
                        do { fct <- scanDefine visPlugin def
                           ; return (Dm.insert (fi_fun_name fn) fct Dm.empty)
                           }
                      _ -> return Dm.empty 
                  ) l
     ; return $ allocateVisualIds (Dm.unions l0)
     }

{- rewrite this with foldBlock -}
rwBlockCC :: (TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O]) 
             -> TypeEnv -> Dm.Map String Const -> H.Block (Node a) C C -> H.Block (Node a) C C
rwBlockCC rwNodeOO te mp blk = let (f, m, l) = blockSplit blk 
                                   middles = blockToList m
                               in blockJoin f (blockFromList $ concat $ fmap (rwNodeOO te mp) middles) l

rwBlockCO :: (TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O]) 
             -> TypeEnv -> Dm.Map String Const -> H.Block (Node a) C O -> H.Block (Node a) C O
rwBlockCO rwNodeOO te mp blk = let (f, m) = blockSplitHead blk
                                   middles = blockToList m
                               in blockJoinHead f (blockFromList $ concat $ fmap (rwNodeOO te mp) middles)

rwBlockOO :: (TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O])
             -> TypeEnv -> Dm.Map String Const -> H.Block (Node a) O O -> H.Block (Node a) O O
rwBlockOO rwNodeOO te mp blk = let middles = blockToList blk
                               in blockFromList $ concat $ fmap (rwNodeOO te mp) middles

rwBlockOC :: (TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O])
             -> TypeEnv -> Dm.Map String Const -> H.Block (Node a) O C -> H.Block (Node a) O C
rwBlockOC rwNodeOO te mp blk = let (m,l) = blockSplitTail blk
                                   middles = blockToList m
                               in blockJoinTail (blockFromList $ concat $ fmap (rwNodeOO te mp) middles) l

rwBlock :: (TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O]) 
           -> TypeEnv -> Dm.Map String Const -> H.Block (Node a) e x -> H.Block (Node a) e x
rwBlock rwNodeOO te mp blk = case blk of
  BlockCO _ _ -> rwBlockCO rwNodeOO te mp blk
  BlockCC _ _ _ -> rwBlockCC rwNodeOO te mp blk
  BlockOC _ _ -> rwBlockOC rwNodeOO te mp blk
  BNil -> blk
  BMiddle _ -> rwBlockOO rwNodeOO te mp blk
  BCat _ _ -> rwBlockOO rwNodeOO te mp blk
  BSnoc _ _ -> rwBlockOO rwNodeOO te mp blk
  BCons _ _ -> rwBlockOO rwNodeOO te mp blk

rwDefine :: (TypeEnv -> Dm.Map String Const -> (Node a) O O  -> [(Node a) O O])
            -> TypeEnv -> Dm.Map String Const -> TlDefine a -> TlDefine a
rwDefine rwNodeOO te gmp (TlDefine fn entry graph) = 
  let graph0 = mapGraphBlocks (rwBlock rwNodeOO te gmp) graph
  in TlDefine fn entry graph0

rwModule :: VisualPlugin a -> Module a -> Dm.Map String GlobalId -> Module a
rwModule visPlugin m@(Module l) duM = 
  let (globals, duC) = stringize duM
      irCxt = irCxtOfModule m
  in Module $ globals 
     ++ (fmap (ToplevelDeclare . TlDeclare) (visFunctions visPlugin))
     ++ (fmap (\x -> case x of
                  ToplevelDefine def@(TlDefine fn _ _) -> 
                    if (maybe True (Ds.member (fi_fun_name fn)) (includedFunctions visPlugin))
                    then ToplevelDefine (rwDefine (visNodeOO visPlugin) (typeEnv $ globalCxt irCxt) duC def)
                    else x
                  _ -> x
              ) l)

stringize :: Dm.Map String GlobalId -> ([Toplevel a], Dm.Map String Const)
stringize mp = 
  let mp0 = Dm.mapWithKey (\c lhs -> internalize (lhs,c)) mp
  in (fmap ToplevelGlobal $ Dm.elems $ Dm.map llvmDef mp0, Dm.map llvmRef mp0)

visualize :: VisualPlugin a -> Module a -> Module a
visualize visPlugin m = 
  let mp = runSimpleUniqueMonad $ runWithFuel H.infiniteFuel
           ((scanModule visPlugin m)::H.SimpleFuelMonad VisualIds)
  in rwModule visPlugin m (Dm.map (\x -> GlobalIdAlphaNum $ ".visual_" ++ show x) mp)