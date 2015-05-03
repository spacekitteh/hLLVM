{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards, TypeFamilies #-}

module Llvm.Pass.Visualization where
import Data.Maybe
import qualified Data.Set as Ds
import qualified Data.Map as Dm
import qualified Data.List as L

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Data.Ir
import Llvm.Syntax.Printer.IrPrint
import Control.Monad (liftM,foldM)

{- 

This pass inserts code to printout the operands and results of store
and and load instructions at runtime. It should be the last pass before 
the native code generation, any tranformation running after this will skew
the printout.

-}

type StoreAndLoad = Ds.Set String


emptyStoreAndLoad :: StoreAndLoad                            
emptyStoreAndLoad = Ds.empty 

liveLattice :: H.DataflowLattice StoreAndLoad
liveLattice = H.DataflowLattice
              { H.fact_name = "Stores and Loads"
              , H.fact_bot = emptyStoreAndLoad
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
            where
              j = Ds.union old new 
              ch = H.changeIf (j /= old)

bwdScan :: H.FuelMonad m => H.BwdPass m (Node a) StoreAndLoad
bwdScan = H.BwdPass { H.bp_lattice = liveLattice
                    , H.bp_transfer = H.mkBTransfer bwdTran
                    , H.bp_rewrite = H.noBwdRewrite
                    }
  where
    bwdTran :: (Node a) e x -> H.Fact x StoreAndLoad -> StoreAndLoad
    bwdTran n@(Tnode _ _) f = let bs = H.successors n
                              in foldl (\p l -> p `Ds.union` (fromMaybe emptyStoreAndLoad $ H.lookupFact l f)) 
                                 emptyStoreAndLoad bs
    bwdTran (Lnode _) f = f
    bwdTran (Pnode _ _) f = f
    bwdTran (Mnode _ _) f = f
    bwdTran (Cnode comp _) f = case comp of
      I_store{..} -> Ds.insert (render $ printIr comp) f
      I_load{..} -> Ds.insert (render $ printIr comp) f
      _ -> f
                             
scanDefine :: (CheckpointMonad m, FuelMonad m) => TlDefine a -> m StoreAndLoad
scanDefine (TlDefine fn entry graph) = 
  do { (_, a, b) <- H.analyzeAndRewriteBwd bwdScan (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyStoreAndLoad (H.lookupFact entry a))
     }
  
  
type VisualIds = Dm.Map String Int
                    
allocateVisialIds :: Dm.Map GlobalId StoreAndLoad -> VisualIds
allocateVisialIds mp = 
  let sl = Ds.toList $ Ds.unions (Dm.elems mp)
  in fst $ foldl (\(p,idx) e -> (Dm.insert e idx p, idx+1)) (Dm.empty,0) sl
                    
                    

scanModule :: (CheckpointMonad m, FuelMonad m) => Module a -> m VisualIds
scanModule (Module l) = 
  do { l0 <- mapM (\x -> case x of
                      ToplevelDefine def@(TlDefine fn _ _) ->
                        do { fct <- scanDefine def
                           ; return (Dm.insert (fp_fun_name fn) fct Dm.empty)
                           }
                      _ -> return Dm.empty 
                  ) l
     ; return $ allocateVisialIds (Dm.unions l0)
     }
  

{- rewrite this with mapGraphBlocks and foldBlock -}
rwNodeOO :: Dm.Map String GlobalId -> (Node a) O O -> [(Node a) O O]
rwNodeOO mp node = case node of
  Cnode cinst dbgs -> case Dm.lookup (render $ printIr cinst) mp of
    Nothing -> [node]
    Just x -> [node, Comment $ "visialid: " ++ (render $ printIr x)]
  _ -> [node]


rwBlockCC :: Dm.Map String GlobalId -> H.Block (Node a) C C -> H.Block (Node a) C C
rwBlockCC mp blk = let (f, m, l) = blockSplit blk -- endNodes blk
                       middles = blockToList m
                   in blockJoin f (blockFromList $ concat $ fmap (rwNodeOO mp) middles) l

rwBlockCO :: Dm.Map String GlobalId -> H.Block (Node a) C O -> H.Block (Node a) C O
rwBlockCO mp blk = let (f, m) = blockSplitHead blk -- endNodes blk
                       middles = blockToList m
                   in blockJoinHead f (blockFromList $ concat $ fmap (rwNodeOO mp) middles)

rwBlockOO :: Dm.Map String GlobalId -> H.Block (Node a) O O -> H.Block (Node a) O O
rwBlockOO mp blk = let middles = blockToList blk
                   in blockFromList $ concat $ fmap (rwNodeOO mp) middles

rwBlockOC :: Dm.Map String GlobalId -> H.Block (Node a) O C -> H.Block (Node a) O C
rwBlockOC mp blk = let (m,l) = blockSplitTail blk -- endNodes blk
                       middles = blockToList m
                   in blockJoinTail (blockFromList $ concat $ fmap (rwNodeOO mp) middles) l
                      
rwBlock :: Dm.Map String GlobalId -> H.Block (Node a) e x -> H.Block (Node a) e x                      
rwBlock mp blk = case blk of
  BlockCO _ _ -> rwBlockCO mp blk
  BlockCC _ _ _ -> rwBlockCC mp blk
  BlockOC _ _ -> rwBlockOC mp blk
  BNil -> blk
  BMiddle _ -> rwBlockOO mp blk
  BCat _ _ -> rwBlockOO mp blk
  BSnoc _ _ -> rwBlockOO mp blk
  BCons _ _ -> rwBlockOO mp blk

rwDefine :: Dm.Map String GlobalId -> TlDefine a -> TlDefine a
rwDefine gmp df@(TlDefine fn entry graph) = 
  let graph0 = mapGraphBlocks (rwBlock gmp) graph
  in TlDefine fn entry graph0
  

rwModule :: Maybe (Ds.Set GlobalId) -> Module a -> Dm.Map String GlobalId -> Module a
rwModule mincluded (Module l) duM = 
  let globals = fmap ToplevelGlobal (stringize duM)
  in Module $ globals ++ (fmap (\x -> case x of
                                   ToplevelDefine def@(TlDefine fn _ _) -> 
                                     if (maybe True (Ds.member (fp_fun_name fn)) mincluded)
                                     then ToplevelDefine (rwDefine duM def)
                                     else x
                                   _ -> x
                               ) l)
                          
stringize :: Dm.Map String GlobalId -> [TlGlobal]
stringize mp = 
  let l = Dm.toList mp
  in fmap (\(c,lhs) -> 
            let str = fmap (\x -> if x == '\\' then '_' else x) (replaceDq c)
            in TlGlobalDtype { tlg_lhs = Just lhs
                             , tlg_linkage = Just LinkagePrivate 
                             , tlg_visibility = Nothing
                             , tlg_dllstorage = Nothing
                             , tlg_tls = Nothing
                             , tlg_addrnaming = UnnamedAddr
                             , tlg_addrspace = Nothing
                             , tlg_externallyInitialized = IsNot ExternallyInitialized
                             , tlg_globalType = GlobalType "constant"
                             , tlg_dtype = ucast (Tarray (fromIntegral $ length str) (ucast i8))
                             , tlg_const = Just $ C_str str
                             , tlg_section = Nothing
                             , tlg_comdat = Nothing
                             , tlg_alignment = Just (Alignment 1)
                             }
          ) l
  
visualize :: Maybe String -> Module a -> Module a   
visualize prefix m = 
  let mp = runSimpleUniqueMonad $ runWithFuel H.infiniteFuel
           ((scanModule m)::H.SimpleFuelMonad VisualIds)
  in rwModule Nothing m (Dm.map (\x -> GlobalIdAlphaNum $ ".visual_" ++ show x) mp)
                        
                        