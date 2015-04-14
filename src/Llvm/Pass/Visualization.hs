{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards #-}

module Llvm.Pass.Visualization where
import Data.Maybe
import qualified Data.Set as Ds
import qualified Data.Map as Dm
import qualified Data.List as L
import Llvm.Query.IrCxt

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Data.Ir
import Llvm.Syntax.Printer.IrPrint

data Live = Live { stores :: Dm.Map CInst String
                 , loads :: Dm.Map CInst String
                 } deriving (Eq, Ord, Show)


emptyLive :: Live                            
emptyLive = Live Dm.empty Dm.empty

instance (IrPrint t1, IrPrint t2, IrPrint t3) => IrPrint (t1, t2, t3) where
  printIr (t1, t2, t3) = parens (printIr t1 <+> printIr t2 <+> printIr t3)

instance IrPrint Live where
  printIr (Live p1 a1) = 
    text "ptrParamStore:" <+> (text $ show p1)
    $+$ text "ptrParamStoreAlloca:" <+> (text $ show a1)


liveLattice :: H.DataflowLattice Live
liveLattice = H.DataflowLattice
              { H.fact_name = "Live variables"
              , H.fact_bot = emptyLive
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
            where
              j = unionLive old new 
              ch = H.changeIf (j /= old)

unionLive :: Live -> Live -> Live
unionLive (Live s1 s2) (Live t1 t2) = Live (s1 `Dm.union` t1) (s2 `Dm.union` t2)


bwdScan :: forall m. H.FuelMonad m => Ds.Set LocalId -> H.BwdPass m Node Live
bwdScan formalParams = H.BwdPass { H.bp_lattice = liveLattice
                                 , H.bp_transfer = H.mkBTransfer (bwdTran formalParams)
                                 , H.bp_rewrite = H.noBwdRewrite
                                 }
  where
    bwdTran :: Ds.Set LocalId -> Node e x -> H.Fact x Live -> Live
    bwdTran _ n@(Tinst _) f = let bs = H.successors n
                              in foldl (\p l -> p `unionLive` (fromMaybe emptyLive $ H.lookupFact l f)) emptyLive bs
    bwdTran _ (Nlabel _) f = f
    bwdTran _ (Pinst _) f = f
    bwdTran fp (Cinst (CInstWithDbg comp _)) f@Live{..} = case comp of
      I_store{..} -> f { stores = Dm.insert comp (render $ printIr comp) stores }
      I_load{..} -> f { loads = Dm.insert comp (render $ printIr comp) loads }
      _ -> f
    getValuesFromParams :: [ActualParam] -> Ds.Set Value
    getValuesFromParams ls = foldl (\p e -> case e of
                                       ActualParamData _ _ _ v _ -> Ds.insert v p
                                       _ -> p
                                   ) Ds.empty ls
                             
bubbleUp :: Ord x => x -> x -> Ds.Set x -> Ds.Set x
bubbleUp dest src s = if Ds.member dest s then Ds.insert src s else s
                                 
                             
typesOfActualParams :: [ActualParam] -> [Dtype]
typesOfActualParams aps = let b = all (\x -> case x of
                                          ActualParamData _ _ _ _ _ -> True
                                          _ -> False
                                      ) aps 
                              ds = fmap (\(ActualParamData d _ _ _ _) -> d) aps
                          in if b then ds
                             else []

returnType :: CallSiteType -> Rtype
returnType ct = case ct of
  CallSiteRet rt -> rt
  CallSiteFun (Tfunction rt _ _) _ -> rt                             
  

preScanGraph :: (H.CheckpointMonad m, H.FuelMonad m) => Ds.Set LocalId -> Label 
                -> H.Graph Node H.C H.C -> m Live
preScanGraph fm entry graph =
  do { (_, a, b) <- H.analyzeAndRewriteBwd (bwdScan fm) (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyLive (H.lookupFact entry a))
     }

preScanDefine :: (CheckpointMonad m, FuelMonad m) => IrCxt -> TlDefine -> m Live
preScanDefine s (TlDefine fn entry graph) = preScanGraph formalParamIds entry graph
  where formalParamIds :: Ds.Set LocalId
        formalParamIds = let (FormalParamList l _ _) = fp_param_list fn
                         in foldl (\p x -> case x of
                                      FormalParamData (DtypeScalarP _)  _ _ (FexplicitParam v) _ -> Ds.insert v p
                                      _ -> p
                                  ) Ds.empty l
  
preScan :: (H.CheckpointMonad m, H.FuelMonad m) => Module -> IrCxt -> m (Dm.Map FunctionPrototype Live)
preScan (Module l) ic = 
  do { l0 <- mapM (\x -> case x of
                      ToplevelDefine def@(TlDefine fn _ _) ->
                        do { fct <- preScanDefine ic def
                           ; return (Dm.insert fn fct Dm.empty)
                           }
                      _ -> return Dm.empty 
                  ) l
     ; return $ Dm.unions l0
     }
  
  
rwLattice :: H.DataflowLattice ()
rwLattice = H.DataflowLattice
              { H.fact_name = "RW"
              , H.fact_bot = ()
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (H.changeIf False, new)

  

rwNode :: FuelMonad m => Node e x -> () -> m (Maybe (Graph Node e x))
rwNode node f = undefined