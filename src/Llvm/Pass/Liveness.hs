{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Llvm.Pass.Liveness (dce) where
import Data.Maybe
import qualified Data.Set as Ds
import qualified Data.List as L

import Compiler.Hoopl
import Llvm.VmCore.CoreIr
import Llvm.VmCore.Ir
import Llvm.Pass.Uda
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.CoreIrWriter()

type Live = Ds.Set LocalId
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
              { fact_name = "Live variables"
              , fact_bot = Ds.empty
              , fact_join = add
              }
    where add _ (OldFact old) (NewFact new) = (ch, j)
              where 
                j = new `Ds.union` old
                ch= changeIf (Ds.size j > Ds.size old)

liveness :: BwdTransfer Node Live
liveness = mkBTransfer live
  where
    live :: Node e x -> Fact x Live -> Live
    live (Nlabel _) f = f
    live (Pinst n) f = f `Ds.union` (filterOutGlobalId $ u1ofPinst n) `Ds.difference` (filterOutGlobalId $ d1ofPinst n)
    live (Cinst n) f = f `Ds.union` (filterOutGlobalId $ u1ofComputingInstWithDbg n) `Ds.difference` (filterOutGlobalId $ d1ofComputingInstWithDbg n)
    live x@(Tinst n) f = let bs = successors x
                             f' = foldl (\p -> \l -> p `Ds.union` (fact f l)) Ds.empty bs
                       in f' `Ds.union` (filterOutGlobalId $ u1ofTerminatorInstWithDbg n) `Ds.difference` (filterOutGlobalId $ d1ofTerminatorInstWithDbg n)
--    fact :: FactBase (Ds.Set LocalId) -> Label -> Live
    fact f l = fromMaybe Ds.empty $ lookupFact l f


deadAsstElim :: forall m. FuelMonad m => BwdRewrite m Node Live
deadAsstElim = mkBRewrite d
   where
     d :: Node e x -> Fact x Live -> m (Maybe (Graph Node e x))
     d (Cinst n) live = dead n live
     d _ _ = return Nothing


dead :: forall m. FuelMonad m => ComputingInstWithDbg -> Fact O Live -> m (Maybe (Graph Node O O))
dead (ComputingInstWithDbg ci _) live = deadCi ci live


deadCi :: forall m. FuelMonad m => ComputingInst -> Fact O Live -> m (Maybe (Graph Node O O))
deadCi (ComputingInst Nothing (RmO m)) live = deadRmo m live
deadCi (ComputingInst Nothing (Call _ cs)) live = deadCallSite cs live
deadCi (ComputingInst _ (LandingPad _ _ _ _ _)) _ = return Nothing
deadCi (ComputingInst lhsOpt _) live = case lhsOpt of
                                         Just (GolL x) | not (x `Ds.member` live) -> return $ Just emptyGraph
                                         _  -> return Nothing


deadRmo :: forall m. FuelMonad m => MemOp -> Fact O Live -> m (Maybe (Graph Node O O))
deadRmo (Store _ _ (TypedPointer _ (Pointer v)) _) live = case localIdOfValue v of
                                                            Just x | not (x `Ds.member` live) -> return $ Just emptyGraph
                                                            _ -> return Nothing
                                                  where localIdOfValue (VgOl (GolL x)) = Just x
                                                        localIdOfValue _ = Nothing
deadRmo _ _ = return Nothing


isDeclare :: FunName -> Bool
isDeclare (FunNameGlobal (GolG gl)) | toLlvm gl == "@llvm.dbg.declare" = True
isDeclare _ = False


isDeadAP :: Fact O Live -> ActualParam -> Bool
isDeadAP live ap  = let u = filterOutGlobalId $ u1 $ uDofActualParam ap
                        dif = u `Ds.intersection` live
                    in dif == Ds.empty

deadCallSite :: forall m. FuelMonad m => CallSite -> Fact O Live -> m (Maybe (Graph Node O O))
deadCallSite (CallFun _ _ _ fn ap _) live | isDeclare fn = if L.all (isDeadAP live) ap then
                                                               return $ Just emptyGraph
                                                           else
                                                               return Nothing
deadCallSite _ _ = return Nothing



filterOutGlobalId :: Ds.Set GlobalOrLocalId -> Ds.Set LocalId
filterOutGlobalId s = Ds.foldl (\a b -> case b of
                                         GolL l -> Ds.insert l a
                                         _ -> a) Ds.empty s
                      
                      
                      


dcePass :: forall m. FuelMonad m => BwdPass m Node Live
dcePass = BwdPass { bp_lattice = liveLattice
                  , bp_transfer = liveness
                  , bp_rewrite = deadAsstElim
                  }
                      
          
          
dce :: Ds.Set (Type, GlobalId) -> Label -> Graph Node C C -> M (Graph Node C C)
dce _ entry graph = 
  do { (graph', _, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph
                             mapEmpty       
     ; return graph'
     }
  where bwd = dcePass
