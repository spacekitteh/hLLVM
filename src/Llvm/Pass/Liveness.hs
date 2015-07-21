{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Llvm.Pass.Liveness (dce) where
import Data.Maybe
import qualified Data.Set as Ds
import qualified Data.List as L

import qualified Compiler.Hoopl as H
import Llvm.Hir.Data
import Llvm.Query.Uda
import Llvm.Hir.Print

type Live = Ds.Set LocalId
liveLattice :: H.DataflowLattice Live
liveLattice = H.DataflowLattice
              { H.fact_name = "Live variables"
              , H.fact_bot = Ds.empty
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
              where
                j = new `Ds.union` old
                ch= H.changeIf (Ds.size j > Ds.size old)

liveness :: H.BwdTransfer (Node g a) Live
liveness = H.mkBTransfer live
  where
    live = undefined
    {-
    live :: Node e x -> H.Fact x Live -> Live
    live (Lnode _) f = f
    live (Pnode n _) f = f `Ds.union` (filterOutGlobalId $ u1ofPinstWithDbg n) `Ds.difference` (filterOutGlobalId $ d1ofPinstWithDbg n)
    -- | FIXME
    -- | this is a very simplistic implementation and it does not consider function calls might have side effects.
    -- | we need to distinguish the uses of a possible side effect computation from the uses of a pure computation.
    live (Cnode n _) f = f `Ds.union` (filterOutGlobalId $ u1ofComputingInstWithDbg n) `Ds.difference` (filterOutGlobalId $ d1ofComputingInstWithDbg n)
    live x@(Tinst n) f = let bs = H.successors x
                             f' = foldl (\p -> \l -> p `Ds.union` (fact f l)) Ds.empty bs
                         in f' `Ds.union` (filterOutGlobalId $ u1ofTerminatorInstWithDbg n) `Ds.difference` (filterOutGlobalId $ d1ofTerminatorInstWithDbg n)
--    fact :: FactBase (Ds.Set LocalId) -> Label -> Live
    fact f l = fromMaybe Ds.empty $ H.lookupFact l f
 -}


deadAsstElim :: forall g.forall a. forall m. H.FuelMonad m => H.BwdRewrite m (Node g a) Live
deadAsstElim = H.mkBRewrite d
   where
     d :: Node g a e x -> H.Fact x Live -> m (Maybe (H.Graph (Node g a) e x))
     d (Cnode n _) live = dead n live
     d _ _ = return Nothing


dead :: forall g.forall a.forall m. H.FuelMonad m => Cinst g -> H.Fact H.O Live -> m (Maybe (H.Graph (Node g a) H.O H.O))
dead ci live = deadCi ci live


deadCi :: forall g.forall a.forall m. H.FuelMonad m => Cinst g -> H.Fact H.O Live -> m (Maybe (H.Graph (Node g a) H.O H.O))
deadCi = undefined
{-
deadCi (ComputingInst Nothing (RmO m)) live = deadRmo m live
deadCi (ComputingInst Nothing (Call _ cs)) live = deadCallSite cs live
deadCi (ComputingInst _ (LandingPad _ _ _ _ _)) _ = return Nothing
deadCi (ComputingInst lhsOpt _) live = case lhsOpt of
                                         Just (GolL x) | not (x `Ds.member` live) -> return $ Just H.emptyGraph
                                         _  -> return Nothing


deadRmo :: forall m. H.FuelMonad m => MemOp -> H.Fact H.O Live -> m (Maybe (H.Graph Node H.O H.O))
deadRmo inst live = case inst of
  (Store _ _ (TypedData _ (Pointer v)) _ _) -> case localIdOfValue v of
    Just x | not (x `Ds.member` live) -> return $ Just H.emptyGraph
    _ -> return Nothing
  (StoreAtomic _ _ _ (TypedData _ (Pointer v)) _) -> case localIdOfValue v of
    Just x | not (x `Ds.member` live) -> return $ Just H.emptyGraph
    _ -> return Nothing
  _ -> return Nothing
  where localIdOfValue (VgOl (GolL x)) = Just x
        localIdOfValue _ = Nothing
-}

isDeclare :: IrPrint g => FunPtr g -> Bool
isDeclare (FunId gl) | (render $ printIr gl) == "@llvm.dbg.declare" = True
isDeclare _ = False



isDeadAP :: H.Fact H.O Live -> FunOperand a -> Bool
isDeadAP = undefined

{-
isDeadAP live ap  = let u = filterOutGlobalId $ u1 $ uDofActualParam ap
                        dif = u `Ds.intersection` live
                    in dif == Ds.empty
-}

{-
deadCallSite :: forall a.forall m. H.FuelMonad m => CallSite -> H.Fact H.O Live -> m (Maybe (H.Graph (Node a) H.O H.O))
deadCallSite (CsFun _ _ _ fn ap _) live | isDeclare fn =
  if L.all (isDeadAP live) ap then return $ Just H.emptyGraph
  else return Nothing
deadCallSite _ _ = return Nothing
-}



filterOutGlobalId :: Ds.Set GlobalOrLocalId -> Ds.Set LocalId
filterOutGlobalId s = Ds.foldl (\a b -> case b of
                                   GolL l -> Ds.insert l a
                                   _ -> a
                               ) Ds.empty s





dcePass :: forall a. forall g.forall m. H.FuelMonad m => H.BwdPass m (Node g a) Live
dcePass = H.BwdPass { H.bp_lattice = liveLattice
                    , H.bp_transfer = liveness
                    , H.bp_rewrite = deadAsstElim
                    }

dce :: (H.CheckpointMonad m, H.FuelMonad m) => Ds.Set (Dtype, g) -> Label -> H.Graph (Node g a) H.C H.C -> m (H.Graph (Node g a) H.C H.C)
dce _ entry graph =
  do { (graph', _, _) <- H.analyzeAndRewriteBwd bwd (H.JustC [entry]) graph
                         H.mapEmpty
     ; return graph'
     }
  where bwd = dcePass
