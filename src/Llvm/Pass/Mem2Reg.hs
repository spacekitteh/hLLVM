{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Llvm.Pass.Mem2Reg (mem2reg) where

import Compiler.Hoopl
import Llvm.Data.Ir
import qualified Data.Map as Dm
import qualified Data.Set as Ds
import Control.Monad
import Llvm.Pass.Rewriter (rwNode,nodeToGraph)
import Prelude hiding(lookup)
#ifdef DEBUG
import Debug.Trace
#endif


-- | TODO: describe in details what this pass is doing
type Mem2RegFact = Dm.Map LValue (WithTop Value)

data LValue = Mem LocalId
            | Ref LocalId
            deriving (Eq, Show, Ord)

mem2RegLattice :: DataflowLattice Mem2RegFact
mem2RegLattice = DataflowLattice { fact_name = "Mem 2 Reg"
                                 , fact_bot = Dm.empty
                                 , fact_join = joinMaps (extendJoinDomain factAdd)
                                 }
  where
    factAdd _ (OldFact old) (NewFact new)
      = if new == old then (NoChange, PElem new)
        else (SomeChange, Top)

isReg :: FwdTransfer (Node a) Mem2RegFact
isReg = mkFTransfer ft

ft :: (Node a) e x -> Mem2RegFact -> Fact x Mem2RegFact
ft (Lnode _) f = f
ft (Pnode _ _) f = f
ft (Cnode cinst _) f = cinstft cinst f
ft n@(Tnode tinst _) f = tinstft n tinst f


tinstft :: (Node a) O C -> Tinst -> Mem2RegFact -> Fact C Mem2RegFact
tinstft n term f =  
  let targets = successors n -- targetOf term
  in case targets of
    [] -> mapEmpty
    l -> mkFactBase mem2RegLattice
         (map (\x -> (x, f)) l)

cinstft :: Cinst -> Mem2RegFact -> Fact O Mem2RegFact
cinstft = undefined

{-
cinstft (ComputingInstWithDbg (ComputingInst lhs rhs) _) f = cinstft' lhs rhs f


cinstft' :: Maybe GlobalOrLocalId -> Rhs -> Mem2RegFact -> Fact O Mem2RegFact
cinstft' lhs (RmO m) f = memOp lhs m f
cinstft' lhs (Re (Ev tv)) f = maybe f (\a -> let TypedData _ v = tv in
                                        case a of
                                          GolG _ -> f
                                          GolL s -> Dm.insert (Ref $ localIdToLstring s) (PElem v) f) lhs
cinstft' _ (Re _) f = f
cinstft' _ _ f = f


memOp :: Maybe GlobalOrLocalId -> MemOp -> Mem2RegFact -> Fact O Mem2RegFact
memOp (Just (GolL lhs)) (Allocate _ _ Nothing _) f = insert (Mem $ localIdToLstring lhs) Top f
memOp _ (Store _ (TypedData _ v1) (TypedData _ (Pointer (VgOl (GolL ptr)))) _ _) f =
    let x = Mem $ localIdToLstring ptr
    in if (x `Dm.member` f) then insert x (PElem v1) f
       else f
memOp _ (StoreAtomic _ _ (TypedData _ v1) (TypedData _ (Pointer (VgOl (GolL ptr)))) _) f =
    let x = Mem $ localIdToLstring ptr
    in if (x `Dm.member` f) then insert x (PElem v1) f
       else f
memOp _ _ f = f
-}

insert :: Ord k => k -> v -> Dm.Map k v -> Dm.Map k v
#ifdef DEBUG
insert x v1 f | trace ("insert " ++ (show x) ++ "->" ++ (show v1)) False = undefined
#endif
insert x v1 f = Dm.insert x v1 f

badAss :: Monad m => (Value -> Maybe Value) -> Node a e x -> m (Maybe (Node a e x))
badAss f node = return (rwNode f node)


mem2Reg :: forall a.forall m . FuelMonad m => FwdRewrite m (Node a) Mem2RegFact
mem2Reg = mkFRewrite cp
    where
      -- each node is rewritten to a one node graph.
      cp :: FuelMonad m => Node a e x -> Mem2RegFact -> m (Maybe (Graph (Node a) e x))
      cp node f = do { x <- badAss (lookup f) node
                     ; return $ liftM {-Maybe-} nodeToGraph x
                     }

      lookup :: Mem2RegFact -> Value -> Maybe Value
      lookup f x = do { x' <- case x of
                           Val_ssa s -> Just $ Ref s
                           -- Deref (Pointer (VgOl (GolL s))) -> Just $ Mem $ localIdToLstring s
                           _ -> Nothing
                       ;  case Dm.lookup x' f of
                            Just (PElem v) -> Just v
                            _ -> Nothing
                       }




mem2RegPass :: forall a. forall m. FuelMonad m => FwdPass m (Node a) Mem2RegFact
mem2RegPass = FwdPass { fp_lattice = mem2RegLattice
                      , fp_transfer = isReg
                      , fp_rewrite = mem2Reg
                      }


mem2reg :: (CheckpointMonad m, FuelMonad m) => Ds.Set (Dtype, GlobalId) -> Label -> Graph (Node a) C C -> m (Graph (Node a) C C)
mem2reg _ entry graph =
  do { (graph', _, _) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph
                         (mapSingleton entry (Dm.empty)) -- initFact gs))
     ; return graph'
     }
  where fwd = mem2RegPass -- debugFwdJoins trace (const True) mem2RegPas
