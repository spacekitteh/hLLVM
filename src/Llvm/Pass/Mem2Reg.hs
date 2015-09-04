{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Llvm.Pass.Mem2Reg (mem2reg) where

import Compiler.Hoopl
import Llvm.Hir.Data
import qualified Data.Map as Dm
import qualified Data.Set as Ds
import Control.Monad
import Llvm.Pass.Rewriter (rwNode,nodeToGraph)
import Prelude hiding(lookup)
#ifdef DEBUG
import Debug.Trace
#endif


-- | TODO: describe in details what this pass is doing
type Mem2RegFact g = Dm.Map LValue (WithTop (Value g))

data LValue = Mem Lname
            | Ref Lname
            deriving (Eq, Show, Ord)

mem2RegLattice :: Eq g => DataflowLattice (Mem2RegFact g)
mem2RegLattice = DataflowLattice { fact_name = "Mem 2 Reg"
                                 , fact_bot = Dm.empty
                                 , fact_join = joinMaps (extendJoinDomain factAdd)
                                 }
  where
    factAdd _ (OldFact old) (NewFact new)
      = if new == old then (NoChange, PElem new)
        else (SomeChange, Top)

isReg :: Eq g => FwdTransfer (Node g a) (Mem2RegFact g)
isReg = mkFTransfer ft

ft :: Eq g => (Node g a) e x -> Mem2RegFact g -> Fact x (Mem2RegFact g)
ft (Lnode _) f = f
ft (Pnode _ _) f = f
ft (Cnode cinst _) f = cinstft cinst f
ft n@(Tnode tinst _) f = tinstft n tinst f


tinstft :: Eq g => (Node g a) O C -> Tinst g -> Mem2RegFact g -> Fact C (Mem2RegFact g)
tinstft n term f =  
  let targets = successors n -- targetOf term
  in case targets of
    [] -> mapEmpty
    l -> mkFactBase mem2RegLattice
         (map (\x -> (x, f)) l)

cinstft :: Cinst g -> Mem2RegFact g -> Fact O (Mem2RegFact g)
cinstft = undefined

{-
cinstft (ComputingInstWithDbg (ComputingInst lhs rhs) _) f = cinstft' lhs rhs f


cinstft' :: Maybe GlobalOrLname -> Rhs -> Mem2RegFact -> Fact O Mem2RegFact
cinstft' lhs (RmO m) f = memOp lhs m f
cinstft' lhs (Re (Ev tv)) f = maybe f (\a -> let TypedData _ v = tv in
                                        case a of
                                          GolG _ -> f
                                          GolL s -> Dm.insert (Ref $ localIdToLstring s) (PElem v) f) lhs
cinstft' _ (Re _) f = f
cinstft' _ _ f = f


memOp :: Maybe GlobalOrLname -> MemOp -> Mem2RegFact -> Fact O Mem2RegFact
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

badAss :: Monad m => (Value g -> Maybe (Value g)) -> Node g a e x -> m (Maybe (Node g a e x))
badAss f node = return (rwNode f node)


mem2Reg :: forall g.forall a.forall m . FuelMonad m => FwdRewrite m (Node g a) (Mem2RegFact g)
mem2Reg = mkFRewrite cp
    where
      -- each node is rewritten to a one node graph.
      cp :: FuelMonad m => Node g a e x -> Mem2RegFact g -> m (Maybe (Graph (Node g a) e x))
      cp node f = do { x <- badAss (lookup f) node
                     ; return $ liftM {-Maybe-} nodeToGraph x
                     }

      lookup :: Mem2RegFact g -> Value g -> Maybe (Value g)
      lookup f x = do { x' <- case x of
                           Val_ssa s -> Just $ Ref s
                           -- Deref (Pointer (VgOl (GolL s))) -> Just $ Mem $ localIdToLstring s
                           _ -> Nothing
                       ;  case Dm.lookup x' f of
                            Just (PElem v) -> Just v
                            _ -> Nothing
                       }




mem2RegPass :: forall g.forall a. forall m. (Eq g, FuelMonad m) => FwdPass m (Node g a) (Mem2RegFact g)
mem2RegPass = FwdPass { fp_lattice = mem2RegLattice
                      , fp_transfer = isReg
                      , fp_rewrite = mem2Reg
                      }


mem2reg :: (CheckpointMonad m, FuelMonad m, Eq g) => Ds.Set (Dtype, g) -> Label -> Graph (Node g a) C C -> m (Graph (Node g a) C C)
mem2reg _ entry graph =
  do { (graph', _, _) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph
                         (mapSingleton entry (Dm.empty)) -- initFact gs))
     ; return graph'
     }
  where fwd = mem2RegPass -- debugFwdJoins trace (const True) mem2RegPas
