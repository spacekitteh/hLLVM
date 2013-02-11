{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Llvm.Pass.Mem2Reg (mem2reg) where

import Compiler.Hoopl
import Llvm.VmCore.CoreIr 
import Llvm.VmCore.Ir
import qualified Data.Map as Dm
import qualified Data.Set as Ds
import Control.Monad
import Llvm.Pass.Rewriter
import Prelude hiding(lookup)
#ifdef DEBUG
import Debug.Trace
#endif
 
type Mem2RegFact = Dm.Map LValue (WithTop Value)

data LValue = Mem Lstring
          | Ref Lstring
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


{-
initFact :: Ds.Set (Type, GlobalId) -> Mem2RegFact
initFact vars = Dm.empty 
-}


isReg :: FwdTransfer Node Mem2RegFact
isReg = mkFTransfer ft

ft :: Node e x -> Mem2RegFact -> Fact x Mem2RegFact
ft (Nlabel _) f = f
ft (Pinst _) f = f
ft (Cinst cinst) f = cinstft cinst f
ft (Tinst tinst) f = tinstft tinst f


tinstft :: TerminatorInstWithDbg -> Mem2RegFact -> Fact C Mem2RegFact
tinstft (TerminatorInstWithDbg term _) f =  let targets = targetOf term
                                            in case targets of
                                                [] -> mapEmpty
                                                l -> mkFactBase mem2RegLattice 
                                                     (map (\x -> (getTargetLabel x, f)) l)

cinstft :: ComputingInstWithDbg -> Mem2RegFact -> Fact O Mem2RegFact
cinstft (ComputingInstWithDbg (ComputingInst lhs rhs) _) f = cinstft' lhs rhs f


cinstft' :: Maybe GlobalOrLocalId -> Rhs -> Mem2RegFact -> Fact O Mem2RegFact
cinstft' lhs (RmO m) f = memOp lhs m f
cinstft' lhs (Re (Ev tv)) f = maybe f (\a -> let TypedValue _ v = tv in 
                                        case a of
                                          GolG _ -> f
                                          GolL s -> Dm.insert (Ref $ localIdToLstring s) (PElem v) f) lhs
cinstft' _ (Re _) f = f
cinstft' _ _ f = f 

  
memOp :: Maybe GlobalOrLocalId -> MemOp -> Mem2RegFact -> Fact O Mem2RegFact  
memOp (Just (GolL lhs)) (Allocate OnStack _ Nothing _) f = insert (Mem $ localIdToLstring lhs) Top f
memOp _ (Store _ (TypedValue _ v1) (TypedPointer _ (Pointer (VgOl (GolL ptr)))) _) f = 
    let x = Mem $ localIdToLstring ptr
    in
    if (x `Dm.member` f) then 
        insert x (PElem v1) f
--        Dm.insert x (PElem v1) f
    else
        f
memOp _ _ f = f


insert :: Ord k => k -> v -> Dm.Map k v -> Dm.Map k v
#ifdef DEBUG
insert x v1 f | trace ("insert " ++ (show x) ++ "->" ++ (show v1)) False = undefined
#endif
insert x v1 f = Dm.insert x v1 f


                                                 

mem2Reg :: forall m . FuelMonad m => FwdRewrite m Node Mem2RegFact
mem2Reg = mkFRewrite cp
    where
      cp :: Node e x -> Mem2RegFact -> m (Maybe (Graph Node e x))
      cp node f = return $ liftM nodeToG $ rwNode (lookup f) node

      lookup :: Mem2RegFact -> Value -> Maybe Value
      lookup f x = do { x' <- case x of
                           VgOl (GolL s) -> Just $ Ref $ localIdToLstring s   
                           Deref (Pointer (VgOl (GolL s))) -> Just $ Mem $ localIdToLstring s
                           _ -> Nothing 
                       ;  case Dm.lookup x' f of
                            Just (PElem v) -> Just v
                            _ -> Nothing
                       }




mem2RegPass :: forall m. FuelMonad m => FwdPass m Node Mem2RegFact
mem2RegPass = FwdPass { fp_lattice = mem2RegLattice
                      , fp_transfer = isReg
                      , fp_rewrite = mem2Reg
                      }
              
              
mem2reg :: Ds.Set (Type, GlobalId) -> Label -> Graph Node C C -> M (Graph Node C C)
mem2reg _ entry graph = 
  do { (graph', _, _) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph
                         (mapSingleton entry (Dm.empty)) -- initFact gs))
     ; return graph'
     }
  where fwd = mem2RegPass -- debugFwdJoins trace (const True) mem2RegPass