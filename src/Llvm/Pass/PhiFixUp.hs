{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Llvm.Pass.PhiFixUp (phiFixUp) where

import Compiler.Hoopl
import Llvm.Hir.Data
import Control.Monad (liftM)

import Llvm.Pass.Rewriter
#ifdef DEBUG
import Debug.Trace
#endif 

-- | this pass fix up Phi instructions that have the parameters from unreachable code
type LiveLabel = LabelMap Label -- all the searchable labels
lattice :: LiveLabel -> DataflowLattice LiveLabel
lattice live = DataflowLattice { fact_name = "Live Label"
                               , fact_bot = live
                               , fact_join = add
                               }
    where add _ (OldFact old) (NewFact new) = (ch, j)
              where 
                j = new `mapUnion` old
                ch= changeIf (mapSize j > mapSize old)


fwdTransfer :: FwdTransfer (Node g a) LiveLabel
fwdTransfer = mkFTransfer live
  where
    live :: Node g a e x -> LiveLabel -> Fact x LiveLabel
    live (Lnode _) f = f
    live (Pnode _ _) f = f 
    live (Cnode _ _) f = f 
    live t@(Tnode n _) f  = tinstft t n f
    tinstft :: Node g a O C -> Tinst g -> LiveLabel -> Fact C LiveLabel
    tinstft n term f =  
      let targets = successors n -- targetOf term
      in case targets of
        [] -> mapEmpty
        l -> mkFactBase (lattice f) (map (\x -> (x, f)) l)
                                                     

fwdRewrite :: forall g.forall a.forall m. (Eq g, FuelMonad m) => FwdRewrite m (Node g a) LiveLabel
fwdRewrite = mkFRewrite d
   where
     d :: Node g a e x -> LiveLabel -> m (Maybe (Graph (Node g a) e x))
     d (Pnode n dbgs) f = removePhi n dbgs f 
     d _ _ = return Nothing


removePhi :: forall g.forall a. forall m. (Eq g, FuelMonad m) => Pinst g -> [Dbg g] -> Fact O LiveLabel -> m (Maybe (Graph (Node g a) O O))
#ifdef DEBUG
removePhi x _ live | trace ("removePhi is called over " ++ show x) False = undefined
removePhi x _ live | trace ("removePhi is called with " ++ show live) False = undefined
removePhi x _ live | trace ("removePhi is called with " ++ show live) False = undefined
#endif
removePhi (Pinst lt ins lhs) dbgs live = 
  if liveOperands == ins 
  then return $ Nothing
  else if liveOperands == [] 
       then return $ Just emptyGraph
       else return $ Just $ nodeToGraph (Pnode (Pinst lt liveOperands lhs) dbgs)
   where 
#ifdef DEBUG
     isAlive x s | trace ("isAlive is called with " ++ show x ++ "  " ++ show s) False = undefined
     isAlive x s | trace ("isAlive result is " ++ show (hooplLabelOf x `mapMember` s)) False = undefined
#endif                                                                                          
     isAlive x s = x `mapMember` s
     liveOperands = foldl (\p -> \x@(_, li) -> 
                            if isAlive li live then x:p else p) [] (reverse ins)


fwdPass :: forall g.forall a.forall m. (Eq g, FuelMonad m) => LiveLabel -> FwdPass m (Node g a) LiveLabel
fwdPass f = FwdPass { fp_lattice = lattice f
                    , fp_transfer = fwdTransfer
                    , fp_rewrite = fwdRewrite
                    }

phiFixUp :: (CheckpointMonad m, FuelMonad m, Eq g) => LabelMap Label -> Label -> Graph (Node g a) C C -> m (Graph (Node g a) C C)
#ifdef DEBUG
phiFixUp idom entry graph | trace ("phiFixUp with idom " ++ show idom) False = undefined
phiFixUp idom entry graph | trace ("phiFixUp with entry " ++ show entry) False = undefined
#endif
phiFixUp idom entry graph = 
  let idom0 = mapInsert entry entry idom
      fwd = fwdPass idom0
  in do { (graph0, _, _) <- analyzeAndRewriteFwd fwd (JustC [entry]) graph
                            (mapInsert entry idom0 mapEmpty)
        ; return graph0
        }
