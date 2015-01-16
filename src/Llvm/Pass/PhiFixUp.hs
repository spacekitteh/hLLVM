{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Llvm.Pass.PhiFixUp (phiFixUp) where

import Compiler.Hoopl
import Llvm.VmCore.CoreIr
import Llvm.VmCore.Ir

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


fwdTransfer :: FwdTransfer Node LiveLabel
fwdTransfer = mkFTransfer live
  where
    live :: Node e x -> LiveLabel -> Fact x LiveLabel
    live (Nlabel _) f = f
    live (Pinst _) f = f 
    live (Cinst _) f = f 
    live (Tinst n) f  = tinstft n f
    tinstft :: TerminatorInstWithDbg -> LiveLabel -> Fact C LiveLabel
    tinstft (TerminatorInstWithDbg term _) f =  
      let targets = targetOf term
      in case targets of
        [] -> mapEmpty
        l -> mkFactBase (lattice f) (map (\x -> (getTargetLabel x, f)) l)
                                                     

fwdRewrite :: forall m. FuelMonad m => FwdRewrite m Node LiveLabel
fwdRewrite = mkFRewrite d
   where
     d :: Node e x -> LiveLabel -> m (Maybe (Graph Node e x))
     d (Pinst n) f = removePhi n f 
     d _ _ = return Nothing


removePhi :: forall m. FuelMonad m => PhiInst -> Fact O LiveLabel -> m (Maybe (Graph Node O O))
#ifdef DEBUG
removePhi x live | trace ("removePhi is called over " ++ show x) False = undefined
removePhi x live | trace ("removePhi is called with " ++ show live) False = undefined
removePhi x live | trace ("removePhi is called with " ++ show live) False = undefined
#endif
removePhi (PhiInst lhs t ins) live = 
  if liveOperands == ins then
    return $ Nothing
  else if liveOperands == [] then
         return $ Just emptyGraph
       else 
         return $ Just $ nodeToGraph $ Pinst $ PhiInst lhs t liveOperands
   where 
#ifdef DEBUG
     isAlive x s | trace ("isAlive is called with " ++ show x ++ "  " ++ show s) False = undefined
     isAlive x s | trace ("isAlive result is " ++ show (hooplLabelOf x `mapMember` s)) False = undefined
#endif                                                                                          
     isAlive x s = (hooplLabelOf x) `mapMember` s
     liveOperands = foldl (\p -> \x@(_, PercentLabel li) -> 
                            if isAlive li live then x:p else p) [] (reverse ins)


fwdPass :: forall m. FuelMonad m => LiveLabel -> FwdPass m Node LiveLabel
fwdPass f = FwdPass { fp_lattice = lattice f
                    , fp_transfer = fwdTransfer
                    , fp_rewrite = fwdRewrite
                    }

phiFixUp :: LabelMap Label -> Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (Graph Node C C)
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