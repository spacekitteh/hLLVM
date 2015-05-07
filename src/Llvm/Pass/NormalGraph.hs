module Llvm.Pass.NormalGraph where
import Compiler.Hoopl (Graph,C,analyzeAndRewriteFwd,MaybeC(..),mapInsert,mapEmpty,LabelMap,CheckpointMonad, FuelMonad) 
import Llvm.Hir.Data
import Llvm.Pass.PhiFixUp
import Llvm.Pass.Dominator

idom :: (CheckpointMonad m, FuelMonad m) => Label -> Graph (Node a) C C -> m (LabelMap Label)
idom entry g = do { (_,fact,_) <- analyzeAndRewriteFwd domPass (JustC [entry]) g
                                   (mapInsert entry domEntry mapEmpty)
                  ; return $ immediateDominators fact
                  }

fixUpPhi :: (CheckpointMonad m, FuelMonad m) => () -> Label -> Graph (Node a) C C -> m (Graph (Node a) C C)
fixUpPhi _ entry g = do { labelMap <- idom entry g
                        ; g' <- phiFixUp labelMap entry g
                        ; return g'
                        }
