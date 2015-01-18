module Llvm.Pass.NormalGraph where
import Compiler.Hoopl (Graph,C,analyzeAndRewriteFwd,MaybeC(..),mapInsert,mapEmpty,LabelMap,CheckingFuelMonad,SimpleUniqueMonad)
import Llvm.Data.Ir
import Llvm.Pass.PhiFixUp
import Llvm.Pass.Dominator

idom :: Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (LabelMap Label)
idom entry g = do { (_,fact,_) <- analyzeAndRewriteFwd domPass (JustC [entry]) g
                                   (mapInsert entry domEntry mapEmpty)
                  ; return $ immediateDominators fact
                  }

fixUpPhi :: () -> Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (Graph Node C C)
fixUpPhi _ entry g = do { labelMap <- idom entry g
                        ; g' <- phiFixUp labelMap entry g
                        ; return g'
                        }
