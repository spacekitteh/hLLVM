module Llvm.Pass.NormalGraph where
import Compiler.Hoopl (Graph,C,analyzeAndRewriteFwd,MaybeC(..),mapInsert,mapEmpty,LabelMap,CheckingFuelMonad,SimpleUniqueMonad)
import Llvm.VmCore.Ir
import Llvm.Pass.PhiElimination
import Llvm.Pass.Dominator

idom :: Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (LabelMap Label)
idom entry g = do { (_,fact,_) <- analyzeAndRewriteFwd domPass (JustC [entry]) g
                                   (mapInsert entry domEntry mapEmpty)
                  ; return $ immediateDominators fact
                  }

killphi :: () -> Label -> Graph Node C C -> CheckingFuelMonad SimpleUniqueMonad (Graph Node C C)
killphi _ entry g = do { labelMap <- idom entry g
                       ; g' <- phiElimination labelMap entry g
                       ; return g'
                       }
