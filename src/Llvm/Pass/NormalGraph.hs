{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Llvm.Pass.NormalGraph where
import Compiler.Hoopl
import Llvm.VmCore.Ir
import Llvm.VmCore.CoreIrWriter()
import Llvm.Pass.PhiElimination
import Llvm.Pass.Dominator

idom :: Label -> Graph Node C C -> M (LabelMap Label)
idom entry g = do { (_,fact,_) <- analyzeAndRewriteFwd domPass (JustC [entry]) g
                                   (mapInsert entry domEntry mapEmpty)
                  ; return $ immediateDominators fact
                  }


killphi :: () -> Label -> Graph Node C C -> M (Graph Node C C)
killphi _ entry g = do { labelMap <- idom entry g
                       ; g' <- phiElimination labelMap entry g
                       ; return g'
                       }
