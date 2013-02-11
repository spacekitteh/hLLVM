{-# LANGUAGE DeriveDataTypeable #-}
module Ast2IrTester (testAst2Ir) where

import Llvm.AsmParser.Basic
import Llvm.AsmParser.Module
import Llvm.VmCore.AsmWriter
import Data.List
import Llvm.VmCore.Ast2Ir
import Llvm.VmCore.Ir2Ast
import Llvm.VmCore.IrWriter
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
import Llvm.VmCore.LabelMap
import qualified Compiler.Hoopl as H


testAst2Ir :: A.Module -> (IdLabelMap, I.Module)
testAst2Ir e = H.runSimpleUniqueMonad $ H.runWithFuel 100 (astToIr e)
