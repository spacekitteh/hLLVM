module Ast2IrTester (testAst2Ir) where

import Llvm.AsmParser.Basic
import Llvm.AsmParser.Module
import Data.List
import Llvm.VmCore.Convert (astToIr) --Ast2Ir
-- import Llvm.VmCore.Ir2Ast
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
import Llvm.VmCore.LabelMapM
import qualified Compiler.Hoopl as H


testAst2Ir :: A.Module -> (IdLabelMap, I.Module)
testAst2Ir e = H.runSimpleUniqueMonad $ astToIr e
