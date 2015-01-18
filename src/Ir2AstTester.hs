module Ir2AstTester (testIr2Ast) where

import Llvm.AsmParser.Basic
import Llvm.AsmParser.Module
import Data.List
import Llvm.VmCore.Convert (irToAst)
-- import Llvm.VmCore.Ir2Ast
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
import qualified Compiler.Hoopl as H
import System.Environment (getArgs)
import System.Console.CmdArgs
import Llvm.VmCore.LabelMapM


testIr2Ast :: IdLabelMap -> I.Module -> A.Module
testIr2Ast m e = H.runSimpleUniqueMonad $ irToAst m e
