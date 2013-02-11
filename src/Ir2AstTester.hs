{-# LANGUAGE DeriveDataTypeable #-}
module Ir2AstTester (testIr2Ast) where

import Llvm.AsmParser.Basic
import Llvm.AsmParser.Module
import Llvm.VmCore.AsmWriter
import Data.List
import Llvm.VmCore.Ast2Ir
import Llvm.VmCore.Ir2Ast
import Llvm.VmCore.IrWriter
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
import Llvm.VmCore.AstCanonicalization
import qualified Compiler.Hoopl as H
import System.Environment (getArgs)
import System.Console.CmdArgs
import Llvm.VmCore.LabelMap


testIr2Ast :: IdLabelMap -> I.Module -> A.Module
testIr2Ast m e = H.runSimpleUniqueMonad $ H.runWithFuel 100 (irToAst m e)
