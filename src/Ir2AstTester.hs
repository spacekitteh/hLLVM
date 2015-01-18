module Ir2AstTester (testIr2Ast) where

import Llvm.Syntax.Parser.Basic
import Llvm.Syntax.Parser.Module
import Data.List
import Llvm.Data.Convert (irToAst, IdLabelMap)
import qualified Llvm.Data.Ast as A
import qualified Llvm.Data.Ir as I
import qualified Compiler.Hoopl as H
import System.Environment (getArgs)
import System.Console.CmdArgs


testIr2Ast :: IdLabelMap -> I.Module -> A.Module
testIr2Ast m e = H.runSimpleUniqueMonad $ irToAst m e
