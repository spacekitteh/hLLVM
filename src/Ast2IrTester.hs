module Ast2IrTester (testAst2Ir) where

import Llvm.Syntax.Parser.Basic
import Llvm.Syntax.Parser.Module
import Data.List
import Llvm.Data.Convert (astToIr) 
import qualified Llvm.Data.Ast as A
import qualified Llvm.Data.Ir as I
import Llvm.Data.LabelMapM
import qualified Compiler.Hoopl as H


testAst2Ir :: A.Module -> (IdLabelMap, I.Module)
testAst2Ir e = H.runSimpleUniqueMonad $ astToIr e
