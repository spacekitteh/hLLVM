{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Llvm.VmCore.IrWriter where

import Llvm.VmCore.Ir 
import qualified Compiler.Hoopl as H
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.CoreIrWriter


  
instance AsmWriter Toplevel where 
  toLlvm (ToplevelTriple s) = "target triple " ++ "=" ++ toLlvm s
  toLlvm (ToplevelDataLayout s) = "target datalayout " ++ "=" ++ toLlvm s
  toLlvm (ToplevelAlias lhs vis link aliasee) = optSepToLlvm lhs "=" ++ 
                                                optSepToLlvm vis " " ++
                                                "alias " ++ optSepToLlvm link " " ++ 
                                                toLlvm aliasee
  toLlvm (ToplevelDbgInit _ _) = undefined
  toLlvm (ToplevelStandaloneMd s t) = "!" ++ s ++ " = " ++ toLlvm t
  toLlvm (ToplevelNamedMd mv nds) = toLlvm mv ++ " = " ++ "!{" ++ 
                                    listToLlvm "" nds ", " "" ++ "}"
  toLlvm (ToplevelDeclare fproto) = "declare " ++ toLlvm fproto
  toLlvm (ToplevelDefine fproto entry graph) = "define " ++ toLlvm fproto ++ "\n" ++
                                               "; the entry label is " ++ 
                                               toLlvm entry ++  "\n" ++
                                               graphToLlvm graph
  toLlvm (ToplevelGlobal lhs linkage vis _ un addrspace gty ty c section align)
                              = optSepToLlvm lhs " = " ++
                                optSepToLlvm linkage " " ++ 
                                optSepToLlvm vis " " ++
                                (if un then "unnamed_addr " else "") ++             
                                optSepToLlvm addrspace " " ++
                                toLlvm gty ++ " " ++
                                toLlvm ty ++ 
                                sepOptToLlvm " " c ++
                                sepOptToLlvm ", " section ++ 
                                sepOptToLlvm ", " align

  toLlvm (ToplevelTypeDef n t) = toLlvm n ++ " = type " ++ toLlvm t
  toLlvm (ToplevelDepLibs l) = "deplibs = " ++ "[" ++ listToLlvm "" l ", " "" ++ "]"
  toLlvm (ToplevelUnamedType x t) = "type " ++ toLlvm t ++ "; " ++ (show x)
  toLlvm (ToplevelModuleAsm qs) = "module asm " ++ toLlvm qs



instance AsmWriter Module where 
  toLlvm (Module tops) = listToLlvm "" tops "\n" ""


instance AsmWriter (Node e x) where
  toLlvm (Nlabel lbl) = toLlvm lbl ++ "\n"
  toLlvm (Pinst i) = toLlvm i
  toLlvm (Cinst c) = toLlvm c
  toLlvm (Tinst t) = toLlvm t
  
  
  
graphToLlvm :: H.Graph Node H.C H.C -> String  
graphToLlvm g = "{\n" ++ 
                H.foldGraphNodes (\n -> \s -> s ++ "\n" ++ (toLlvm n)) g "" ++
                "\n}\n"
