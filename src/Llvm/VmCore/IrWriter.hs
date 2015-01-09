{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Llvm.VmCore.IrWriter where

import Llvm.VmCore.Ir 
import qualified Compiler.Hoopl as H
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.CoreIrWriter


  
instance AsmWriter Toplevel where 
  toLlvm (ToplevelTriple s) = text "target" <+> text "triple" <+> text "=" <+> toLlvm s
  toLlvm (ToplevelDataLayout s) = text "target" <+> text "datalayout" <+> text "=" <+> toLlvm s
  toLlvm (ToplevelAlias lhs vis dll tlm na link aliasee) = 
    toLlvm lhs <+> equals
    <+> (maybe empty toLlvm vis) 
    <+> (maybe empty toLlvm dll)
    <+> (maybe empty toLlvm tlm)
    <+> (toLlvm na)
    <+> (text "alias") <+> (maybe empty toLlvm link)
    <+> toLlvm aliasee
  toLlvm (ToplevelDbgInit _ _) = undefined
  toLlvm (ToplevelStandaloneMd s t) = char '!'<>(text s) <+> equals <+> toLlvm t
  toLlvm (ToplevelNamedMd mv nds) = toLlvm mv <+> equals <+> char '!'<>(braces (hsep $ punctuate comma $ fmap toLlvm nds))
  toLlvm (ToplevelDeclare fproto) = text "declare" <+> toLlvm fproto
  toLlvm (ToplevelDefine fproto entry graph) = text "define" <+> toLlvm fproto $$
                                               text "; the entry label is" <+> 
                                               toLlvm entry $$
                                               graphToLlvm graph
  toLlvm (ToplevelGlobal lhs linkage vis dll tlm un addrspace externali gty ty c section comdat align)
                              = optSepToLlvm lhs equals <+>
                                optSepToLlvm linkage empty <+>
                                optSepToLlvm vis empty <+>
                                optSepToLlvm dll empty <+>
                                optSepToLlvm tlm empty <+>
                                toLlvm un <+> 
                                optSepToLlvm addrspace empty <+>
                                toLlvm externali <+>
                                toLlvm gty <+>
                                toLlvm ty <+>
                                sepOptToLlvm empty c <+>
                                sepOptToLlvm comma section <+> 
                                sepOptToLlvm comma comdat <+>
                                sepOptToLlvm comma align

  toLlvm (ToplevelTypeDef n t) = toLlvm n <+> equals <+> text "type" <+> toLlvm t
  toLlvm (ToplevelDepLibs l) = text "deplibs" <+> equals <+> brackets (hsep $ punctuate comma $ fmap toLlvm l)
  toLlvm (ToplevelUnamedType x t) = text "type" <+> toLlvm t <+> text("; " ++ (show x))
  toLlvm (ToplevelModuleAsm qs) = text "module" <+> text "asm" <+> toLlvm qs
  toLlvm (ToplevelAttribute n l) = text "attributes" <+> char '#' <> (integer n) 
                                   <+> braces (hsep $ fmap toLlvm l)
  toLlvm (ToplevelComdat l s) = toLlvm l <+> equals <+> toLlvm s


instance AsmWriter Module where 
  toLlvm (Module tops) = fcat $ fmap toLlvm tops


instance AsmWriter (Node e x) where
  toLlvm (Nlabel lbl) = toLlvm lbl
  toLlvm (Pinst i) = toLlvm i
  toLlvm (Cinst c) = toLlvm c
  toLlvm (Tinst t) = toLlvm t
  
  
  
graphToLlvm :: H.Graph Node H.C H.C -> Doc
graphToLlvm g = braces (H.foldGraphNodes (\n -> \s -> s $$ (toLlvm n)) g empty)

