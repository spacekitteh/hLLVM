{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs, RecordWildCards #-}
module Llvm.Hir.Print
       (module Llvm.Hir.Print
       ,module Llvm.Asm.Printer.Common
       ) where

import Llvm.Hir.Data
import Llvm.Hir.Cast

import Llvm.Asm.Printer.Common
import Llvm.Asm.Printer.SharedEntityPrint (integral)
import qualified Llvm.Asm.Printer.SharedEntityPrint as P
import qualified Compiler.Hoopl as H
import qualified Data.Map as M (Map, toList)
import qualified Data.Set as S (Set, toList)

class IrPrint a where
  printIr :: a -> Doc

instance IrPrint v => IrPrint (H.LabelMap v) where
  printIr lm = let ls = H.mapToList lm
               in vcat $ fmap (\(k,v) -> printIr k <+> text "::=>" <+> printIr v) ls

instance (IrPrint t1, IrPrint t2) => IrPrint (t1, t2) where
  printIr (v1, v2) = parens (printIr v1 <> comma <+> printIr v2)

instance IrPrint x => IrPrint [x] where
  printIr l = hsep $ fmap printIr l

instance (IrPrint k, IrPrint v) => IrPrint (M.Map k v) where
  printIr mp = let l = M.toList mp
               in fcat (fmap printIr l)

instance IrPrint v => IrPrint (S.Set v) where
  printIr s = let l = S.toList s
              in fcat (fmap printIr l)

instance IrPrint () where
  printIr _ = text "()"

instance IrPrint Bool where
  printIr b = case b of
    True -> text "true"
    False -> text "false"

commaSepMaybe :: IrPrint a => Maybe a -> Doc
commaSepMaybe = maybe empty ((comma <+>) . printIr)

optSepToLlvm :: IrPrint a => Maybe a -> Doc -> Doc
optSepToLlvm (Nothing) _ = empty
optSepToLlvm (Just x) sep = printIr x <+> sep

instance IrPrint a => IrPrint (Maybe a) where
  printIr (Just x) = printIr x
  printIr Nothing = empty

instance IrPrint Word64 where
  printIr n = integer $ fromIntegral n

instance IrPrint g => IrPrint (TlAlias g) where
  printIr (TlAlias lhs vis dll tlm na link aliasee) =
    hsep [printIr lhs, equals, printIr vis, printIr dll, printIr tlm
         , printIr na, text "alias", printIr link, printIr aliasee]

instance IrPrint g => IrPrint (TlUnamedMd g) where
  printIr x = case x of
    TlUnamedMd s t -> char '!'<>(integer $ fromIntegral s) <+> equals <+> printIr t
    TlUnamedMd_Tagged s tag lst -> char '!'<>(integer $ fromIntegral s) <+> equals <+> text (show tag) <+> equals <+> printIr lst
    _ -> text "()"

instance IrPrint TlNamedMd where
  printIr (TlNamedMd mv nds) = 
    char '!' <> text mv <+> equals <+> char '!'<>(braces (hsep $ punctuate comma $ fmap printIr nds))

instance IrPrint g => IrPrint (TlDeclare g) where
  printIr (TlDeclare fproto) = text "declare" <+> printIr fproto

instance IrPrint TlDeclareIntrinsic where
  printIr (TlDeclareIntrinsic fproto) = text "declareintrinsic" <+> printIr fproto

instance (IrPrint g, IrPrint a) => IrPrint (TlDefine g a) where
  printIr (TlDefine fproto entry graph) = text "define" <+> printIr fproto $$
                                          text "; the entry label is" <+>
                                          printIr entry $$
                                          printIr graph

instance IrPrint g => IrPrint (TlGlobal g) where
  printIr x = case x of
    (TlGlobalDtype lhs linkage vis dll tlm un addrspace externali gty ty c section comdat align) ->
      hsep [printIr lhs, equals, printIr linkage, printIr vis, printIr dll, printIr tlm, printIr un, optSepToLlvm addrspace empty
           , printIr externali, printIr gty, printIr ty, printIr c, commaSepNonEmpty [printIr section, printIr comdat, printIr align]]
    (TlGlobalOpaque lhs linkage vis dll tlm un addrspace externali gty ty c section comdat align) ->
      hsep [printIr lhs, equals, printIr linkage, printIr vis, printIr dll, printIr tlm, printIr un, optSepToLlvm addrspace empty
           , printIr externali, printIr gty, printIr ty, printIr c, commaSepNonEmpty [printIr section, printIr comdat, printIr align]]

instance IrPrint g => IrPrint (TlIntrinsic g) where
  printIr x = case x of
    TlIntrinsic_llvm_used ty c s -> hsep [text "llvm.used", printIr c, printIr s]
    TlIntrinsic_llvm_compiler_used ty c s -> hsep [text "llvm.compiler.used", printIr c, printIr s]
    TlIntrinsic_llvm_global_ctors ty c s -> hsep [text "llvm.global_ctors", printIr c, printIr s]
    TlIntrinsic_llvm_global_dtors ty c s -> hsep [text "llvm.global_dtors", printIr c, printIr s]

instance IrPrint TlTypeDef where
  printIr x = case x of
    (TlDatTypeDef n t) -> hsep [printIr n, equals, text "data type", printIr t]
    (TlOpqTypeDef n t) -> hsep [printIr n, equals, text "opaque type", printIr t]
    (TlFunTypeDef n t) -> hsep [printIr n, equals, text "function type", printIr t]

instance IrPrint TlDepLibs where
  printIr (TlDepLibs l) = hsep [text "deplibs", equals, brackets (hsep $ punctuate comma $ fmap printIr l)]

instance IrPrint TlUnamedType where
  printIr (TlUnamedType x t) = text "type" <+> printIr t <+> text("; " ++ (show x))

instance IrPrint TlModuleAsm where
  printIr (TlModuleAsm qs) = hsep [text "module", text "asm", printIr qs]

instance IrPrint TlAttribute where
  printIr (TlAttribute n l) = hsep [text "attributes", char '#' <> (integral n), braces (hsep $ fmap printIr l)]

instance IrPrint g => IrPrint (TlComdat g) where
  printIr (TlComdat l s) = hsep [printIr l, equals, printIr s]

instance (IrPrint g, IrPrint a) => IrPrint (Toplevel g a) where
  printIr (ToplevelAlias x) = printIr x
  printIr (ToplevelUnamedMd x) = printIr x
  printIr (ToplevelNamedMd x) = printIr x
  printIr (ToplevelDeclare x) = printIr x
  printIr (ToplevelDeclareIntrinsic x) = printIr x
  printIr (ToplevelDefine x) = printIr x
  printIr (ToplevelGlobal x) = printIr x
  printIr (ToplevelTypeDef x) = printIr x
  printIr (ToplevelDepLibs x) = printIr x
  printIr (ToplevelUnamedType x) = printIr x
  printIr (ToplevelModuleAsm x) = printIr x
  printIr (ToplevelAttribute x) = printIr x
  printIr (ToplevelComdat x) = printIr x
  printIr (ToplevelIntrinsic x) = printIr x

instance (IrPrint g, IrPrint a) => IrPrint (Module g a) where
  printIr (Module tops) = fcat $ fmap printIr tops

instance (IrPrint g, IrPrint a) => IrPrint (Node g a e x) where
  printIr (Lnode lbl) = printIr lbl
  printIr (Pnode i dbgs) = commaSepList $ (printIr i):(fmap printIr dbgs)
  printIr (Cnode c dbgs) = commaSepList $ (printIr c):(fmap printIr dbgs)
  printIr (Mnode c dbgs) = commaSepList $ (printIr c):(fmap printIr dbgs)
  printIr (Tnode t dbgs) = commaSepList $ (printIr t):(fmap printIr dbgs)
  printIr (Comment s) = vcat $ fmap (\x -> text ";" <> text x) (commentize s)
  printIr (Enode a dbgs) = commaSepList $ (printIr a):(fmap printIr dbgs)

instance (IrPrint g, IrPrint a) => IrPrint (H.Graph (Node g a) H.C H.C) where
  printIr g = braces (H.foldGraphNodes (\n -> \s -> s $$ (printIr n)) g empty)

instance IrPrint Label where
  printIr l = text (show l)

instance IrPrint Exact where
  printIr Exact = text "exact"

instance IrPrint NoWrap where
    printIr Nsw = text "nsw"
    printIr Nuw = text "nuw"
    printIr Nsuw = text "nsw nuw"

printX :: IrPrint g => (String, Doc, T Utype (Const g), T Utype (Const g)) -> Doc
printX (op, cs, s1, s2) = text op <+> cs <+> parens (printIr s1 <> comma <+> printIr s2)

printF :: IrPrint g => (String, Doc, T Utype (Const g), T Utype (Const g)) -> Doc
printF (op, cs, s1, s2) = text op <+> cs <+> parens (printIr s1 <> comma <+> printIr s2)

printConversion :: IrPrint x => Conversion s x -> (Doc -> Doc) -> Doc
printConversion x p = case x of
  Trunc fv dt -> text "trunc" <+> p (printIr fv <+> text "to" <+> printIr dt)
  Zext fv dt -> text "zext" <+> p (printIr fv <+> text "to" <+> printIr dt)
  Sext fv dt -> text "sext" <+> p (printIr fv <+> text "to" <+> printIr dt)
  FpTrunc fv dt -> text "fptrunc" <+> p (printIr fv <+> text "to" <+> printIr dt)
  FpExt fv dt -> text "fpext" <+> p (printIr fv <+> text "to" <+> printIr dt)
  FpToUi fv dt -> text "fptoui" <+> p (printIr fv <+> text "to" <+> printIr dt)
  FpToSi fv dt -> text "fptosi" <+> p (printIr fv <+> text "to" <+> printIr dt)
  UiToFp fv dt -> text "uitofp" <+> p (printIr fv <+> text "to" <+> printIr dt)
  SiToFp fv dt -> text "sitofp" <+> p (printIr fv <+> text "to" <+> printIr dt)
  PtrToInt fv dt -> text "ptrtoint" <+> p (printIr fv <+> text "to" <+> printIr dt)
  IntToPtr fv dt -> text "inttoptr" <+> p (printIr fv <+> text "to" <+> printIr dt)
  Bitcast fv dt -> text "bitcast" <+> p (printIr fv <+> text "to" <+> printIr dt)
  AddrSpaceCast fv dt -> text "addrspacecast" <+> p (printIr fv <+> text "to" <+> printIr dt)

instance IrPrint g => IrPrint (Conversion s (Const g)) where
  printIr x = printConversion x parens

instance IrPrint g => IrPrint (GetElementPtr s (Const g) (Const g)) where
  printIr (GetElementPtr b base indices) =
    hsep [text "getelementptr", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]

instance IrPrint g => IrPrint (GetElementPtr s (Aliasee g) (Const g)) where
  printIr (GetElementPtr b base indices) =
    hsep [text "getelementptr", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]

instance IrPrint g => IrPrint (Select s r (Const g)) where
  printIr (Select cnd tc1 tc2) = text "select" <+> (parens (commaSepList [printIr cnd, printIr tc1, printIr tc2]))

instance (IrPrint a, IrPrint b) => IrPrint (Either a b) where
  printIr (Left a) = printIr a
  printIr (Right b) = printIr b

instance IrPrint g => IrPrint (Icmp ScalarB (Const g)) where
  printIr (Icmp op t c1 c2) =
    text "icmp" <+> printIr op <+> parens (commaSepList [printIr (T t c1), printIr (T t c2)])

instance IrPrint g => IrPrint (Icmp VectorB (Const g)) where
  printIr (Icmp op t c1 c2) =
    text "icmp" <+> printIr op <+> parens (commaSepList [printIr (T t c1), printIr (T t c2)])

instance IrPrint g => IrPrint (Fcmp s (Const g)) where
  printIr (Fcmp op t c1 c2) =
    text "fcmp" <+> printIr op <+> parens (commaSepList [printIr (T t c1), printIr (T t c2)])

instance IrPrint g => IrPrint (ShuffleVector r (Const g)) where
  printIr (ShuffleVector tc1 tc2 mask) =
    text "shufflevector" <+> parens (commaSepList [printIr tc1, printIr tc2, printIr mask])

instance IrPrint g => IrPrint (ExtractValue (Const g)) where
  printIr (ExtractValue tc indices) =
    hsep [text "extractvalue", parens (commaSepList ((printIr tc:(fmap integral indices))))]

instance IrPrint g => IrPrint (InsertValue (Const g)) where
  printIr (InsertValue vect tc indices) =
    hsep [text "insertvalue", parens (commaSepList ((printIr vect:printIr tc:(fmap integral indices))))]

instance IrPrint g => IrPrint (ExtractElement r (Const g)) where
  printIr (ExtractElement tc index) =
    hsep [text "extractelement", parens (printIr tc <> comma <+> printIr index)]

instance IrPrint g => IrPrint (InsertElement r (Const g)) where
  printIr (InsertElement tc1 tc2 index) =
    hsep [text "insertelement", parens (printIr tc1 <> comma <+> printIr tc2 <> comma <+> printIr index)]

instance IrPrint g => IrPrint (Const g) where
  printIr cst = case cst of
    C_u8 v -> integral v
    C_u16 v -> integral v
    C_u32 v -> integral v
    C_u64 v -> integral v
    C_u96 v -> integral v
    C_u128 v -> integral v
    C_s8 v -> integral v
    C_s16 v -> integral v
    C_s32 v -> integral v
    C_s64 v -> integral v
    C_s96 v -> integral v
    C_s128 v -> integral v
    C_int i -> text i
    C_uhex_int i -> text "u0x" <> (text i)
    C_shex_int i -> text "s0x" <> (text i)
    C_float s -> text s
    C_null -> text "null"
    C_undef -> text "undef"
    C_true -> text "true"
    C_false -> text "false"
    C_zeroinitializer -> text "zeroinitializer"
    C_globalAddr g -> printIr g
    C_str s -> char 'c'<> (doubleQuotes $ text s)

    (C_struct b ts) -> let v = (braces (commaSepList $ fmap printIr ts))
                       in case b of
                         Packed -> angleBrackets v
                         Unpacked -> v
    (C_vector ts) -> angleBrackets (commaSepList $ fmap printIr ts)
    (C_array ts) -> brackets (commaSepList $ fmap printIr ts)
    C_vectorN n e -> angleBrackets (commaSepList $ fmap (\x -> printIr e) [1..n])
    C_arrayN n e -> brackets (commaSepList $ fmap (\x -> printIr e) [1..n])
    C_labelId l -> printIr l
    C_block g a -> text "blockaddress" <+> parens (printIr g <> comma <+> printIr a)

    C_add x t v1 v2 -> printX ("add", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_sub x t v1 v2 -> printX ("sub", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_mul x t v1 v2 -> printX ("mul", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_udiv x t v1 v2 -> printX ("udiv", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_sdiv x t v1 v2 -> printX ("sdiv", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_urem t v1 v2 -> printX ("urem", empty, T (ucast t) v1, T (ucast t) v2)
    C_srem t v1 v2 -> printX ("srem", empty, T (ucast t) v1, T (ucast t) v2)
    C_shl x t v1 v2 -> printX ("shl", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_lshr x t v1 v2 -> printX ("lshr", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_ashr x t v1 v2 -> printX ("ashr", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_and  t v1 v2 -> printX ("and", empty, T (ucast t) v1, T (ucast t) v2)
    C_or  t v1 v2 -> printX ("or", empty, T (ucast t) v1, T (ucast t) v2)
    C_xor t v1 v2 -> printX ("xor", empty, T (ucast t) v1, T (ucast t) v2)

    C_add_V x t v1 v2 -> printX ("add_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_sub_V x t v1 v2 -> printX ("sub_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_mul_V x t v1 v2 -> printX ("mul_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_udiv_V x t v1 v2 -> printX ("udiv_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_sdiv_V x t v1 v2 -> printX ("sdiv_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_urem_V t v1 v2 -> printX ("urem_v", empty, T (ucast t) v1, T (ucast t) v2)
    C_srem_V t v1 v2 -> printX ("srem_v", empty, T (ucast t) v1, T (ucast t) v2)
    C_shl_V x t v1 v2 -> printX ("shl_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_lshr_V x t v1 v2 -> printX ("lshr_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_ashr_V x t v1 v2 -> printX ("ashr_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_and_V  t v1 v2 -> printX ("and_v", empty, T (ucast t) v1, T (ucast t) v2)
    C_or_V  t v1 v2 -> printX ("or_v", empty, T (ucast t) v1, T (ucast t) v2)
    C_xor_V t v1 v2 -> printX ("xor_v", empty, T (ucast t) v1, T (ucast t) v2)

    C_fadd x t v1 v2 -> printF ("fadd", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_fsub x t v1 v2 -> printF ("fsub", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_fmul x t v1 v2 -> printF ("fmul", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_fdiv x t v1 v2 -> printF ("fdiv", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_frem x t v1 v2 -> printF ("frem", printIr x, T (ucast t) v1, T (ucast t) v2)

    C_fadd_V x t v1 v2 -> printF ("fadd_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_fsub_V x t v1 v2 -> printF ("fsub_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_fmul_V x t v1 v2 -> printF ("fmul_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_fdiv_V x t v1 v2 -> printF ("fdiv_v", printIr x, T (ucast t) v1, T (ucast t) v2)
    C_frem_V x t v1 v2 -> printF ("frem_v", printIr x, T (ucast t) v1, T (ucast t) v2)

    {- conversion -}
    C_trunc fv dt -> text "trunc" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_zext fv dt -> text "zext" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_sext fv dt -> text "sext" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fptrunc fv dt -> text "fptrunc" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fpext fv dt -> text "fpext" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fptoui fv dt -> text "fptoui" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fptosi fv dt -> text "fptosi" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_uitofp fv dt -> text "uitofp" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_sitofp fv dt -> text "sitofp" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_ptrtoint fv dt -> text "ptrtoint" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_inttoptr fv dt -> text "inttoptr" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_addrspacecast fv dt -> text "addrspacecast" <+> parens (printIr fv <+> text "to" <+> printIr dt)

    C_bitcast fv dt -> text "bitcast" <+> parens (printIr fv <+> text "to" <+> printIr dt)

    C_trunc_V fv dt -> text "trunc_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_zext_V fv dt -> text "zext_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_sext_V fv dt -> text "sext_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fptrunc_V fv dt -> text "fptrunc_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fpext_V fv dt -> text "fpext_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fptoui_V fv dt -> text "fptoui_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_fptosi_V fv dt -> text "fptosi_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_uitofp_V fv dt -> text "uitofp_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_sitofp_V fv dt -> text "sitofp_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_ptrtoint_V fv dt -> text "ptrtoint_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_inttoptr_V fv dt -> text "inttoptr_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)
    C_addrspacecast_V fv dt -> text "addrspacecast_v" <+> parens (printIr fv <+> text "to" <+> printIr dt)

    C_getelementptr b base indices ->
      hsep [text "getelementptr", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]

    C_getelementptr_V b base indices ->
      hsep [text "getelementptr_v", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]

    C_select_I a -> printIr a
    C_select_F a -> printIr a
    C_select_P a -> printIr a
    C_select_First cnd t f -> hsep [text "select", printIr cnd, printIr t, printIr f]
    C_select_VI a -> printIr a
    C_select_VF a -> printIr a
    C_select_VP a -> printIr a
    C_icmp a -> printIr a
    C_icmp_V a -> printIr a
    C_fcmp a -> printIr a
    C_fcmp_V a -> printIr a
    C_shufflevector_I a -> printIr a
    C_shufflevector_F a -> printIr a
    C_shufflevector_P a -> printIr a
    C_extractelement_I a -> printIr a
    C_extractelement_F a -> printIr a
    C_extractelement_P a -> printIr a
    C_insertelement_I a -> printIr a
    C_insertelement_F a -> printIr a
    C_insertelement_P a -> printIr a
    C_extractvalue a -> printIr a
    C_insertvalue a -> printIr a

instance IrPrint MdName where
  printIr (MdName s) = char '!' <> (text s)

instance IrPrint MdNode where
  printIr (MdNode s) = char '!' <> (integer $ fromIntegral s)

instance IrPrint MdRef where
  printIr x = case x of
    MdRefName s -> printIr s
    MdRefNode s -> printIr s

instance IrPrint g => IrPrint (MetaConst g) where
  printIr (McStruct c) = char '!' <> braces (commaSepList (fmap printIr c))
  printIr (McString s) = char '!' <> (printIr s)
  printIr (McMdRef n) = printIr n
  printIr (McSsa s) = text $ show s
  printIr (McSimple sc) = printIr sc

instance IrPrint g => IrPrint (GetElementPtr s (Value g) (Value g)) where
  printIr (GetElementPtr ib tv tcs) =
    hsep [text "getelementptr", printIr ib, printIr tv, (commaSepList $ fmap printIr tcs)]

instance IrPrint g => IrPrint (Icmp ScalarB (Value g)) where
  printIr (Icmp op t v1 v2) = hsep [text "icmp", printIr op, printIr t, printIr v1 <> comma, printIr v2]

instance IrPrint g => IrPrint (Icmp VectorB (Value g)) where
  printIr (Icmp op t v1 v2) = hsep [text "icmp", printIr op, printIr t, printIr v1 <> comma, printIr v2]

instance IrPrint g => IrPrint (Fcmp s (Value g)) where
  printIr (Fcmp op t v1 v2) = hsep [text "fcmp", printIr op, printIr t, printIr v1 <> comma, printIr v2]

instance IrPrint g => IrPrint (Conversion s (Value g)) where
  printIr x = printConversion x id

instance IrPrint g => IrPrint (Select s r (Value g)) where
  printIr (Select c t f) = text "select" <+> (commaSepList [printIr c, printIr t, printIr f])

instance IrPrint g => IrPrint (Value g) where
  printIr (Val_ssa i) = printIr i
  printIr (Val_const c) = printIr c

instance IrPrint Lname where
  printIr (Lname s) = quotes $ text s

instance IrPrint g => IrPrint (TypedConstOrNull g) where
  printIr (TypedConst tc) = printIr tc
  printIr UntypedNull = text "null"

instance (IrPrint t, IrPrint x) => IrPrint (T t x) where
  printIr (T t v) = printIr t <+> printIr v

instance IrPrint g => IrPrint (FunPtr g) where
  printIr (FunId g) = printIr g
  printIr (FunIdBitcast (T t g) toT) = text "bitcast" <> parens (printIr t <+> printIr g <+> text "to" <+> printIr toT)
  printIr (FunIdInttoptr (T t g) toT) = text "inttoptr" <> parens (printIr t <+> printIr g <+> text "to" <+> printIr toT)
  printIr (FunSsa g) = printIr g
  printIr Fun_null = text "null"
  printIr Fun_undef = text "undef"
  
  
instance IrPrint AsmCode where  
  printIr AsmCode{ asm_dialect = d
                 , asm_dqstring1 = s1
                 , asm_dqstring2 = s2
                 } = printIr d <+> printIr s1 <+> comma <+> printIr s2

instance IrPrint CallSiteType where
  printIr (CallSiteTypeRet e) = printIr e
  printIr (CallSiteTypeFun e as) = printIr (Tpointer (ucast e) as)

instance (IrPrint a) => IrPrint (FunSignature a) where
  printIr FunSignature { fs_callConv = cc
                       , fs_type = ty
                       , fs_params = params
                       } = commaSepList [printIr cc, printIr ty, printIr params]
    
instance IrPrint g => IrPrint (CallFunInterface g) where
  printIr x = case x of
    CallFunInterface{..} -> commaSepList [printIr cfi_tail, 
                                          printIr cfi_signature,
                                          printIr cfi_funAttrs]

instance IrPrint g => IrPrint (InvokeFunInterface g) where
  printIr x = case x of
    InvokeFunInterface{..} -> commaSepList [printIr ifi_signature, 
                                            printIr ifi_funAttrs]

instance IrPrint g => IrPrint (CallAsmInterface g) where
  printIr CallAsmInterface{..} = commaSepList [printIr cai_type,
                                               printIr cai_sideeffect, 
                                               printIr cai_alignstack,
                                               printIr cai_actualParams, 
                                               printIr cai_funAttrs]
  

instance IrPrint g => IrPrint (Clause g) where
  printIr (Catch tv) = text "catch" <+> printIr tv
  printIr (Filter tc) = text "filter" <+> printIr tc
  printIr (CcoS c) = printIr c
  printIr (CcoV c) = printIr c

instance IrPrint (Conversion s GlobalOrLocalId) where
  printIr x = printConversion x parens

instance IrPrint g => IrPrint (ExtractElement r (Value g)) where
  printIr (ExtractElement tv1 tv2) = hsep [text "extractelement", printIr tv1 <> comma, printIr tv2]

instance IrPrint g => IrPrint (InsertElement r (Value g)) where
  printIr (InsertElement vect tv idx) =
    hsep [text "insertelement", printIr vect <> comma, printIr tv <> comma, printIr idx]

instance IrPrint g => IrPrint (ShuffleVector r (Value g)) where
  printIr (ShuffleVector vect1 vect2 mask) =
    hsep [text "shufflevector", printIr vect1 <> comma, printIr vect2 <> comma, printIr mask]

instance IrPrint g => IrPrint (ExtractValue (Value g)) where
  printIr (ExtractValue tv idxs) =
    hsep [text "extractvalue", printIr tv <> comma, (commaSepList $ fmap integral idxs)]

instance IrPrint g => IrPrint (InsertValue (Value g)) where
  printIr (InsertValue vect tv idx) = text "insertvalue" <+> 
                                      hsep (punctuate comma ((printIr vect):(printIr tv):(fmap integral idx)))


instance (IrPrint a) => IrPrint (FunOperand a) where
  printIr x = case x of
    (FunOperandAsRet t att1 align v) ->
      hsep [printIr t, text "asret", hsep $ fmap printIr att1, printIr align, printIr v]
    (FunOperandData t att1 align v) ->
      hsep [printIr t, hsep $ fmap printIr att1, printIr align, printIr v]
    (FunOperandExt e t att1 align v) ->
      hsep [printIr e, printIr t, hsep $ fmap printIr att1, printIr align, printIr v]      
    (FunOperandByVal t att1 align v) ->
      hsep [printIr t, text "byval", hsep $ fmap printIr att1, printIr align, printIr v]      
    (FunOperandLabel t att1 align v) ->
      hsep [printIr t, hsep $ fmap printIr att1, printIr align, printIr v]

instance IrPrint g => IrPrint (MetaOperand g) where
  printIr x = case x of
    MetaOperandData t att1 align v -> hsep [printIr t, hsep $ fmap printIr att1, printIr align, printIr v]
    MetaOperandMeta mc -> printIr mc

instance IrPrint g => IrPrint (Dbg g) where
  printIr (Dbg mv meta) = printIr mv <+> printIr meta


instance IrPrint g => IrPrint (Pinst g) where
  printIr (Pinst t pairs lhs) =  printIr lhs <+> equals <+> text "phi"
                                 <+> printIr t <+> (commaSepList $ fmap tvToLLvm pairs)
    where tvToLLvm (h1,h2) = brackets (printIr h1 <+> comma <+> printIr h2)

instance IrPrint g => IrPrint (Cinst g) where
  printIr x = case x of
    (I_alloca ma t s a lhs) ->
      hsep [printIr lhs, equals, text "alloca", printIr ma, printIr t, commaSepMaybe s, commaSepMaybe a]
    (I_load v ptr align nonterm invar nonull lhs) ->
      hsep [printIr lhs, equals, text "load", printIr v, printIr ptr
           , commaSepNonEmpty [printIr align, printIr nonterm, printIr invar, printIr nonull]]
    (I_loadatomic (Atomicity st ord) v ptr align lhs) ->
      hsep [printIr lhs, equals, text "load atomic", printIr v, printIr ptr, printIr st, printIr ord, commaSepMaybe align]
    (I_store b v addr align nonterm) ->
      hsep [text "store", printIr b, printIr v <> comma, printIr addr, commaSepMaybe align, commaSepMaybe nonterm]
    (I_storeatomic (Atomicity st ord) b v ptr align) ->
      hsep [text "store atomic", printIr b, printIr v <> comma, printIr ptr, printIr st, printIr ord <> commaSepMaybe align]
    (I_fence b order) -> hsep [text "fence", printIr b, printIr order]
    (I_cmpxchg_I wk v p c n st sord ford lhs) ->
      hsep [printIr lhs, equals, text "cmpxchg", printIr wk, printIr v, printIr p <> comma
           , printIr c <> comma, printIr n, printIr st, printIr sord, printIr ford]
    (I_cmpxchg_F wk v p c n st sord ford lhs) ->
      hsep [printIr lhs, equals, text "cmpxchg", printIr wk, printIr v, printIr p <> comma
           , printIr c <> comma, printIr n, printIr st, printIr sord, printIr ford]
    (I_cmpxchg_P wk v p c n st sord ford lhs) ->
      hsep [printIr lhs, equals, text "cmpxchg", printIr wk, printIr v, printIr p <> comma
           , printIr c <> comma, printIr n, printIr st, printIr sord, printIr ford]

    (I_atomicrmw v op p vl st ord lhs) ->
      hsep [printIr lhs, equals, text "atomicrmw", printIr v, printIr op, printIr p <> comma
           , printIr vl, printIr st, printIr ord]
      
    (I_call_fun fna cfi lhs) -> hsep [optSepToLlvm lhs equals, printIr fna, printIr cfi]
    (I_call_asm asm cfi lhs) -> hsep [optSepToLlvm lhs equals, printIr asm, printIr cfi]
    (I_extractelement_I tv idx lhs) -> hsep [printIr lhs, equals, text "extractelement_i", printIr tv, printIr idx]
    (I_extractelement_F tv idx lhs) -> hsep [printIr lhs, equals, text "extractelement_f", printIr tv, printIr idx]
    (I_extractelement_P tv idx lhs) -> hsep [printIr lhs, equals, text "extractelement_p", printIr tv, printIr idx]

    (I_insertelement_I tv v idx lhs) -> hsep [printIr lhs, equals, text "insertelement_i", printIr tv, printIr v, printIr idx]
    (I_insertelement_F tv v idx lhs) -> hsep [printIr lhs, equals, text "insertelement_f", printIr tv, printIr v, printIr idx]
    (I_insertelement_P tv v idx lhs) -> hsep [printIr lhs, equals, text "insertelement_p", printIr tv, printIr v, printIr idx]

    (I_shufflevector_I v1 v2 v3 lhs) -> hsep [printIr lhs, equals, text "shufflevector_i", printIr v1, printIr v2, printIr v3]
    (I_shufflevector_F v1 v2 v3 lhs) -> hsep [printIr lhs, equals, text "shufflevector_f", printIr v1, printIr v2, printIr v3]
    (I_shufflevector_P v1 v2 v3 lhs) -> hsep [printIr lhs, equals, text "shufflevector_p", printIr v1, printIr v2, printIr v3]

    (I_extractvalue v idx lhs) -> hsep ([printIr lhs, equals, text "extractvalue", printIr v] ++ fmap integral idx)
    (I_insertvalue vv v idx lhs) -> hsep ([printIr lhs, equals, text "insertvalue", printIr vv, printIr v] ++ fmap integral idx)

    I_va_arg dv t lhs -> hsep [printIr lhs, equals, text "va_arg", printIr dv, printIr t]
    I_llvm_va_start tv -> text "call" <+> text "void" <+> text "@llvm.va_start" <+> parens(printIr tv)
    I_llvm_va_end tv -> text "call" <+> text "void" <+> text "@llvm.va_end" <+> parens(printIr tv)
    I_llvm_va_copy tv1 tv2 -> text "call" <+> text "void" <+> text "@llvm.va_copy" <+> parens(commaSepList [printIr tv1, printIr tv2])
    I_landingpad rt pt tgl b clauses lhs ->
      hsep ([printIr lhs, equals, text "landingpad", printIr rt, text "personality", printIr pt, printIr tgl, printIr b] ++ fmap printIr clauses)

    I_getelementptr b base indices lhs -> hsep [printIr lhs, equals, text "getelementptr", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]
    I_getelementptr_V b base indices lhs -> hsep [printIr lhs, equals, text "getelementptr_v", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]

    (I_icmp op t v1 v2 lhs) -> hsep [printIr lhs, equals, text "icmp", printIr op, printIr t, printIr v1, printIr v2]
    (I_icmp_V op t v1 v2 lhs) -> hsep [printIr lhs, equals, text "icmp_v", printIr op, printIr t, printIr v1, printIr v2]

    (I_fcmp op t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fcmp", printIr op, printIr t, printIr v1, printIr v2]
    (I_fcmp_V op t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fcmp_v", printIr op, printIr t, printIr v1, printIr v2]

    (I_add n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "add", printIr n, printIr t, printIr v1, printIr v2]
    (I_sub n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "sub", printIr n, printIr t, printIr v1, printIr v2]
    (I_mul n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "mul", printIr n, printIr t, printIr v1, printIr v2]
    (I_udiv n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "udiv", printIr n, printIr t, printIr v1, printIr v2]
    (I_sdiv n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "sdiv", printIr n, printIr t, printIr v1, printIr v2]
    (I_urem t v1 v2 lhs) -> hsep [printIr lhs, equals, text "urem", printIr t, printIr v1, printIr v2]
    (I_srem t v1 v2 lhs) -> hsep [printIr lhs, equals, text "srem", printIr t, printIr v1, printIr v2]
    (I_shl n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "shl", printIr n, printIr t, printIr v1, printIr v2]
    (I_lshr n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "lshr", printIr n, printIr t, printIr v1, printIr v2]
    (I_ashr n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "ashr", printIr n, printIr t, printIr v1, printIr v2]
    (I_and t v1 v2 lhs) -> hsep [printIr lhs, equals, text "and", printIr t, printIr v1, printIr v2]
    (I_or t v1 v2 lhs) -> hsep [printIr lhs, equals, text "or", printIr t, printIr v1, printIr v2]
    (I_xor t v1 v2 lhs) -> hsep [printIr lhs, equals, text "xor", printIr t, printIr v1, printIr v2]

    (I_add_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "add_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_sub_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "sub_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_mul_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "mul_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_udiv_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "udiv_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_sdiv_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "sdiv_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_urem_V t v1 v2 lhs) -> hsep [printIr lhs, equals, text "urem_v", printIr t, printIr v1, printIr v2]
    (I_srem_V t v1 v2 lhs) -> hsep [printIr lhs, equals, text "srem_v", printIr t, printIr v1, printIr v2]
    (I_shl_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "shl_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_lshr_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "lshr_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_ashr_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "ashr_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_and_V t v1 v2 lhs) -> hsep [printIr lhs, equals, text "and_v", printIr t, printIr v1, printIr v2]
    (I_or_V t v1 v2 lhs) -> hsep [printIr lhs, equals, text "or_v", printIr t, printIr v1, printIr v2]
    (I_xor_V t v1 v2 lhs) -> hsep [printIr lhs, equals, text "xor_v", printIr t, printIr v1, printIr v2]


    (I_fadd n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fadd", printIr n, printIr t, printIr v1, printIr v2]
    (I_fsub n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fsub", printIr n, printIr t, printIr v1, printIr v2]
    (I_fmul n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fmul", printIr n, printIr t, printIr v1, printIr v2]
    (I_fdiv n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fdiv", printIr n, printIr t, printIr v1, printIr v2]
    (I_frem n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "frem", printIr n, printIr t, printIr v1, printIr v2]

    (I_fadd_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fadd_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_fsub_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fsub_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_fmul_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fmul_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_fdiv_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "fdiv_v", printIr n, printIr t, printIr v1, printIr v2]
    (I_frem_V n t v1 v2 lhs) -> hsep [printIr lhs, equals, text "frem_v", printIr n, printIr t, printIr v1, printIr v2]

    (I_trunc tv dt lhs) -> hsep [printIr lhs, equals, text "trunc", printIr tv, text "to", printIr dt]
    (I_zext tv dt lhs) -> hsep [printIr lhs, equals, text "zext", printIr tv, text "to", printIr dt]
    (I_sext tv dt lhs) -> hsep [printIr lhs, equals, text "sext", printIr tv, text "to", printIr dt]
    (I_fptrunc tv dt lhs) -> hsep [printIr lhs, equals, text "fptrunc", printIr tv, text "to", printIr dt]
    (I_fpext tv dt lhs) -> hsep [printIr lhs, equals, text "fpext", printIr tv, text "to", printIr dt]
    (I_fptoui tv dt lhs) -> hsep [printIr lhs, equals, text "fptoui", printIr tv, text "to", printIr dt]
    (I_fptosi tv dt lhs) -> hsep [printIr lhs, equals, text "fptosi", printIr tv, text "to", printIr dt]
    (I_uitofp tv dt lhs) -> hsep [printIr lhs, equals, text "uitofp", printIr tv, text "to", printIr dt]
    (I_sitofp tv dt lhs) -> hsep [printIr lhs, equals, text "sitofp", printIr tv, text "to", printIr dt]
    (I_ptrtoint tv dt lhs) -> hsep [printIr lhs, equals, text "ptrtoint", printIr tv, text "to", printIr dt]
    (I_inttoptr tv dt lhs) -> hsep [printIr lhs, equals, text "inttoptr", printIr tv, text "to", printIr dt]
    (I_addrspacecast tv dt lhs) -> hsep [printIr lhs, equals, text "addrspacecast", printIr tv, text "to", printIr dt]

    (I_bitcast tv dt lhs) -> hsep [printIr lhs, equals, text "bitcast", printIr tv, text "to", printIr dt]
    (I_bitcast_D tv dt lhs) -> hsep [printIr lhs, equals, text "bitcast_D", printIr tv, text "to", printIr dt]

    (I_trunc_V tv dt lhs) -> hsep [printIr lhs, equals, text "trunc_v", printIr tv, text "to", printIr dt]
    (I_zext_V tv dt lhs) -> hsep [printIr lhs, equals, text "zext_v", printIr tv, text "to", printIr dt]
    (I_sext_V tv dt lhs) -> hsep [printIr lhs, equals, text "sext_v", printIr tv, text "to", printIr dt]
    (I_fptrunc_V tv dt lhs) -> hsep [printIr lhs, equals, text "fptrunc_v", printIr tv, text "to", printIr dt]
    (I_fpext_V tv dt lhs) -> hsep [printIr lhs, equals, text "fpext_v", printIr tv, text "to", printIr dt]
    (I_fptoui_V tv dt lhs) -> hsep [printIr lhs, equals, text "fptoui_v", printIr tv, text "to", printIr dt]
    (I_fptosi_V tv dt lhs) -> hsep [printIr lhs, equals, text "fptosi_v", printIr tv, text "to", printIr dt]
    (I_uitofp_V tv dt lhs) -> hsep [printIr lhs, equals, text "uitofp_v", printIr tv, text "to", printIr dt]
    (I_sitofp_V tv dt lhs) -> hsep [printIr lhs, equals, text "sitofp_v", printIr tv, text "to", printIr dt]
    (I_ptrtoint_V tv dt lhs) -> hsep [printIr lhs, equals, text "ptrtoint_v", printIr tv, text "to", printIr dt]
    (I_inttoptr_V tv dt lhs) -> hsep [printIr lhs, equals, text "inttoptr_v", printIr tv, text "to", printIr dt]
    (I_addrspacecast_V tv dt lhs) -> hsep [printIr lhs, equals, text "addrspacecast_v", printIr tv, text "to", printIr dt]

    (I_select_I cnd t f lhs) -> hsep [printIr lhs, equals, text "select_I", printIr cnd, printIr t, printIr f]
    (I_select_F cnd t f lhs) -> hsep [printIr lhs, equals, text "select_F", printIr cnd, printIr t, printIr f]
    (I_select_P cnd t f lhs) -> hsep [printIr lhs, equals, text "select_P", printIr cnd, printIr t, printIr f]

    (I_select_First cnd t f lhs) -> hsep [printIr lhs, equals, text "select_First", printIr cnd, printIr t, printIr f]

    (I_select_VI cnd t f lhs) -> hsep [printIr lhs, equals, text "select_VI", printIr cnd, printIr t, printIr f]
    (I_select_VF cnd t f lhs) -> hsep [printIr lhs, equals, text "select_VF", printIr cnd, printIr t, printIr f]
    (I_select_VP cnd t f lhs) -> hsep [printIr lhs, equals, text "select_VP", printIr cnd, printIr t, printIr f]

    I_llvm_memcpy md tv1 tv2 tv3 tv4 tv5 -> text "I_llvm_memcpy" <+> printIr md <+> 
                                            parens (hsep [printIr tv1, printIr tv2, printIr tv3, printIr tv4, printIr tv5])
    I_llvm_memmove md tv1 tv2 tv3 tv4 tv5 -> text "I_llvm_memmove" <+> printIr md <+> 
                                             parens (hsep [printIr tv1, printIr tv2, printIr tv3, printIr tv4, printIr tv5])
    I_llvm_memset md tv1 tv2 tv3 tv4 tv5 -> text "I_llvm_memset" <+> printIr md <+> 
                                            parens (hsep [printIr tv1, printIr tv2, printIr tv3, printIr tv4, printIr tv5])
    I_llvm_read_register ml ma lhs -> hsep [printIr lhs, equals, text "llvm.read_register", printIr ml, printIr ma]
    I_llvm_write_register ml ma mv -> hsep [text "llvm.write_register", printIr ml, printIr ma, printIr mv]
    I_llvm_stacksave lhs -> hsep [printIr lhs, equals, text "llvm.stacksave"]
    I_llvm_stackrestore v -> hsep [text "llvm.stackrestore", printIr v]
    I_llvm_ctpop s dv lhs -> hsep [printIr lhs, equals, text "llvm.ctpop", text s, printIr dv] 

instance IrPrint g => IrPrint (Minst g) where
  printIr mi = case mi of
    Minst cs fn params -> printIr cs <+> printIr fn <+> hsep (fmap printIr params)
    M_llvm_dbg_declare m1 m2 -> text "llvm.dbg.declare" <+> printIr m1 <+> printIr m2
    M_llvm_dbg_func_start m1 -> text "llvm.dbg.func.start" <+> printIr m1
    M_llvm_dbg_stoppoint m1 m2 m3 -> text "llvm.dbg.stoppoint" <+> printIr m1 <+> printIr m2 <+> printIr m3
    M_llvm_dbg_value m1 m2 m3 -> text "llvm.dbg.value" <+> printIr m1 <+> printIr m2 <+> printIr m3   
    M_llvm_dbg_region_end m1 -> text "llvm.dbg.region.end" <+> printIr m1

instance IrPrint MemLen where
  printIr m = case m of
    MemLenI32 -> text "i32"
    MemLenI64 -> text "i64"

instance IrPrint g => IrPrint (Tinst g) where
  printIr T_ret_void = text "ret" <+> text "void"
  printIr (T_return x) = text "ret" <+> (commaSepList $ fmap printIr x)
  printIr (T_br a) = text "br" <+> printIr a
  printIr (T_cbr v t f) = text "br i1" <+> (commaSepList [printIr v, printIr t, printIr f])
  printIr (T_indirectbr v l) = hsep [text "indirectbr", printIr v <> comma, brackets (commaSepList $ fmap printIr l)]
  printIr (T_switch (v,d) tbl) =
    hsep [text "switch", printIr v <> comma, printIr d, brackets (hsep $ fmap (\(p1,p2) -> printIr p1 <> comma <+> printIr p2) tbl)]
  printIr (T_invoke fptr itf toL unwindL lhs) =
    hsep [optSepToLlvm lhs equals, text "invoke", printIr fptr, printIr itf, printIr toL, text "unwind", printIr unwindL]
  printIr (T_invoke_asm asmcode itf toL unwindL lhs) =
    hsep [optSepToLlvm lhs equals, text "invoke", text "asm", printIr asmcode, printIr itf,
          printIr toL, text "unwind", printIr unwindL]
  printIr T_unreachable = text "unreachable"
  printIr (T_resume a) = text "resume" <+> printIr a
  printIr T_unwind = text "unwind"

instance IrPrint g => IrPrint (Aliasee g) where
  printIr (Aliasee tv ) = printIr tv
  printIr (AliaseeTyped dt tv) = printIr dt <+> printIr tv
  printIr (AliaseeConversion c) = printConversion c parens
  printIr (AliaseeGEP a) = printIr a

instance IrPrint g => IrPrint (FunctionInterface g) where
  printIr (FunctionInterface fhLinkage fhVisibility fhDllStorageClass fhSig fhName 
           fhd fhAttr1 fhSection fhCmd fhAlign fhGc fhPrefix fhPrologue) =
    hsep [printIr fhLinkage, printIr fhVisibility, printIr fhDllStorageClass, printIr fhSig
         , printIr fhName, printIr fhd, hsep $ fmap printIr fhAttr1
         , printIr fhSection, printIr fhCmd, printIr fhAlign, printIr fhGc, printIr fhPrefix, printIr fhPrologue]


instance IrPrint g => IrPrint (FunctionDeclare g) where
  printIr (FunctionDeclareData fhLinkage fhVisibility fhDllStorageClass fhSig fhName
           fhd fhAttr1 fhSection fhCmd fhAlign fhGc fhPrefix fhPrologue) =
    hsep [printIr fhLinkage, printIr fhVisibility, printIr fhDllStorageClass, printIr fhSig
         , printIr fhName, printIr fhd, hsep $ fmap printIr fhAttr1
         , printIr fhSection, printIr fhCmd, printIr fhAlign, printIr fhGc, printIr fhPrefix, printIr fhPrologue]
  printIr (FunctionDeclareMeta fhName attrs rtype metakinds) =
    hsep [printIr fhName, printIr attrs, printIr rtype, printIr metakinds]

instance IrPrint IntrinsicDeclare where
  printIr (IntrinsicDeclareData fhSig fhName fhAttr1) =
    hsep [printIr fhSig, printIr fhName, hsep $ fmap printIr fhAttr1]


instance IrPrint MetaKind where
  printIr a = case a of
    MKtype t -> printIr t
    MKmetadata -> text "metadata"

instance IrPrint g => IrPrint (MetaKindedConst g) where
  printIr x = case x of
    MetaKindedConst mk mc -> printIr mk <+> printIr mc
    UnmetaKindedNull -> text "null"

instance IrPrint (Type s x) where
  printIr a = case a of
    TpI i -> text "i" <> integral i
    TpF f -> text "f" <> integral f
    TpV v -> text "v" <> integral v
    TpHalf -> text "half"
    TpFloat -> text "float"
    TpDouble -> text "double"
    TpFp128 -> text "fp128"
    TpX86Fp80 -> text "x86_fp80"
    TpPpcFp128 -> text "ppc_fp128"
    TpX86Mmx -> text "x86_mmx"
    TpLabel -> text "label"
    Tvoid -> text "void"
    Topaque -> text "opaque"
    TpNull -> text "null"

    Tfirst_class_array i t -> brackets (integral i <+> char 'x' <+> printIr t)
    Tfirst_class_struct b ts -> let (start, end) = case b of
                                      Packed -> (char '<', char '>')
                                      Unpacked -> (empty, empty)
                                in start <+> braces (hsep $ punctuate comma $ fmap printIr ts) <+> end
    Tfirst_class_name s -> char '%' <> text s
    TnameScalarI s -> char '%' <> text s
    TnameScalarF s -> char '%' <> text s
    TnameScalarP s -> char '%' <> text s
    TnameVectorI s -> char '%' <> text s
    TnameVectorF s -> char '%' <> text s
    TnameVectorP s -> char '%' <> text s
    TnameRecordD s -> char '%' <> text s
    TnameCodeFunX s -> char '%' <> text s

    Tarray i t -> brackets (integral i <+> char 'x' <+> printIr t)
    TvectorI i t -> char '<' <> integral i <+> char 'x' <+> printIr t <> char '>'
    TvectorF i t -> char '<' <> integral i <+> char 'x' <+> printIr t <> char '>'
    TvectorP i t -> char '<' <> integral i <+> char 'x' <+> printIr t <> char '>'
    Tstruct b ts -> let (start, end) = case b of
                          Packed -> (char '<', char '>')
                          Unpacked -> (empty, empty)
                    in start <+> braces (hsep $ punctuate comma $ fmap printIr ts) <+> end
    Tpointer t addr -> printIr t <+> integral addr <+> text "*"
    Tfunction t fp mv -> printIr t <+> printIr fp <+> maybe empty printIr mv
    Topaque_struct b ts -> let (start, end) = case b of
                                 Packed -> (char '<', char '>')
                                 Unpacked -> (empty, empty)
                           in start <+> braces (hsep $ punctuate comma $ fmap printIr ts) <+> end
    Topaque_array i t -> brackets (integral i <+> char 'x' <+> printIr t)
    TnameOpaqueD s -> char '%' <> text s
      
instance IrPrint MetaFunParam where      
  printIr (MetaFunParam e lv) = printIr e <+> printIr lv

{-
instance IrPrint TypeParamList where
  printIr (TypeParamList params b) = parens (commaSepNonEmpty ((fmap printIr params) ++ [maybe empty printIr b]))
-}

instance IrPrint Utype where
  printIr x = case x of
    UtypeScalarI e -> printIr e
    UtypeScalarF e -> printIr e
    UtypeScalarP e -> printIr e
    UtypeVectorI e -> printIr e
    UtypeVectorF e -> printIr e
    UtypeVectorP e -> printIr e
    UtypeFirstClassD e -> printIr e
    UtypeRecordD e -> printIr e
    UtypeOpaqueD e -> printIr e
    UtypeVoidU e -> printIr e
    UtypeFunX e -> printIr e
    UtypeLabelX e -> printIr e


instance IrPrint Etype where
  printIr x = case x of
    EtypeScalarI e -> printIr e
    EtypeScalarF e -> printIr e
    EtypeScalarP e -> printIr e
    EtypeVectorI e -> printIr e
    EtypeVectorF e -> printIr e
    EtypeVectorP e -> printIr e
    EtypeFirstClassD e -> printIr e
    EtypeRecordD e -> printIr e
    EtypeOpaqueD e -> printIr e
    EtypeFunX e -> printIr e

instance IrPrint Ftype where
  printIr x = case x of
    FtypeScalarI e -> printIr e
    FtypeScalarF e -> printIr e
    FtypeScalarP e -> printIr e
    FtypeVectorI e -> printIr e
    FtypeVectorF e -> printIr e
    FtypeVectorP e -> printIr e
    FtypeFirstClassD e -> printIr e

instance IrPrint Ext where
  printIr x = case x of
    Sign -> text "signext"
    Zero -> text "zeroext"
    
instance IrPrint Mtype where
  printIr x = case x of
    MtypeAsRet dt -> text "sret" <+> printIr dt
    MtypeExt e dt -> printIr e <+> printIr dt
    MtypeData dt -> printIr dt
    MtypeByVal dt -> text "byval" <+> printIr dt
    MtypeLabel dt -> printIr dt

instance IrPrint Dtype where
  printIr x = case x of
    DtypeScalarI e -> printIr e
    DtypeScalarF e -> printIr e
    DtypeScalarP e -> printIr e
    DtypeVectorI e -> printIr e
    DtypeVectorF e -> printIr e
    DtypeVectorP e -> printIr e
    DtypeFirstClassD e -> printIr e
    DtypeRecordD e -> printIr e

instance IrPrint Rtype where
  printIr x = case x of
    RtypeScalarI e -> printIr e
    RtypeScalarF e -> printIr e
    RtypeScalarP e -> printIr e
    RtypeVectorI e -> printIr e
    RtypeVectorF e -> printIr e
    RtypeVectorP e -> printIr e
    RtypeFirstClassD e -> printIr e
    RtypeRecordD e -> printIr e
    RtypeVoidU e -> printIr e

instance IrPrint ScalarType where
  printIr x = case x of
    ScalarTypeI e -> printIr e
    ScalarTypeF e -> printIr e
    ScalarTypeP e -> printIr e

instance IrPrint (IntOrPtrType ScalarB) where
  printIr x = case x of
    IntOrPtrTypeI e -> printIr e
    IntOrPtrTypeP e -> printIr e

instance IrPrint (IntOrPtrType VectorB) where
  printIr x = case x of
    IntOrPtrTypeI e -> printIr e
    IntOrPtrTypeP e -> printIr e

instance IrPrint a => IrPrint (Prefix a) where
  printIr (Prefix n) = text "prefix" <+> printIr n

instance IrPrint a => IrPrint (Prologue a) where
  printIr (Prologue n) = text "prologue" <+> printIr n

instance IrPrint PAttr where
  printIr  x = case x of
    PInReg -> text "inreg"
    PInAlloca -> text "inalloca"
    PNoAlias -> text "noalias"
    PNoCapture -> text "nocapture"
    PNest -> text "nest"
    PReturned -> text "returned"
    PNonNull -> text "nonnull"
    PDereferenceable n -> text "deferenceable" <+> (integer $ fromIntegral n)
    PReadOnly -> text "readonly"
    PReadNone -> text "readnone"

instance IrPrint AlignInByte where
  printIr (AlignInByte n) = text "align" <+> (integer $ fromIntegral n)

instance IrPrint Gname where
  printIr x = case x of
    Gname x -> text x

instance IrPrint g => IrPrint (Comdat g) where
  printIr (Comdat x) = text "comdat" <+> printIr x

instance IrPrint IcmpOp where
  printIr = P.print

instance IrPrint FcmpOp where
  printIr = P.print

instance IrPrint Linkage where
  printIr = P.print

instance IrPrint CallConv where
  printIr = P.print

instance IrPrint Visibility where
  printIr = P.print

instance IrPrint DllStorageClass where
  printIr = P.print

instance IrPrint ThreadLocalStorage where
  printIr = P.print

instance IrPrint RetAttr where
  printIr = P.print

instance IrPrint CallFunAttr where
  printIr = P.print

instance IrPrint ParamAttr where
  printIr = P.print

instance IrPrint FunAttr where
  printIr = P.print

instance IrPrint SelectionKind where
  printIr = P.print

instance IrPrint AddrNaming where
  printIr = P.print

instance IrPrint DqString where
  printIr = P.print


instance IrPrint Section where
  printIr = P.print

instance IrPrint Gc where
    printIr = P.print

instance IrPrint GlobalType where
    printIr = P.print

instance IrPrint GlobalOrLocalId where
  printIr = P.print

instance IrPrint LocalId where
  printIr = P.print


instance IrPrint BinaryConstant where
  printIr = P.print

instance IrPrint AtomicMemoryOrdering where
  printIr = P.print

instance IrPrint AtomicOp where
    printIr = P.print

instance IrPrint Integer where
  printIr = integer

instance IrPrint AddrSpace where
  printIr = integral

instance IrPrint Fparam where
  printIr = P.print

instance IrPrint InAllocaAttr where
  printIr = P.print

instance IrPrint Volatile where
  printIr = P.print

instance IrPrint Weak where
  printIr = P.print

instance IrPrint SingleThread where
  printIr = P.print

instance IrPrint InBounds where
  printIr = P.print

instance (P.Print a, IrPrint a) => IrPrint (IsOrIsNot a) where
  printIr = P.print

instance IrPrint Nontemporal where
  printIr = P.print

instance IrPrint InvariantLoad where
  printIr = P.print

instance IrPrint Nonnull where
  printIr = P.print


instance IrPrint TailCall where
  printIr = P.print


instance IrPrint FastMathFlag where
  printIr = P.print

instance IrPrint FastMathFlags where
  printIr = P.print

instance IrPrint ExternallyInitialized where
  printIr = P.print


instance IrPrint AsmDialect where
  printIr = P.print

instance IrPrint Endianness where
  printIr = P.print

instance IrPrint LayoutAddrSpace where
  printIr = P.print

instance IrPrint SizeInBit where
  printIr = P.print

instance IrPrint AlignInBit where
  printIr = P.print

instance IrPrint StackAlign where
  printIr = P.print

instance IrPrint Mangling where
  printIr = P.print

instance IrPrint AlignMetrics where
  printIr = P.print

instance IrPrint LayoutSpec where
  printIr = P.print

instance IrPrint DataLayout where
  printIr = P.print

instance IrPrint SideEffect where
  printIr = P.print

instance IrPrint AlignStack where
  printIr = P.print

instance IrPrint Cleanup where
  printIr = P.print

instance IrPrint TargetTriple where
  printIr = P.print

instance IrPrint VarArgParam where
  printIr = P.print