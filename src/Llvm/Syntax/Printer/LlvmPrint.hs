{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Llvm.Syntax.Printer.LlvmPrint
       (module Llvm.Syntax.Printer.LlvmPrint
       ,module Llvm.Syntax.Printer.Common
       ) where

import Llvm.Syntax.Printer.Common
import Llvm.Data.Ast
import qualified Llvm.Syntax.Printer.SharedEntityPrint as P
import Llvm.Syntax.Printer.SharedEntityPrint (integral)
import Llvm.Util.Mapping (getValOrImplError)

class AsmPrint a where
  toLlvm :: a -> Doc

commaSepMaybe :: AsmPrint a => Maybe a -> Doc
commaSepMaybe = maybe empty ((comma<+>) . toLlvm)

maybeSepByEquals :: AsmPrint a => Maybe a -> Doc
maybeSepByEquals = maybe empty ((<+>equals).toLlvm)

instance AsmPrint a => AsmPrint (Maybe a) where
  toLlvm (Just x) = toLlvm x
  toLlvm Nothing = empty

instance AsmPrint LabelId where
  toLlvm (LabelString s) = text s
  toLlvm (LabelNumber n) = integral n
  toLlvm (LabelDqNumber n) = doubleQuotes $ integral n
  toLlvm (LabelDqString s) = doubleQuotes $ text s

instance AsmPrint PercentLabel where
  toLlvm (PercentLabel li) = char '%' <> (toLlvm li)
  
instance AsmPrint TargetLabel where
  toLlvm (TargetLabel x) = text "label" <+> toLlvm x
  
instance AsmPrint BlockLabel where
  toLlvm (ImplicitBlockLabel (f, l, c)) = 
    text ";<label>:? ;" <+> parens ((text f) <> comma <+> (integer $ toInteger l) <> comma <+> (integer $ toInteger c))
  toLlvm (ExplicitBlockLabel l) = toLlvm l <> char ':'

instance AsmPrint ComplexConstant where
  toLlvm (Cstruct b ts) = 
    let (start, end) = case b of 
          Packed -> (char '<', char '>') 
          Unpacked -> (empty, empty)
    in start <> braces (commaSepList $ fmap toLlvm ts) <> end
  toLlvm (Cvector ts) = char '<' <+> (commaSepList $ fmap toLlvm ts) <+> char '>'
  toLlvm (Carray ts) = brackets $ commaSepList $ fmap toLlvm ts

instance AsmPrint IbinOp where
  toLlvm x = text $ getValOrImplError (ibinOpMap, "ibinOpMap") x 

instance AsmPrint FbinOp where
  toLlvm x = text $ getValOrImplError (fbinOpMap, "fbinOpMap") x

instance AsmPrint (BinExpr Const) where
  toLlvm (Ie v) = toLlvm v
  toLlvm (Fe v) = toLlvm v

instance AsmPrint (BinExpr Value) where
  toLlvm (Ie v) = toLlvm v
  toLlvm (Fe v) = toLlvm v

instance AsmPrint (IbinExpr Const) where
  toLlvm (IbinExpr op cs t c1 c2) = 
    toLlvm op <+> (hsep $ fmap toLlvm cs) <+> parens (toLlvm (Typed t c1) <> comma <+> toLlvm (Typed t c2))
  
instance AsmPrint (FbinExpr Const) where
  toLlvm (FbinExpr op cs t c1 c2) = 
    toLlvm op <+> toLlvm cs <+> parens (toLlvm (Typed t c1) <> comma <+> toLlvm (Typed t c2))

instance AsmPrint (Conversion Const) where
  toLlvm (Conversion op tc t) = toLlvm op <+> parens (toLlvm tc <+> text "to" <+> toLlvm t)

instance AsmPrint (GetElementPtr Const) where
  toLlvm (GetElementPtr b base indices) = 
    text "getelementptr" <+> toLlvm b <+> parens (commaSepList ((toLlvm base):fmap toLlvm indices))

instance AsmPrint (Select Const) where
  toLlvm (Select cnd tc1 tc2) = 
    text "select" <+> parens (commaSepList [toLlvm cnd, toLlvm tc1, toLlvm tc2])

instance AsmPrint (Icmp Const) where
  toLlvm (Icmp op t c1 c2) = 
    text "icmp" <+> toLlvm op <+> parens (toLlvm (Typed t c1) <> comma <+> toLlvm (Typed t c2))

instance AsmPrint (Fcmp Const) where
  toLlvm (Fcmp op t c1 c2) = 
    text "fcmp" <+> toLlvm op <+> parens (toLlvm (Typed t c1) <> comma <+> toLlvm (Typed t c2))

instance AsmPrint (ShuffleVector Const) where
  toLlvm (ShuffleVector tc1 tc2 mask) = 
    text "shufflevector" <+> parens (commaSepList [toLlvm tc1, toLlvm tc2, toLlvm mask])

instance AsmPrint (ExtractValue Const) where
  toLlvm (ExtractValue tc indices) = 
    text "extractvalue" <+> parens (commaSepList ((toLlvm tc):fmap integral indices))
                                   
instance AsmPrint (InsertValue Const) where
  toLlvm (InsertValue vect tc indices) = 
    text "insertvalue" <+> parens (commaSepList ((toLlvm vect):(toLlvm tc):fmap integral indices)) 
                                       
instance AsmPrint (ExtractElement Const) where
  toLlvm (ExtractElement tc index) = 
    text "extractelement" <+> parens (toLlvm tc <> comma <+> toLlvm index)

instance AsmPrint (InsertElement Const) where                                  
  toLlvm (InsertElement tc1 tc2 index) = 
    text "insertelement" <+> parens (commaSepList [toLlvm tc1, toLlvm tc2, toLlvm index])

instance AsmPrint TrapFlag where
  toLlvm Nuw = text "nuw"
  toLlvm Nsw = text "nsw"
  toLlvm Exact = text "exact"
                                      
instance AsmPrint Const where
  toLlvm x = case x of
    C_simple c -> toLlvm c
    C_complex a -> toLlvm a
    C_labelId l -> toLlvm l
    C_blockAddress g a -> text "blockaddress" <+> parens (toLlvm g <> comma <+> toLlvm a)
    C_binexp a -> toLlvm a
    C_conv a -> toLlvm a 
    C_gep a -> toLlvm a
    C_select a -> toLlvm a
    C_icmp a -> toLlvm a
    C_fcmp a -> toLlvm a 
    C_shufflevector a -> toLlvm a
    C_extractvalue a -> toLlvm a
    C_insertvalue a -> toLlvm a
    C_extractelement a -> toLlvm a
    C_insertelement a -> toLlvm a
--    C_localId v -> toLlvm v

instance AsmPrint MdVar where 
  toLlvm (MdVar s) = char '!'<> (text s)
  
instance AsmPrint MdNode where
  toLlvm (MdNode s) = char '!' <> (text s)
  
instance AsmPrint MetaConst where
  toLlvm (McStruct c) = char '!' <> braces (commaSepList (fmap toLlvm c))
  toLlvm (McString s) = char '!' <> (toLlvm s)
  toLlvm (McMn n) = toLlvm n
  toLlvm (McMv v) = toLlvm v
  toLlvm (McRef s) = toLlvm s
  toLlvm (McSimple sc) = toLlvm sc

instance AsmPrint MetaKindedConst where
  toLlvm x = case x of
    (MetaKindedConst mk mc) -> toLlvm mk <+> toLlvm mc
    UnmetaKindedNull -> text "null"

instance AsmPrint (GetElementPtr Value) where
  toLlvm (GetElementPtr ib tv tcs) = 
    text "getelementptr" <+> toLlvm ib <+> (commaSepList ((toLlvm tv):fmap toLlvm tcs))

instance AsmPrint (IbinExpr Value) where
  toLlvm (IbinExpr op cs t v1 v2) = hsep ((toLlvm op):(fmap toLlvm cs)++[toLlvm t, toLlvm v1 <> comma, toLlvm v2])

instance AsmPrint (FbinExpr Value) where
  toLlvm (FbinExpr op cs t v1 v2) = hsep [toLlvm op, toLlvm cs, toLlvm t, toLlvm v1 <> comma, toLlvm v2]

instance AsmPrint (Icmp Value) where  
  toLlvm (Icmp op t v1 v2) = hsep [text "icmp", toLlvm op, toLlvm t, toLlvm v1 <> comma, toLlvm v2]
  
instance AsmPrint (Fcmp Value) where  
  toLlvm (Fcmp op t v1 v2) = hsep [text "fcmp", toLlvm op, toLlvm t, toLlvm v1 <> comma, toLlvm v2]
  
instance AsmPrint (Conversion Value) where
  toLlvm (Conversion op tv t) = hsep [toLlvm op, toLlvm tv, text "to", toLlvm t]
  
instance AsmPrint (Select Value) where
  toLlvm (Select c t f) = hsep [text "select", toLlvm c <> comma, toLlvm t <> comma, toLlvm f]

instance AsmPrint Expr where
  toLlvm (ExprGetElementPtr a) = toLlvm a
  toLlvm (ExprIcmp a) = toLlvm a
  toLlvm (ExprFcmp a) = toLlvm a
  toLlvm (ExprBinExpr a) = toLlvm a
  toLlvm (ExprConversion a) = toLlvm a
  toLlvm (ExprSelect a) = toLlvm a

instance AsmPrint MemOp where  
  toLlvm (Alloca ma t s a) = hsep [text "alloca", toLlvm ma, hcat [toLlvm t, commaSepMaybe s, commaSepMaybe a]]
  toLlvm (Load b ptr align noterm inv nonul) = 
    hsep [text "load", toLlvm b, hcat [toLlvm ptr, commaSepMaybe align, commaSepMaybe noterm, commaSepMaybe inv, commaSepMaybe nonul]]
  toLlvm (LoadAtomic (Atomicity st ord) b ptr align) = 
    hsep [text "load", text "atomic", toLlvm b, toLlvm ptr, toLlvm st, toLlvm ord] <> (commaSepMaybe align)
  toLlvm (Store b v addr align noterm) = 
    hsep [text "store", toLlvm b, toLlvm v <> comma, toLlvm addr <> (commaSepMaybe align) <> (commaSepMaybe noterm)]
  toLlvm (StoreAtomic (Atomicity st ord) b v ptr align) = 
    hsep [text "store", text "atomic", toLlvm b, toLlvm v <> comma, toLlvm ptr, toLlvm st, toLlvm ord] <> (commaSepMaybe align)
  toLlvm (Fence b order) = hsep [text "fence", toLlvm b, toLlvm order]
  toLlvm (CmpXchg wk v p c n st sord ford) = 
    hsep [text "cmpxchg", toLlvm wk, toLlvm v, toLlvm p <> comma, toLlvm c <> comma, toLlvm n, toLlvm st, toLlvm sord, toLlvm ford]
  toLlvm (AtomicRmw v op p vl st ord) = 
    hsep [text "atomicrmw", toLlvm v, toLlvm op, toLlvm p <> comma, toLlvm vl, toLlvm st, toLlvm ord]

instance AsmPrint Value where
  toLlvm (Val_local i) = toLlvm i
  toLlvm (Val_const c) = toLlvm c

instance AsmPrint v => AsmPrint (Typed v) where
  toLlvm (Typed t v) = toLlvm t <+> toLlvm v

instance AsmPrint a => AsmPrint (Pointer a) where
  toLlvm (Pointer i) = toLlvm i

instance AsmPrint FunName where
  toLlvm (FunNameGlobal s) = toLlvm s
  toLlvm (FunNameBitcast tv t) = text "bitcast" <> parens (toLlvm tv <+> text "to" <+> toLlvm t)
  toLlvm (FunNameInttoptr tv t) = text "inttoptr" <> parens (toLlvm tv <+> text "to" <+> toLlvm t)  
  toLlvm FunName_null = text "null"  
  toLlvm FunName_undef = text "null"                         
  
instance AsmPrint CallSite where
  toLlvm (CallSiteFun cc ra rt ident params fa) = 
    hsep [toLlvm cc, hsep $ fmap toLlvm ra, toLlvm rt, toLlvm ident, 
          parens (commaSepList $ fmap toLlvm params), hsep $ fmap toLlvm fa]
  toLlvm (CallSiteAsm t se as dia s1 s2 params fa) = 
    hsep [toLlvm t, text "asm", toLlvm se, toLlvm as, toLlvm dia, 
          toLlvm s1 <> comma, toLlvm s2, parens (commaSepList $ fmap toLlvm params), hsep $ fmap toLlvm fa]
   

instance AsmPrint Clause where
  toLlvm (ClauseCatch tv) = text "catch" <+> toLlvm tv
  toLlvm (ClauseFilter tc) = text "filter" <+> toLlvm tc
  toLlvm (ClauseConversion c) = toLlvm c

instance AsmPrint (Conversion GlobalOrLocalId) where
  toLlvm (Conversion op (Typed t g) dt) = toLlvm op <+> parens (hsep [toLlvm t, toLlvm g, text "to", toLlvm dt])
  
instance AsmPrint (ExtractElement Value) where
  toLlvm (ExtractElement tv1 tv2) = 
    text "extractelement" <+> (commaSepList [toLlvm tv1, toLlvm tv2])
  
instance AsmPrint (InsertElement Value) where  
  toLlvm (InsertElement vect tv idx) = 
    text "insertelement" <+> (commaSepList [toLlvm vect, toLlvm tv, toLlvm idx])
  
instance AsmPrint (ShuffleVector Value) where  
  toLlvm (ShuffleVector vect1 vect2 mask) = 
    text "shufflevector" <+> (commaSepList [toLlvm vect1, toLlvm vect2, toLlvm mask])
  
instance AsmPrint (ExtractValue Value) where  
  toLlvm (ExtractValue tv idxs) = text "extractvalue" <+> (commaSepList ((toLlvm tv):(fmap integral idxs)))
  
instance AsmPrint (InsertValue Value) where  
  toLlvm (InsertValue vect tv idxs) = 
    text "insertvalue" <+> (commaSepList ((toLlvm vect):(toLlvm tv):(fmap integral idxs)))


instance AsmPrint Rhs where
  toLlvm (RhsMemOp a) = toLlvm a
  toLlvm (RhsExpr a) = toLlvm a
  toLlvm (RhsCall tailc callSite) = hsep [toLlvm tailc, text "call", toLlvm callSite]
  toLlvm (RhsExtractElement a) = toLlvm a
  toLlvm (RhsInsertElement a) = toLlvm a
  toLlvm (RhsShuffleVector a) = toLlvm a
  toLlvm (RhsExtractValue a) = toLlvm a
  toLlvm (RhsInsertValue a) = toLlvm a
  toLlvm (RhsVaArg (VaArg tv t)) = hsep [text "va_arg", toLlvm tv <> comma, toLlvm t]
  toLlvm (RhsLandingPad (LandingPad rt pt tgl b clause)) = 
    hsep ([text "landingpad", toLlvm rt, text "personality", toLlvm pt, toLlvm tgl, toLlvm b] 
          ++ (fmap toLlvm clause))

instance AsmPrint ActualParam where
  toLlvm x = case x of
    ActualParamData t att1 align v att2 ->
      hsep [toLlvm t, hsep $ fmap toLlvm att1, toLlvm align, toLlvm v, hsep $ fmap toLlvm att2]
    ActualParamLabel t att1 align v att2 ->
      hsep [toLlvm t, hsep $ fmap toLlvm att1, toLlvm align, toLlvm v, hsep $ fmap toLlvm att2]      
    ActualParamMeta mc -> toLlvm mc

instance AsmPrint Dbg where
  toLlvm (Dbg mv meta) = toLlvm mv <+> toLlvm meta

instance AsmPrint PhiInst where
  toLlvm (PhiInst lhs t pairs) =  hsep [maybe empty ((<+> equals) . toLlvm) lhs, text "phi"
                                       , toLlvm t, commaSepList $ fmap tvToLLvm pairs]
    where tvToLLvm (h1,h2) = brackets (toLlvm h1 <> comma <+> toLlvm h2)

instance AsmPrint ComputingInst where
  toLlvm (ComputingInst lhs rhs) = maybe empty ((<+> equals) . toLlvm) lhs <+> toLlvm rhs

instance AsmPrint TerminatorInst where
  toLlvm RetVoid = text "ret" <+> text "void"
  toLlvm (Return x) = text "ret" <+> (commaSepList $ fmap toLlvm x)
  toLlvm (Br a) = text "br" <+> toLlvm a
  toLlvm (Cbr v t f) = hsep [text "br",  text "i1", toLlvm v <> comma, toLlvm t <> comma, toLlvm f]
  toLlvm (IndirectBr v l) = hsep [text "indirectbr", toLlvm v <> comma, brackets (commaSepList $ fmap toLlvm l)]
  toLlvm (Switch v d tbl) = 
    hsep [text "switch", toLlvm v <> comma, toLlvm d
         , brackets (hsep $ fmap (\(p1,p2) -> toLlvm p1 <> comma <+> toLlvm p2) tbl)]
  toLlvm (Invoke lhs callSite toL unwindL) = 
    hsep [maybe empty ((<+> equals) . toLlvm) lhs, text "invoke", toLlvm callSite, text "to"
         , toLlvm toL, text "unwind", toLlvm unwindL]
  toLlvm Unreachable = text "unreachable"
  toLlvm (Resume a) = text "resume" <+> toLlvm a
             
instance AsmPrint PhiInstWithDbg where
  toLlvm (PhiInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)

instance AsmPrint TerminatorInstWithDbg where
  toLlvm (TerminatorInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)

instance AsmPrint ComputingInstWithDbg where
  toLlvm ci = case ci of
    ComputingInstWithDbg ins dbgs -> commaSepList ((toLlvm ins):fmap toLlvm dbgs)
    ComputingInstWithComment s -> char ';' <+> text s 

instance AsmPrint Aliasee where
  toLlvm (AliaseeTv tv ) = toLlvm tv
  toLlvm (AliaseeConversion c) = toLlvm c
  toLlvm (AliaseeGetElementPtr a) = toLlvm a

instance AsmPrint FunctionPrototype where
  toLlvm (FunctionPrototype fhLinkage fhVisibility fhDllStorageClass fhCCoonc fhAttr fhRetType fhName fhParams fnd 
          fhAttr1 fhSection fhcmd fhAlign fhGc fhPrefix fhPrologue) = 
    hsep [toLlvm fhLinkage, toLlvm fhVisibility, toLlvm fhDllStorageClass, toLlvm fhCCoonc, hsep $ fmap toLlvm fhAttr 
         , toLlvm fhRetType, toLlvm fhName, toLlvm fhParams, toLlvm fnd, hsep $ fmap toLlvm fhAttr1
         , toLlvm fhSection, toLlvm fhcmd, toLlvm fhAlign, toLlvm fhGc, toLlvm fhPrefix, toLlvm fhPrologue]

instance AsmPrint Block where
  toLlvm (Block lbl phis ins end) = toLlvm lbl $$
                                    (nest 2 
                                     ((vcat $ fmap toLlvm phis) $$
                                      (vcat $ fmap toLlvm ins) $$
                                      toLlvm end))

instance AsmPrint Toplevel where 
  toLlvm (ToplevelTriple (TlTriple s)) = hsep [text "target", text "triple", equals, toLlvm s]
  toLlvm (ToplevelDataLayout (TlDataLayout s)) = hsep [text "target", text "datalayout", equals, toLlvm s]
  toLlvm (ToplevelAlias (TlAlias lhs vis dll tlm naddr link aliasee)) = 
    hsep [toLlvm lhs, equals, toLlvm vis, toLlvm dll, toLlvm tlm, toLlvm naddr, text "alias", toLlvm link, toLlvm aliasee]
  toLlvm (ToplevelDbgInit (TlDbgInit s i)) = error "DbgInit is not implemented"
  toLlvm (ToplevelStandaloneMd smd) = toLlvm smd
  toLlvm (ToplevelNamedMd (TlNamedMd mv nds)) = toLlvm mv <+> equals <+> char '!'<>(braces (commaSepList $ fmap toLlvm nds))
  toLlvm (ToplevelDeclare (TlDeclare fproto)) = text "declare" <+> toLlvm fproto
  toLlvm (ToplevelDefine (TlDefine fproto blocks)) = text "define" <+> toLlvm fproto <+> text "{" $$
                                          (fcat $ fmap toLlvm blocks) $$ text "}"
  toLlvm (ToplevelGlobal (TlGlobal lhs linkage vis dllStor threadLoc un addrspace externali gty ty const0 
                          section comdat align)) = 
    hsep [maybeSepByEquals lhs, toLlvm linkage, toLlvm vis, toLlvm dllStor, toLlvm threadLoc, toLlvm un
         , toLlvm addrspace, toLlvm externali, toLlvm gty, toLlvm ty, toLlvm const0]
    <> (hcat [commaSepMaybe section, commaSepMaybe comdat, commaSepMaybe align])

  toLlvm (ToplevelTypeDef (TlTypeDef n t)) = toLlvm n <+> equals <+> text "type" <+> toLlvm t
  toLlvm (ToplevelDepLibs (TlDepLibs l)) = text "deplibs" <+> equals <+> brackets (commaSepList $ fmap toLlvm l)
  toLlvm (ToplevelUnamedType (TlUnamedType x t)) = text "type" <+> toLlvm t <+> text "; " <+> (integral x)
  toLlvm (ToplevelModuleAsm (TlModuleAsm qs)) = text "module asm" <+> toLlvm qs
  toLlvm (ToplevelAttribute (TlAttribute n l)) = 
    hsep [text "attributes", char '#' <> (integral n), equals, braces (hsep $ fmap toLlvm l)]
  toLlvm (ToplevelComdat (TlComdat l c)) = 
    hsep [toLlvm l, equals, text "comdat", toLlvm c]

instance AsmPrint TlStandaloneMd where
  toLlvm (TlStandaloneMd lhs rhs) = char '!' <> (text lhs) <+> equals <+> toLlvm rhs

instance AsmPrint FormalParam where
  toLlvm x = case x of
    (FormalParamData t att1 align id att2) ->
      (toLlvm t) <+> (hsep $ fmap toLlvm att1) 
      <> (maybe empty ((comma <+>) . toLlvm) align) <+> (toLlvm id) <+> (hsep $ fmap toLlvm att2)
    (FormalParamMeta e lv) -> toLlvm e <+> toLlvm lv

instance AsmPrint FormalParamList where
  toLlvm (FormalParamList params var atts) =
    parens (commaSepNonEmpty ((fmap toLlvm params) ++ [maybe empty toLlvm var])) <+> (hsep $ fmap toLlvm atts)

instance AsmPrint TypeParamList where
  toLlvm (TypeParamList params b) = parens (commaSepNonEmpty ((fmap toLlvm params) ++ [maybe empty toLlvm b]))

instance AsmPrint TypePrimitive where
  toLlvm a = case a of 
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
    TpNull -> text "null"

instance AsmPrint MetaKind where
  toLlvm a = case a of
    Mtype e -> toLlvm e
    Mmetadata -> text "metadata"

instance AsmPrint Type where
  toLlvm a = case a of 
    Tprimitive tp -> toLlvm tp
    Tvoid -> text "void"
    Topaque -> text "opaque"
    Tname s -> char '%' <> text s 
    TquoteName s -> char '%'<> (doubleQuotes $ text s)
    Tno i -> char '%'<> integral i
    Tarray i t -> brackets (integral i <+> char 'x' <+> toLlvm t)
    Tvector i t -> char '<' <> integral i <+> char 'x' <+> toLlvm t <> char '>'
    Tstruct b ts -> let (start, end) = case b of 
                          Packed -> (char '<', char '>')
                          Unpacked -> (empty, empty)
                    in start <> braces (hsep $ punctuate comma $ fmap toLlvm ts) <> end
    Tpointer t addr -> toLlvm t <+> toLlvm addr <> text "*"
    Tfunction t fp atts -> toLlvm t <+> toLlvm fp <+> (hsep $ punctuate comma $ fmap toLlvm atts)

instance AsmPrint AddrSpace where
  toLlvm (AddrSpace n) = text "addrspace" <+> (parens $ integral n)
  toLlvm AddrSpaceUnspecified = empty

instance AsmPrint ConvertOp where
  toLlvm x = text $ getValOrImplError (convertOpMap, "convertOpMap") x

instance AsmPrint TypedConstOrNull where  
  toLlvm (TypedConst tc) = toLlvm tc
  toLlvm UntypedNull = text "null"

instance AsmPrint Module where 
  toLlvm (Module tops) = vcat $ fmap toLlvm tops

instance AsmPrint Prefix where
  toLlvm (Prefix n) = text "prefix" <+> toLlvm n
  
instance AsmPrint Prologue where  
  toLlvm (Prologue n) = text "prologue" <+> toLlvm n

instance AsmPrint IcmpOp where
  toLlvm = P.print 

instance AsmPrint FcmpOp where
  toLlvm = P.print

instance AsmPrint Linkage where
  toLlvm = P.print

instance AsmPrint CallConv where
  toLlvm = P.print

instance AsmPrint Visibility where
  toLlvm = P.print

instance AsmPrint DllStorageClass where
  toLlvm = P.print
  
instance AsmPrint ThreadLocalStorage where  
  toLlvm = P.print

instance AsmPrint CallRetAttr where
  toLlvm = P.print

instance AsmPrint CallFunAttr where
  toLlvm = P.print

instance AsmPrint ParamAttr where
  toLlvm = P.print

instance AsmPrint FunAttr where
  toLlvm = P.print
  
instance AsmPrint SelectionKind where  
  toLlvm = P.print
   
instance AsmPrint AddrNaming where
  toLlvm = P.print

instance AsmPrint DqString where
  toLlvm = P.print

instance AsmPrint Section where
  toLlvm = P.print

instance AsmPrint Alignment where
  toLlvm = P.print

instance AsmPrint Gc where
  toLlvm = P.print

instance AsmPrint GlobalType where
  toLlvm = P.print

instance AsmPrint GlobalOrLocalId where
  toLlvm = P.print
                    
instance AsmPrint LocalId where
  toLlvm = P.print
                      
instance AsmPrint GlobalId where
  toLlvm = P.print

instance AsmPrint SimpleConstant where
  toLlvm = P.print
                          
instance AsmPrint AtomicMemoryOrdering where
  toLlvm = P.print

instance AsmPrint AtomicOp where
  toLlvm = P.print
    
instance AsmPrint Fparam where
  toLlvm = P.print
  
instance AsmPrint VarArgParam where
  toLlvm = P.print

instance AsmPrint InAllocaAttr where
  toLlvm = P.print
    
instance AsmPrint Volatile where
  toLlvm = P.print

instance AsmPrint Weak where
  toLlvm = P.print
  
instance AsmPrint SingleThread where
  toLlvm = P.print

instance AsmPrint InBounds where
  toLlvm = P.print
  
instance (P.Print a, AsmPrint a) => AsmPrint (IsOrIsNot a) where
  toLlvm = P.print

instance AsmPrint Nontemporal where    
  toLlvm = P.print
  
instance AsmPrint InvariantLoad where
  toLlvm = P.print
  
instance AsmPrint Nonnull where
  toLlvm = P.print

instance AsmPrint TailCall where
  toLlvm = P.print

instance AsmPrint DollarId where
  toLlvm = P.print

instance AsmPrint Comdat where
  toLlvm = P.print

instance AsmPrint FastMathFlag where  
  toLlvm = P.print
                
instance AsmPrint FastMathFlags where                
  toLlvm = P.print
  
instance AsmPrint ExternallyInitialized where  
  toLlvm = P.print
  
instance AsmPrint AsmDialect where  
  toLlvm = P.print

instance AsmPrint Endianness where
  toLlvm = P.print
  
instance AsmPrint LayoutAddrSpace where  
  toLlvm = P.print
  
instance AsmPrint SizeInBit where  
  toLlvm = P.print
  
instance AsmPrint AlignInBit where  
  toLlvm = P.print
  
instance AsmPrint StackAlign where  
  toLlvm = P.print
  
instance AsmPrint Mangling where
  toLlvm = P.print
    
instance AsmPrint AbiAlign where    
  toLlvm = P.print

instance AsmPrint PrefAlign where
  toLlvm = P.print
  
instance AsmPrint LayoutSpec where  
  toLlvm = P.print
  
instance AsmPrint DataLayout where
  toLlvm = P.print
  
instance AsmPrint SideEffect where  
  toLlvm = P.print
  
instance AsmPrint AlignStack where  
  toLlvm = P.print
  
instance AsmPrint Cleanup where  
  toLlvm = P.print

instance AsmPrint TargetTriple where
  toLlvm = P.print