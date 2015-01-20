{-# LANGUAGE FlexibleInstances #-}
module Llvm.Syntax.Printer.LlvmPrint
       (module Llvm.Syntax.Printer.LlvmPrint
       ,module Llvm.Syntax.Printer.Common
       ) where

import Llvm.Syntax.Printer.Common
import Llvm.Data.Ast
import qualified Llvm.Syntax.Printer.SharedEntityPrint as P
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
  toLlvm (LabelString s) = toLlvm s 
  toLlvm (LabelNumber n) = integer n
  toLlvm (LabelDqNumber n) = doubleQuotes $ integer n
  toLlvm (LabelDqString s) = doubleQuotes $ toLlvm s

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
    toLlvm op <+> (hsep $ fmap toLlvm cs) <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))
  
instance AsmPrint (FbinExpr Const) where
  toLlvm (FbinExpr op cs t c1 c2) = 
    toLlvm op <+> toLlvm cs <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))

instance AsmPrint (Conversion TypedConst) where
  toLlvm (Conversion op tc t) = toLlvm op <+> parens (toLlvm tc <+> text "to" <+> toLlvm t)

instance AsmPrint (GetElemPtr TypedConst) where
  toLlvm (GetElemPtr b base indices) = 
    text "getelementptr" <+> toLlvm b <+> parens (commaSepList ((toLlvm base):fmap toLlvm indices))

instance AsmPrint (Select TypedConst) where
  toLlvm (Select cnd tc1 tc2) = 
    text "select" <+> parens (commaSepList [toLlvm cnd, toLlvm tc1, toLlvm tc2])

instance AsmPrint (Icmp Const) where
  toLlvm (Icmp op t c1 c2) = 
    text "icmp" <+> toLlvm op <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))

instance AsmPrint (Fcmp Const) where
  toLlvm (Fcmp op t c1 c2) = 
    text "fcmp" <+> toLlvm op <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))

instance AsmPrint (ShuffleVector TypedConst) where
  toLlvm (ShuffleVector tc1 tc2 mask) = 
    text "shufflevector" <+> parens (commaSepList [toLlvm tc1, toLlvm tc2, toLlvm mask])

instance AsmPrint (ExtractValue TypedConst) where
  toLlvm (ExtractValue tc indices) = 
    text "extractvalue" <+> parens (commaSepList ((toLlvm tc):fmap text indices))
                                   
instance AsmPrint (InsertValue TypedConst) where
  toLlvm (InsertValue vect tc indices) = 
    text "insertvalue" <+> parens (commaSepList ((toLlvm vect):(toLlvm tc):fmap text indices)) 
                                       
instance AsmPrint (ExtractElem TypedConst) where
  toLlvm (ExtractElem tc index) = 
    text "extractelement" <+> parens (toLlvm tc <> comma <+> toLlvm index)
                                  
instance AsmPrint (InsertElem TypedConst) where                                  
  toLlvm (InsertElem tc1 tc2 index) = 
    text "insertelement" <+> parens (commaSepList [toLlvm tc1, toLlvm tc2, toLlvm index])

instance AsmPrint TrapFlag where
  toLlvm Nuw = text "nuw"
  toLlvm Nsw = text "nsw"
  toLlvm Exact = text "exact"
                                      
instance AsmPrint Const where
  toLlvm x = case x of
    Ccp c -> toLlvm c
    Cca a -> toLlvm a
    Cl l -> toLlvm l
    CblockAddress g a -> text "blockaddress" <+> parens (toLlvm g <> comma <+> toLlvm a)
    Cb a -> toLlvm a
    Cconv a -> toLlvm a 
    CgEp a -> toLlvm a
    Cs a -> toLlvm a
    CiC a -> toLlvm a
    CfC a -> toLlvm a 
    CsV a -> toLlvm a
    CeV a -> toLlvm a
    CiV a -> toLlvm a
    CeE a -> toLlvm a
    CiE a -> toLlvm a
    CmC mc -> toLlvm mc
    CmL v -> toLlvm v

instance AsmPrint MdVar where 
  toLlvm (MdVar s) = char '!'<> (text s)
  
instance AsmPrint MdNode where
  toLlvm (MdNode s) = char '!' <> (text s)
  
instance AsmPrint MetaConst where
  toLlvm (MdConst c) = char '!' <> (toLlvm c)
  toLlvm (MdString s) = char '!' <> (toLlvm s)
  toLlvm (McMn n) = toLlvm n
  toLlvm (McMv v) = toLlvm v
  toLlvm (MdRef s) = text $ show s
                        
instance AsmPrint (GetElemPtr TypedValue) where
  toLlvm (GetElemPtr ib tv tcs) = 
    text "getelementptr" <+> toLlvm ib <+> (commaSepList ((toLlvm tv):fmap toLlvm tcs))
  
instance AsmPrint (IbinExpr Value) where
  toLlvm (IbinExpr op cs t v1 v2) = hsep ((toLlvm op):(fmap toLlvm cs)++[toLlvm t, toLlvm v1 <> comma, toLlvm v2])

instance AsmPrint (FbinExpr Value) where
  toLlvm (FbinExpr op cs t v1 v2) = hsep [toLlvm op, toLlvm cs, toLlvm t, toLlvm v1 <> comma, toLlvm v2]

instance AsmPrint (Icmp Value) where  
  toLlvm (Icmp op t v1 v2) = hsep [text "icmp", toLlvm op, toLlvm t, toLlvm v1 <> comma, toLlvm v2]
  
instance AsmPrint (Fcmp Value) where  
  toLlvm (Fcmp op t v1 v2) = hsep [text "fcmp", toLlvm op, toLlvm t, toLlvm v1 <> comma, toLlvm v2]
  
instance AsmPrint (Conversion TypedValue) where
  toLlvm (Conversion op tv t) = hsep [toLlvm op, toLlvm tv, text "to", toLlvm t]
  
instance AsmPrint (Select TypedValue) where
  toLlvm (Select c t f) = hsep [text "select", toLlvm c <> comma, toLlvm t <> comma, toLlvm f]

instance AsmPrint Expr where
  toLlvm (EgEp a) = toLlvm a
  toLlvm (EiC a) = toLlvm a
  toLlvm (EfC a) = toLlvm a
  toLlvm (Eb a) = toLlvm a
  toLlvm (Ec a) = toLlvm a
  toLlvm (Es a) = toLlvm a

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
  toLlvm (VgOl i) = toLlvm i
  toLlvm (Ve e) = toLlvm e
  toLlvm (Vc c) = toLlvm c

instance AsmPrint TypedValue where
  toLlvm (TypedValue t v) = toLlvm t <+> toLlvm v

instance AsmPrint Pointer where
  toLlvm (Pointer i) = toLlvm i

instance AsmPrint TypedPointer where
  toLlvm (TypedPointer t v) = toLlvm t <+> toLlvm v
  
instance AsmPrint TypedConst where  
  toLlvm (TypedConst t c) = toLlvm t <+> toLlvm c
  toLlvm TypedConstNull = text "null"

instance AsmPrint FunName where
  toLlvm (FunNameGlobal s) = toLlvm s
  toLlvm (FunNameString s) = text s
  
instance AsmPrint CallSite where
  toLlvm (CsFun cc ra rt ident params fa) = 
    hsep [toLlvm cc, hsep $ fmap toLlvm ra, toLlvm rt, toLlvm ident, parens (commaSepList $ fmap toLlvm params), hsep $ fmap toLlvm fa]
  toLlvm (CsAsm t se as dia s1 s2 params fa) = 
    hsep [toLlvm t, text "asm", toLlvm se, toLlvm as, toLlvm dia, toLlvm s1 <> comma, toLlvm s2, parens (commaSepList $ fmap toLlvm params), hsep $ fmap toLlvm fa]
  toLlvm (CsConversion ra t convert params fa) = 
    hsep [hsep $ fmap toLlvm ra, toLlvm t, toLlvm convert, parens (commaSepList $ fmap toLlvm params), hsep $ fmap toLlvm fa]

instance AsmPrint Clause where
  toLlvm (Catch tv) = text "catch" <+> toLlvm tv
  toLlvm (Filter tc) = text "filter" <+> toLlvm tc
  toLlvm (Cco c) = toLlvm c

instance AsmPrint (Conversion (Type, GlobalOrLocalId)) where
  toLlvm (Conversion op (t,g) dt) = toLlvm op <+> parens (hsep [toLlvm t, toLlvm g, text "to", toLlvm dt])
  
instance AsmPrint PersFn where
    toLlvm (PersFnId g) = toLlvm g
    toLlvm (PersFnCast c) = toLlvm c
    toLlvm PersFnUndef = text "undef"
    toLlvm PersFnNull = text "null"
    toLlvm (PersFnConst c) = toLlvm c

instance AsmPrint (ExtractElem TypedValue) where
  toLlvm (ExtractElem tv1 tv2) = 
    text "extractelement" <+> (commaSepList [toLlvm tv1, toLlvm tv2])
  
instance AsmPrint (InsertElem TypedValue) where  
  toLlvm (InsertElem vect tv idx) = 
    text "insertelement" <+> (commaSepList [toLlvm vect, toLlvm tv, toLlvm idx])
  
instance AsmPrint (ShuffleVector TypedValue) where  
  toLlvm (ShuffleVector vect1 vect2 mask) = 
    text "shufflevector" <+> (commaSepList [toLlvm vect1, toLlvm vect2, toLlvm mask])
  
instance AsmPrint (ExtractValue TypedValue) where  
  toLlvm (ExtractValue tv idxs) = text "extractvalue" <+> (commaSepList ((toLlvm tv):(fmap text idxs)))
  
instance AsmPrint (InsertValue TypedValue) where  
  toLlvm (InsertValue vect tv idxs) = 
    text "insertvalue" <+> (commaSepList ((toLlvm vect):(toLlvm tv):(fmap text idxs)))


instance AsmPrint Rhs where
  toLlvm (RmO a) = toLlvm a
  toLlvm (Re a) = toLlvm a
  toLlvm (Call tailc callSite) = hsep [toLlvm tailc, text "call", toLlvm callSite]
  toLlvm (ReE a) = toLlvm a
  toLlvm (RiE a) = toLlvm a
  toLlvm (RsV a) = toLlvm a
  toLlvm (ReV a) = toLlvm a
  toLlvm (RiV a) = toLlvm a
  toLlvm (VaArg tv t) = hsep [text "va_arg", toLlvm tv <> comma, toLlvm t]
  toLlvm (LandingPad rt pt tgl b clause) = 
    hsep ([text "landingpad", toLlvm rt, text "personality", toLlvm pt, toLlvm tgl, toLlvm b] ++ (fmap toLlvm clause))

instance AsmPrint ActualParam where
  toLlvm (ActualParam t att1 align v att2) = 
    hsep [toLlvm t, hsep $ fmap toLlvm att1, toLlvm align, toLlvm v, hsep $ fmap toLlvm att2]

instance AsmPrint Dbg where
  toLlvm (Dbg mv meta) = toLlvm mv <+> toLlvm meta

instance AsmPrint PhiInst where
  toLlvm (PhiInst lhs t pairs) =  hsep [maybe empty ((<+> equals) . toLlvm) lhs, text "phi", toLlvm t, commaSepList $ fmap tvToLLvm pairs]
    where tvToLLvm (h1,h2) = brackets (toLlvm h1 <> comma <+> toLlvm h2)

instance AsmPrint ComputingInst where
  toLlvm (ComputingInst lhs rhs) = maybe empty ((<+> equals) . toLlvm) lhs <+> toLlvm rhs

instance AsmPrint TerminatorInst where
  toLlvm (Return x) = text "ret" <+> (if null x then text "void" else commaSepList $ fmap toLlvm x)
  toLlvm (Br a) = text "br" <+> toLlvm a
  toLlvm (Cbr v t f) = hsep [text "br",  text "i1", toLlvm v <> comma, toLlvm t <> comma, toLlvm f]
  toLlvm (IndirectBr v l) = hsep [text "indirectbr", toLlvm v <> comma, brackets (commaSepList $ fmap toLlvm l)]
  toLlvm (Switch v d tbl) = 
    hsep [text "switch", toLlvm v <> comma, toLlvm d, brackets (hsep $ fmap (\(p1,p2) -> toLlvm p1 <> comma <+> toLlvm p2) tbl)]
  toLlvm (Invoke lhs callSite toL unwindL) = 
    hsep [maybe empty ((<+> equals) . toLlvm) lhs, text "invoke", toLlvm callSite, text "to", toLlvm toL, text "unwind", toLlvm unwindL]
  toLlvm Unreachable = text "unreachable"
  toLlvm (Resume a) = text "resume" <+> toLlvm a
             
instance AsmPrint PhiInstWithDbg where
  toLlvm (PhiInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)

instance AsmPrint TerminatorInstWithDbg where
  toLlvm (TerminatorInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)

instance AsmPrint ComputingInstWithDbg where
  toLlvm (ComputingInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)

instance AsmPrint Aliasee where
  toLlvm (AtV tv ) = toLlvm tv
  toLlvm (Ac c) = toLlvm c
  toLlvm (AgEp a) = toLlvm a

instance AsmPrint FunctionPrototype where
  toLlvm (FunctionPrototype fhLinkage fhVisibility fhCCoonc fhAttr fhRetType fhName fhParams fnd 
          fhAttr1 fhSection fhcmd fhAlign fhGc fhPrefix fhPrologue) = 
    hsep [toLlvm fhLinkage, toLlvm fhVisibility, toLlvm fhCCoonc, hsep $ fmap toLlvm fhAttr 
         , toLlvm fhRetType, toLlvm fhName, toLlvm fhParams, toLlvm fnd, hsep $ fmap toLlvm fhAttr1
         , toLlvm fhSection, toLlvm fhcmd, toLlvm fhAlign, toLlvm fhGc, toLlvm fhPrefix, toLlvm fhPrologue]

instance AsmPrint Block where
  toLlvm (Block lbl phis ins end) = toLlvm lbl $$
                                    (nest 2 
                                     ((vcat $ fmap toLlvm phis) $$
                                      (vcat $ fmap toLlvm ins) $$
                                      toLlvm end))

instance AsmPrint Toplevel where 
  toLlvm (ToplevelTriple s) = hsep [text "target", text "triple", equals, toLlvm s]
  toLlvm (ToplevelDataLayout s) = hsep [text "target", text "datalayout", equals, toLlvm s]
  toLlvm (ToplevelAlias lhs vis dll tlm naddr link aliasee) = 
    hsep [toLlvm lhs, equals, toLlvm vis, toLlvm dll, toLlvm tlm, toLlvm naddr, text "alias", toLlvm link, toLlvm aliasee]
  toLlvm (ToplevelDbgInit s i) = error "DbgInit is not implemented"
  toLlvm (ToplevelStandaloneMd s t) = char '!' <> (text s) <+> equals <+> toLlvm t
  toLlvm (ToplevelNamedMd mv nds) = toLlvm mv <+> equals <+> char '!'<>(braces (commaSepList $ fmap toLlvm nds))
  toLlvm (ToplevelDeclare fproto) = text "declare" <+> toLlvm fproto
  toLlvm (ToplevelDefine fproto blocks) = text "define" <+> toLlvm fproto <+> text "{" $$
                                          (fcat $ fmap toLlvm blocks) $$ text "}"
  toLlvm (ToplevelGlobal lhs linkage vis dllStor threadLoc un addrspace externali gty ty const0 
          section comdat align) = 
    hsep [maybeSepByEquals lhs, toLlvm linkage, toLlvm vis, toLlvm dllStor, toLlvm threadLoc, toLlvm un
         , toLlvm addrspace, toLlvm externali, toLlvm gty, toLlvm ty, toLlvm const0]
    <> (hcat [commaSepMaybe section, commaSepMaybe comdat, commaSepMaybe align])

  toLlvm (ToplevelTypeDef n t) = toLlvm n <+> equals <+> text "type" <+> toLlvm t
  toLlvm (ToplevelDepLibs l) = text "deplibs" <+> equals <+> brackets (commaSepList $ fmap toLlvm l)
  toLlvm (ToplevelUnamedType x t) = text "type" <+> toLlvm t <+> text "; " <+> (integer x)
  toLlvm (ToplevelModuleAsm qs) = text "module asm" <+> toLlvm qs
  toLlvm (ToplevelAttribute n l) = 
    hsep [text "attributes", char '#' <> (integer n), equals, braces (hsep $ fmap toLlvm l)]
  toLlvm (ToplevelComdat l c) = 
    hsep [toLlvm l, equals, text "comdat", toLlvm c]

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

instance AsmPrint ConvertOp where
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

instance AsmPrint TypePrimitive where
  toLlvm = P.print

instance AsmPrint Lstring where
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
    
instance AsmPrint AddrSpace where
  toLlvm = P.print
    
instance AsmPrint Type where
  toLlvm = P.print

instance AsmPrint Fparam where
  toLlvm = P.print
  
instance AsmPrint FormalParam where
  toLlvm = P.print

instance AsmPrint FormalParamList where
  toLlvm = P.print

instance AsmPrint TypeParamList where
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
