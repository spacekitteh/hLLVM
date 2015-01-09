{-# LANGUAGE FlexibleInstances #-}
module Llvm.VmCore.AstWriter where
import Data.List
import Llvm.VmCore.Ast
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.SharedEntityWriter
import Llvm.VmCore.DataLayoutWriter


instance AsmWriter LabelId where
  toLlvm (LabelString s) = toLlvm s 
  toLlvm (LabelNumber n) = integer n
  toLlvm (LabelQuoteNumber n) = doubleQuotes $ integer n
  toLlvm (LabelQuoteString s) = doubleQuotes $ toLlvm s

instance AsmWriter PercentLabel where
  toLlvm (PercentLabel li) = char '%' <> (toLlvm li)
  
instance AsmWriter TargetLabel where
  toLlvm (TargetLabel id) = text "label" <+> toLlvm id
  
instance AsmWriter BlockLabel where
  toLlvm (ImplicitBlockLabel) = text ";<label>:?"
  toLlvm (ExplicitBlockLabel l) = toLlvm l <> char ':'

instance AsmWriter ComplexConstant where
  toLlvm (Cstruct b ts) = let (start, end) = if b then (char '<', char '>') else (empty, empty)
                          in start <> braces (commaSepList $ fmap toLlvm ts) <> end
  toLlvm (Cvector ts) = char '<' <+> (commaSepList $ fmap toLlvm ts) <+> char '>'
  toLlvm (Carray ts) = brackets $ commaSepList $ fmap toLlvm ts

instance AsmWriter IbinaryOperator where
  toLlvm Add = text "add"
  toLlvm Sub = text "sub"
  toLlvm Mul = text "mul"
  toLlvm Udiv = text "udiv"
  toLlvm Sdiv = text "sdiv"
  toLlvm Urem = text "urem"
  toLlvm Srem = text "srem"
  toLlvm Shl = text "shl"
  toLlvm Lshr = text "lshr"
  toLlvm Ashr = text "ashr"
  toLlvm And = text "and"
  toLlvm Or = text "or"
  toLlvm Xor = text "xor"


instance AsmWriter FbinaryOperator where
  toLlvm Fadd = text "fadd"
  toLlvm Fsub = text "fsub"
  toLlvm Fmul = text "fmul"
  toLlvm Fdiv = text "fdiv"
  toLlvm Frem = text "frem"

instance AsmWriter (BinExpr Const) where
  toLlvm (Ie v) = toLlvm v
  toLlvm (Fe v) = toLlvm v

instance AsmWriter (BinExpr Value) where
  toLlvm (Ie v) = toLlvm v
  toLlvm (Fe v) = toLlvm v

instance AsmWriter (IbinExpr Const) where
  toLlvm (IbinExpr op cs t c1 c2) = toLlvm op 
                                    <+> (hsep $ fmap toLlvm cs)
                                    <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))
  
instance AsmWriter (FbinExpr Const) where
  toLlvm (FbinExpr op cs t c1 c2) = toLlvm op 
                                    <+> toLlvm cs
                                    <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))


instance AsmWriter (Conversion TypedConst) where
  toLlvm (Conversion op tc t) = toLlvm op <+> parens (toLlvm tc <+> text "to" <+> toLlvm t)


instance AsmWriter (GetElemPtr TypedConst) where
  toLlvm (GetElemPtr b base indices) = 
    text "getelementptr" <+> toLlvm b <+> parens (commaSepList ((toLlvm base):fmap toLlvm indices))

instance AsmWriter (Select TypedConst) where
  toLlvm (Select cnd tc1 tc2) = 
    text "select" <+> parens (toLlvm cnd <> comma <+> toLlvm tc1 <> comma <+> toLlvm tc2)


instance AsmWriter (Icmp Const) where
  toLlvm (Icmp op t c1 c2) = 
    text "icmp" <+> toLlvm op <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))

instance AsmWriter (Fcmp Const) where
  toLlvm (Fcmp op t c1 c2) = 
    text "fcmp" <+> toLlvm op <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))
  

instance AsmWriter (ShuffleVector TypedConst) where
  toLlvm (ShuffleVector tc1 tc2 mask) = 
    text "shufflevector" <+> parens (toLlvm tc1 <> comma <+> toLlvm tc2 <+> comma <+> toLlvm mask)


instance AsmWriter (ExtractValue TypedConst) where
  toLlvm (ExtractValue tc indices) = 
    text "extractvalue" <+> parens (commaSepList ((toLlvm tc):fmap text indices))
                                   
instance AsmWriter (InsertValue TypedConst) where
  toLlvm (InsertValue vect tc indices) = 
    text "insertvalue" <+> parens (commaSepList ((toLlvm vect):(toLlvm tc):fmap text indices)) 
                                       
instance AsmWriter (ExtractElem TypedConst) where
  toLlvm (ExtractElem tc index) = 
    text "extractelement" <+> parens (toLlvm tc <> comma <+> toLlvm index)
                                
                                  
instance AsmWriter (InsertElem TypedConst) where                                  
  toLlvm (InsertElem tc1 tc2 index) = 
    text "insertelement" <+> parens (toLlvm tc1 <> comma <+> toLlvm tc2)
    

instance AsmWriter TrapFlag where
  toLlvm Nuw = text "nuw"
  toLlvm Nsw = text "nsw"
  toLlvm Exact = text "exact"
                                      
instance AsmWriter Const where
  toLlvm x = case x of
    Ccp c -> toLlvm c
    Cca a -> toLlvm a
    Cl l -> toLlvm l
    CblockAddress g a -> text "blockaddress" <+> parens (toLlvm g <> comma <+> toLlvm a)
    -- Ca a -> toLlvm a
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
    CmL id -> toLlvm id

instance AsmWriter MdVar where 
  toLlvm (MdVar s) = char '!'<> (text s)
  
instance AsmWriter MdNode where
  toLlvm (MdNode s) = char '!' <> (text s)
  
instance AsmWriter MetaConst where
  toLlvm (MdConst c) = char '!' <> (toLlvm c)
  toLlvm (MdString s) = char '!' <> (toLlvm s)
  toLlvm (McMn n) = toLlvm n
  toLlvm (McMv v) = toLlvm v
  toLlvm (MdRef s) = text $ show s
                        
instance AsmWriter (GetElemPtr TypedValue) where
  toLlvm (GetElemPtr ib tv tcs) = 
    text "getelementptr" <+> toLlvm ib <+> (commaSepList ((toLlvm tv):fmap toLlvm tcs))
  
instance AsmWriter (IbinExpr Value) where
  toLlvm (IbinExpr op cs t v1 v2) = 
    toLlvm op <+> listToDoc toLlvm cs (<+>) 
    <+> toLlvm t 
    <+> toLlvm v1 
    <+> comma <+> toLlvm v2
  
instance AsmWriter (FbinExpr Value) where
  toLlvm (FbinExpr op cs t v1 v2) = 
    toLlvm op <+> toLlvm cs
    <+> toLlvm t 
    <+> toLlvm v1 
    <+> comma <+> toLlvm v2


instance AsmWriter (Icmp Value) where  
  toLlvm (Icmp op t v1 v2) = 
    text "icmp" <+> toLlvm op <+> toLlvm t <+> toLlvm v1 <> comma <+> toLlvm v2

  
instance AsmWriter (Fcmp Value) where  
  toLlvm (Fcmp op t v1 v2) = 
    text "fcmp" <+> toLlvm op <+> toLlvm t <+> toLlvm v1 <> comma <+> toLlvm v2
  
{-
instance AsmWriter (Bitwise Value) where  
  toLlvm (Bitwise op l t v1 v2) = toLlvm op 
                                  ++ listToLlvm " " l " " ""
                                  ++ " " ++ toLlvm t ++ " " 
                                  ++ toLlvm v1 ++ ", " ++ toLlvm v2  
-}

instance AsmWriter (Conversion TypedValue) where  
  toLlvm (Conversion op tv t) = toLlvm op <+> toLlvm tv <+> (text "to") <+> toLlvm t
  
instance AsmWriter (Select TypedValue) where  
  toLlvm (Select c t f) = text "select" <+> toLlvm c <> comma <+> toLlvm t <> comma <+> toLlvm f

instance AsmWriter Expr where
  toLlvm (EgEp a) = toLlvm a
--  toLlvm (Ea a) = toLlvm a
  toLlvm (EiC a) = toLlvm a
  toLlvm (EfC a) = toLlvm a
  toLlvm (Eb a) = toLlvm a
  toLlvm (Ec a) = toLlvm a
  toLlvm (Es a) = toLlvm a

  {-
instance AsmWriter MemSize where
  toLlvm (MemSize t size) = toLlvm t ++ sepOptToLlvm ", " size ++ sepOptToLlvm ", " a
-}
  
instance AsmWriter MemArea where  
  toLlvm OnStack = text "alloca"
  toLlvm InHeap = text "malloc"
  
{-  
instance AsmWriter (AddrAttr l) where
  toLlvm (AddrAttr ptr align) = toLlvm ptr ++ sepOptToLlvm ", " align
-}



-- stOrdToLlvm st ord = (if st then text "singlethread" else empty) <+> sepOptToLlvm empty ord

instance AsmWriter MemOp where  
  toLlvm (Alloca ma t s a) = text "alloca" <+> toLlvm ma <+> toLlvm t <+> sepOptToLlvm comma s <+> sepOptToLlvm comma a
--  toLlvm (Free tv) = "free " ++ toLlvm tv
  toLlvm (Load b ptr align noterm inv nonul) = text "load" <+> toLlvm b <+> toLlvm ptr <> sepOptToLlvm comma align 
                                               <> sepOptToLlvm comma noterm
                                               <> sepOptToLlvm comma inv
                                               <> sepOptToLlvm comma nonul
  toLlvm (LoadAtomic (Atomicity st ord) b ptr align) = 
    text "load" <+> text "atomic"
    <+> toLlvm b
    <+> toLlvm ptr 
    <+> toLlvm st
    <+> toLlvm ord
    <> sepOptToLlvm comma align
  toLlvm (Store b v addr align noterm) = text "store" <+> toLlvm b
                                  <+> toLlvm v <> comma <+> toLlvm addr <> sepOptToLlvm comma align
  toLlvm (StoreAtomic (Atomicity st ord) b v ptr align) = text "store" <+> text "atomic" 
                                                          <+> toLlvm b 
                                                          <+> toLlvm v <> comma
                                                          <+> toLlvm ptr 
                                                          <+> toLlvm st
                                                          <+> toLlvm ord 
                                                          <+> sepOptToLlvm comma align
  toLlvm (Fence b order) = text "fence" <+> toLlvm b <+> toLlvm order
  toLlvm (CmpXchg wk v p c n st sord ford) = text "cmpxchg" <+> toLlvm wk
                                             <+> toLlvm v
                                             <+> toLlvm p <> comma
                                             <+> toLlvm c <> comma
                                             <+> toLlvm n 
                                             <+> toLlvm st
                                             <+> toLlvm sord
                                             <+> toLlvm ford
  toLlvm (AtomicRmw v op p vl st ord) = text "atomicrmw" <+> toLlvm v
                                        <+> toLlvm op 
                                        <+> toLlvm p <+> comma
                                        <+> toLlvm vl 
                                        <+> toLlvm st
                                        <+> toLlvm ord
                                        

instance AsmWriter Value where
  toLlvm (VgOl i) = toLlvm i
  toLlvm (Ve e) = toLlvm e
  toLlvm (Vc c) = toLlvm c
  -- toLlvm (Vr r) = "" -- toLlvm r
--  toLlvm (ViA i) = toLlvm i
  toLlvm (InlineAsm se as s1 s2) = text "asm" 
                                   <+> (if se then text "sideeffect" else empty) 
                                   <+> (if as then text "alignstack" else empty)
                                   <+> text s1 <> comma <+> text s2     


instance AsmWriter TypedValue where
  toLlvm (TypedValue t v) = toLlvm t <+> toLlvm v

instance AsmWriter Pointer where
  toLlvm (Pointer i) = toLlvm i

instance AsmWriter TypedPointer where
  toLlvm (TypedPointer t v) = toLlvm t <+> toLlvm v
  
instance AsmWriter TypedConst where  
  toLlvm (TypedConst t c) = toLlvm t <+> toLlvm c
  toLlvm TypedConstNull = text "null"

instance AsmWriter FunName where
  toLlvm (FunNameGlobal s) = toLlvm s
  toLlvm (FunNameString s) = text s
  
instance AsmWriter CallSite where
  toLlvm (CallFun cc ra rt ident params fa) = optSepToLlvm cc empty <+> (hsep $ fmap toLlvm ra)
                                              <+> toLlvm rt <+> toLlvm ident
                                              <+> parens (commaSepList $ fmap toLlvm params)
                                              <+> toLlvm fa
  toLlvm (CallAsm t se as dia s1 s2 params fa) = (toLlvm t) <+> text "asm"
                                                 <+> (if se then text "sideeffect" else empty)
                                                 <+> (if as then text "alignstack" else empty)
                                                 <+> toLlvm dia
                                                 <+> toLlvm s1 <> comma <+> toLlvm s2
                                                 <+> parens (commaSepList $ fmap toLlvm params)
                                                 <+> toLlvm fa
  toLlvm (CallConversion ra t convert params fa) = (hsep $ fmap toLlvm ra)
                                                <+> toLlvm t 
                                                <+> toLlvm convert 
                                                <+> parens (commaSepList $ fmap toLlvm params)
                                                <+> (toLlvm fa)

instance AsmWriter Clause where
  toLlvm (Catch tv) = text "catch" <+> toLlvm tv
  toLlvm (Filter tc) = text "filter" <+> toLlvm tc
  toLlvm (Cco c) = toLlvm c

instance AsmWriter (Conversion (Type, GlobalOrLocalId)) where
  toLlvm (Conversion op (t,g) dt) = toLlvm op <+> parens (toLlvm t <+> toLlvm g <+> text "to" <+> toLlvm dt)
  
instance AsmWriter PersFn where
    toLlvm (PersFnId g) = toLlvm g
    toLlvm (PersFnCast c) = toLlvm c
    toLlvm PersFnUndef = text "undef"
    toLlvm PersFnNull = text "null"

instance AsmWriter (ExtractElem TypedValue) where
  toLlvm (ExtractElem tv1 tv2) = 
    text "extractelement" <+> (commaSepList [toLlvm tv1, toLlvm tv2])
  
instance AsmWriter (InsertElem TypedValue) where  
  toLlvm (InsertElem vect tv idx) = 
    text "insertelement" <+> (commaSepList [toLlvm vect, toLlvm tv, toLlvm idx])
  
instance AsmWriter (ShuffleVector TypedValue) where  
  toLlvm (ShuffleVector vect1 vect2 mask) = 
    text "shufflevector" <+> (commaSepList [toLlvm vect1, toLlvm vect2, toLlvm mask])
  
instance AsmWriter (ExtractValue TypedValue) where  
  toLlvm (ExtractValue tv idxs) = text "extractvalue" <+> (commaSepList ((toLlvm tv):(fmap text idxs)))
  
instance AsmWriter (InsertValue TypedValue) where  
  toLlvm (InsertValue vect tv idxs) = 
    text "insertvalue" <+> (commaSepList ((toLlvm vect):(toLlvm tv):(fmap text idxs)))


instance AsmWriter Rhs where
  toLlvm (RmO a) = toLlvm a
  toLlvm (Re a) = toLlvm a
  toLlvm (Call tail callSite) = toLlvm tail <+> text "call" <+> toLlvm callSite
  toLlvm (ReE a) = toLlvm a
  toLlvm (RiE a) = toLlvm a
  toLlvm (RsV a) = toLlvm a
  toLlvm (ReV a) = toLlvm a
  toLlvm (RiV a) = toLlvm a
  toLlvm (VaArg tv t) = text "va_arg" <+> toLlvm tv <+> comma <+> toLlvm t    
  toLlvm (LandingPad rt pt tgl b clause) =  
    text "landingpad" <+> toLlvm rt
    <+> text "personality" 
    <+> (toLlvm pt) 
    <+> (toLlvm tgl)
    <+> (if b then text "cleanup" else empty)
    <+> listToDoc toLlvm clause (<+>)
  

instance AsmWriter ActualParam where
  toLlvm (ActualParam t att1 align v att2) = toLlvm t <+> listToDoc toLlvm att1 (<+>)
                                             <+> optSepToLlvm align empty
                                             <+> toLlvm v <+> listToDoc toLlvm att2 (<+>)

instance AsmWriter Dbg where
  toLlvm (Dbg mv meta) = toLlvm mv <+> toLlvm meta

instance AsmWriter PhiInst where
  toLlvm (PhiInst lhs t pairs) =  optSepToLlvm lhs equals <+> text "phi"  
                                  <+> toLlvm t <+> (commaSepList $ fmap tvToLLvm pairs)
    where tvToLLvm (h1,h2) = brackets (toLlvm h1 <+> comma <+> toLlvm h2)

instance AsmWriter ComputingInst where
  toLlvm (ComputingInst lhs rhs) = optSepToLlvm lhs (text "=") <+> toLlvm rhs
                          

instance AsmWriter TerminatorInst where
  toLlvm (Return x) = text "ret" <+> (if null x then text "void" else commaSepList $ fmap toLlvm x)
  toLlvm (Br a) = text "br" <+> toLlvm a
  toLlvm (Cbr v t f) = text "br i1" <+> toLlvm v <+> comma <+> toLlvm t <+> comma <+> toLlvm f
  toLlvm (IndirectBr v l) = text "indirectbr" <+> toLlvm v <+> comma <+> brackets (commaSepList $ fmap toLlvm l)
  toLlvm (Switch v d tbl) = text "switch" <+> toLlvm v <+> comma <+> toLlvm d <+> 
                            brackets (listToDoc (\(p1,p2) -> toLlvm p1 <+> comma <+> toLlvm p2) tbl (<+>))
  toLlvm (Invoke lhs callSite toL unwindL) = optSepToLlvm lhs (text "=") <+>
                                             (text "invoke") <+> (toLlvm callSite) <+>
                                             (text "to") <+> (toLlvm toL) <+>
                                             (text "unwind") <+> toLlvm unwindL
  toLlvm Unreachable = text "unreachable"
  toLlvm (Resume a) = text "resume" <+> toLlvm a
             
instance AsmWriter TerminatorInstWithDbg where
  toLlvm (TerminatorInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)


instance AsmWriter ComputingInstWithDbg where
  toLlvm (ComputingInstWithDbg ins dbgs) = commaSepList ((toLlvm ins):fmap toLlvm dbgs)


instance AsmWriter Aliasee where
  toLlvm (AtV tv ) = toLlvm tv
  toLlvm (Ac c) = toLlvm c
  toLlvm (AgEp a) = toLlvm a
                      

instance AsmWriter FunctionPrototype where
  toLlvm (FunctionPrototype fhLinkage fhVisibility
          fhCCoonc fhAttr fhRetType fhName fhParams fnd fhAttr1 fhSection
          fhcmd fhAlign fhGc fhPrefix fhPrologue) = 
    optSepToLlvm fhLinkage empty <+> optSepToLlvm fhVisibility empty <+>
    optSepToLlvm fhCCoonc empty <+> (listToDoc toLlvm fhAttr (<+>)) <+>
    toLlvm fhRetType <+> toLlvm fhName <+> toLlvm fhParams 
    <+> (maybe empty toLlvm) fnd
    <+> (toLlvm fhAttr1) 
    <+> sepOptToLlvm empty fhSection 
    <+> sepOptToLlvm empty fhcmd
    <+> sepOptToLlvm empty fhAlign 
    <+> sepOptToLlvm empty fhGc
    <+> (maybe empty toLlvm) fhPrefix
    <+> (maybe empty toLlvm) fhPrologue

instance AsmWriter Block where
  toLlvm (Block lbl phis ins end) = toLlvm lbl $$
                                    (nest 2 
                                     ((vcat $ fmap toLlvm phis) $$
                                      (vcat $ fmap toLlvm ins) $$
                                      toLlvm end))

instance AsmWriter Toplevel where 
  toLlvm (ToplevelTriple s) = text "target" <+> text "triple" <+> equals <+> toLlvm s
  toLlvm (ToplevelDataLayout s) = text "target" <+> text "datalayout" <+> equals <+> toLlvm s
  toLlvm (ToplevelAlias lhs vis dll tlm naddr link aliasee) = 
    toLlvm lhs <+>  text "=" <+> optSepToLlvm vis empty <+>
    optSepToLlvm dll empty <+>
    optSepToLlvm tlm empty <+>
    toLlvm naddr <+>
    text "alias" <+> optSepToLlvm link empty <+>
    toLlvm aliasee
  toLlvm (ToplevelDbgInit s i) = error "DbgInit is not implemented"
  toLlvm (ToplevelStandaloneMd s t) = char '!' <> (text s) <+> equals <+> toLlvm t
  toLlvm (ToplevelNamedMd mv nds) = toLlvm mv <+> equals <+> char '!'<>(braces (commaSepList $ fmap toLlvm nds))
  toLlvm (ToplevelDeclare fproto) = text "declare" <+> toLlvm fproto
  toLlvm (ToplevelDefine fproto blocks) = text "define" <+> toLlvm fproto <+> text "{" $$
                                          (fcat $ fmap toLlvm blocks) $$ text "}"
  toLlvm (ToplevelGlobal lhs linkage vis dllStor threadLoc un addrspace externali gty ty const 
          section comdat align)
                              = optSepToLlvm lhs equals <+>
                                optSepToLlvm linkage empty <+>
                                optSepToLlvm vis empty <+>
                                optSepToLlvm dllStor empty <+>
                                optSepToLlvm threadLoc empty <+>
                                toLlvm un <+>
                                optSepToLlvm addrspace empty <+>
                                toLlvm externali <+>
                                toLlvm gty <+>
                                toLlvm ty <+> 
                                sepOptToLlvm empty const <+>
                                sepOptToLlvm comma section <+> 
                                sepOptToLlvm comma comdat <+>
                                sepOptToLlvm comma align

  toLlvm (ToplevelTypeDef n t) = toLlvm n <+> equals <+> text "type" <+> toLlvm t
  toLlvm (ToplevelDepLibs l) = text "deplibs" <+> equals <+> brackets (commaSepList $ fmap toLlvm l)
  toLlvm (ToplevelUnamedType id t) = text "type" <+> toLlvm t <+> text "; " <+> (integer id)
  toLlvm (ToplevelModuleAsm qs) = text "module asm" <+> toLlvm qs
  toLlvm (ToplevelAttribute n l) = text "attributes" <+> char '#' <> (integer n) 
                                   <+> equals <+> braces (hsep $ fmap toLlvm l)
  toLlvm (ToplevelComdat l c) = toLlvm l <+> equals <+> text "comdat" <+> toLlvm c



instance AsmWriter Module where 
  toLlvm (Module tops) = vcat $ fmap toLlvm tops

instance AsmWriter Prefix where
  toLlvm (Prefix n) = text "prefix" <+> toLlvm n
  
  
instance AsmWriter Prologue where  
  toLlvm (Prologue n) = text "prologue" <+> toLlvm n
