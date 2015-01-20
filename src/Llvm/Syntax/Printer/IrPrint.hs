{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Llvm.Syntax.Printer.IrPrint 
       (module Llvm.Syntax.Printer.IrPrint
       ,module Llvm.Syntax.Printer.Common
       ) where

import Llvm.Data.Ir

import Llvm.Syntax.Printer.Common
import qualified Llvm.Syntax.Printer.SharedEntityPrint as P
import qualified Compiler.Hoopl as H

class IrPrint a where
  printIr :: a -> Doc
  
  
commaSepMaybe :: IrPrint a => Maybe a -> Doc
commaSepMaybe = maybe empty ((comma <+>) . printIr)


optSepToLlvm :: IrPrint a => Maybe a -> Doc -> Doc
optSepToLlvm (Nothing) _ = empty
optSepToLlvm (Just x) sep = printIr x <+> sep

instance IrPrint a => IrPrint (Maybe a) where
  printIr (Just x) = printIr x
  printIr Nothing = empty

instance IrPrint Toplevel where 
  printIr (ToplevelTriple s) = hsep [text "target", text "triple", equals, printIr s]
  printIr (ToplevelDataLayout s) = hsep [text "target", text "datalayout", equals, printIr s]
  printIr (ToplevelAlias lhs vis dll tlm na link aliasee) = 
    hsep [printIr lhs, equals, printIr vis, printIr dll, printIr tlm, printIr na, text "alias", printIr link, printIr aliasee]
  printIr (ToplevelDbgInit s i) = hsep [text "dbginit", text s, integer i]
  printIr (ToplevelStandaloneMd s t) = char '!'<>(text s) <+> equals <+> printIr t
  printIr (ToplevelNamedMd mv nds) = printIr mv <+> equals <+> char '!'<>(braces (hsep $ punctuate comma $ fmap printIr nds))
  printIr (ToplevelDeclare fproto) = text "declare" <+> printIr fproto
  printIr (ToplevelDefine fproto entry graph) = text "define" <+> printIr fproto $$
                                               text "; the entry label is" <+> 
                                               printIr entry $$
                                               printIr graph
  printIr (ToplevelGlobal lhs linkage vis dll tlm un addrspace externali gty ty c section comdat align) = 
    hsep [optSepToLlvm lhs equals, printIr linkage, printIr vis, printIr dll, printIr tlm, printIr un, printIr addrspace
         , printIr externali, printIr gty, printIr ty, printIr c, commaSepNonEmpty [printIr section, printIr comdat, printIr align]]
  printIr (ToplevelTypeDef n t) = hsep [printIr n, equals, text "type", printIr t]
  printIr (ToplevelDepLibs l) = hsep [text "deplibs", equals, brackets (hsep $ punctuate comma $ fmap printIr l)]
  printIr (ToplevelUnamedType x t) = text "type" <+> printIr t <+> text("; " ++ (show x))
  printIr (ToplevelModuleAsm qs) = hsep [text "module", text "asm", printIr qs]
  printIr (ToplevelAttribute n l) = 
    hsep [text "attributes", char '#' <> (integer n), braces (hsep $ fmap printIr l)]
  printIr (ToplevelComdat l s) = hsep [printIr l, equals, printIr s]


instance IrPrint Module where 
  printIr (Module tops) = fcat $ fmap printIr tops


instance IrPrint (Node e x) where
  printIr (Nlabel lbl) = printIr lbl
  printIr (Pinst i) = printIr i
  printIr (Cinst c) = printIr c
  printIr (Tinst t) = printIr t


instance IrPrint (H.Graph Node H.C H.C) where
  printIr g = braces (H.foldGraphNodes (\n -> \s -> s $$ (printIr n)) g empty)

instance IrPrint Label where
  printIr l = text (show l) 
  
instance IrPrint LabelId where
  printIr (LabelString l) = printIr l
  printIr (LabelNumber l) = printIr l
  printIr (LabelDqNumber l) = printIr l
  printIr (LabelDqString l) = printIr l

instance IrPrint PercentLabel where
  printIr (PercentLabel li) = char '%' <> (printIr li)
  
instance IrPrint TargetLabel where
  printIr (TargetLabel x) = text "label" <+> printIr x
  

instance IrPrint BlockLabel where
  printIr (BlockLabel l) = printIr l <> char ':'

instance IrPrint ComplexConstant where
  printIr (Cstruct b ts) = let v = (braces (commaSepList $ fmap printIr ts))
                           in case b of 
                             Packed -> angleBrackets v
                             Unpacked -> v
  printIr (Cvector ts) = angleBrackets (commaSepList $ fmap printIr ts)
  printIr (Carray ts) = brackets (commaSepList $ fmap printIr ts)


instance IrPrint Exact where
  printIr Exact = text "exact"

instance IrPrint NoWrap where
    printIr Nsw = text "nsw"
    printIr Nuw = text "nuw"
    printIr Nsuw = text "nsw nuw"


instance IrPrint (IbinExpr Const) where  
  printIr e = let op = operatorOfIbinExpr e
                  (c1, c2) = operandOfIbinExpr e
                  t = typeOfIbinExpr e
                  cs = case e of
                        Add x _ _ _ -> printIr x
                        Sub x _ _ _ -> printIr x
                        Udiv x _ _ _ -> printIr x
                        Sdiv x _ _ _ -> printIr x
                        Shl x _ _ _ -> printIr x
                        Lshr x _ _ _ -> printIr x
                        Ashr x _ _ _ -> printIr x
                        _ -> empty
              in text op <+> cs <+> parens (printIr (TypedConst t c1) <> comma <+> printIr (TypedConst t c2))


instance IrPrint (FbinExpr Const) where  
  printIr e = let op = operatorOfFbinExpr e
                  (c1, c2) = operandOfFbinExpr e
                  t = typeOfFbinExpr e
                  cs = case e of
                    Fadd x _ _ _ -> printIr x
                    Fsub x _ _ _ -> printIr x
                    Fdiv x _ _ _ -> printIr x
                    Frem x _ _ _ -> printIr x
                    _ -> empty
              in text op <+> cs <+> parens (printIr (TypedConst t c1) <> comma <+> printIr (TypedConst t c2))


instance IrPrint (BinExpr Const) where  
  printIr (Ie e) = printIr e
  printIr (Fe e) = printIr e

instance IrPrint (Conversion TypedConst) where
  printIr (Conversion op tc t) = printIr op <+> parens (printIr tc <+> text "to" <+> printIr t)

instance IrPrint (GetElemPtr TypedConst) where
  printIr (GetElemPtr b base indices) = 
    hsep [text "getelementptr", printIr b, parens (commaSepList ((printIr base):fmap printIr indices))]
                                       
instance IrPrint (Select TypedConst) where
  printIr (Select cnd tc1 tc2) = text "select" <+> (parens (commaSepList [printIr cnd, printIr tc1, printIr tc2]))

instance IrPrint (Icmp Const) where
  printIr (Icmp op t c1 c2) = 
    text "icmp" <+> printIr op <+> parens (commaSepList [printIr (TypedConst t c1), printIr (TypedConst t c2)])
  
instance IrPrint (Fcmp Const) where
  printIr (Fcmp op t c1 c2) = 
    text "fcmp" <+> printIr op <+> parens (commaSepList [printIr (TypedConst t c1), printIr (TypedConst t c2)])
  
instance IrPrint (ShuffleVector TypedConst) where
  printIr (ShuffleVector tc1 tc2 mask) = 
    text "shufflevector" <+> parens (commaSepList [printIr tc1, printIr tc2, printIr mask])

instance IrPrint (ExtractValue TypedConst) where
  printIr (ExtractValue tc indices) = 
    hsep [text "extractvalue", parens (commaSepList ((printIr tc:(fmap text indices))))]
                                   
instance IrPrint (InsertValue TypedConst) where
  printIr (InsertValue vect tc indices) = 
    hsep [text "insertvalue", parens (commaSepList ((printIr vect:printIr tc:(fmap text indices))))]
                                       
instance IrPrint (ExtractElem TypedConst) where
  printIr (ExtractElem tc index) = 
    hsep [text "extractelement", parens (printIr tc <> comma <+> printIr index)]
                                
instance IrPrint (InsertElem TypedConst) where                                  
  printIr (InsertElem tc1 tc2 index) = 
    hsep [text "insertelement", parens (printIr tc1 <> comma <+> printIr tc2 <> comma <+> printIr index)]
                                          
instance IrPrint Const where
  printIr x = case x of
    Ccp c -> printIr c
    Cca a -> printIr a
    Cl l -> printIr l
    CblockAddress g a -> text "blockaddress" <+> parens (printIr g <> comma <+> printIr a)
    Cb a -> printIr a
    Cconv a -> printIr a 
    CgEp a -> printIr a
    Cs a -> printIr a
    CiC a -> printIr a
    CfC a -> printIr a 
    CsV a -> printIr a
    CeV a -> printIr a
    CiV a -> printIr a
    CeE a -> printIr a
    CiE a -> printIr a
    CmC mc -> printIr mc
    CmL id -> printIr id

instance IrPrint MdVar where 
  printIr (MdVar s) = char '!' <> (text s)
  
instance IrPrint MdNode where
  printIr (MdNode s) = char '!' <> (text s)
  
instance IrPrint MetaConst where
  printIr (MdConst c) = char '!' <> (printIr c)
  printIr (MdString s) = char '!' <> (printIr s)
  printIr (McMn n) = printIr n
  printIr (McMv v) = printIr v
  printIr (MdRef s) = text $ show s
                        
instance IrPrint (GetElemPtr TypedValue) where
  printIr (GetElemPtr ib tv tcs) = 
    hsep [text "getelementptr", printIr ib, printIr tv, (commaSepList $ fmap printIr tcs)]
  

instance IrPrint (IbinExpr Value) where  
  printIr e = let op = operatorOfIbinExpr e
                  (v1, v2) = operandOfIbinExpr e
                  t = typeOfIbinExpr e
                  cs = case e of
                        Add x _ _ _ -> printIr x
                        Sub x _ _ _ -> printIr x
                        Udiv x _ _ _ -> printIr x
                        Sdiv x _ _ _ -> printIr x
                        Shl x _ _ _ -> printIr x
                        Lshr x _ _ _ -> printIr x
                        Ashr x _ _ _ -> printIr x
                        _ -> empty
              in 
               text op <+> cs 
               <+> printIr t 
               <+> printIr v1 <+> comma <+> printIr v2


instance IrPrint (FbinExpr Value) where  
  printIr e = let op = operatorOfFbinExpr e
                  (v1, v2) = operandOfFbinExpr e
                  t = typeOfFbinExpr e
                  cs = case e of
                        Fadd x _ _ _ -> printIr x
                        Fsub x _ _ _ -> printIr x
                        Fdiv x _ _ _ -> printIr x
                        Frem x _ _ _ -> printIr x
                        _ -> empty
              in 
               text op <+> cs 
               <+> printIr t 
               <+> printIr v1 <+> comma <+> printIr v2

instance IrPrint (BinExpr Value) where  
  printIr (Ie e) = printIr e
  printIr (Fe e) = printIr e
  
instance IrPrint (Icmp Value) where  
  printIr (Icmp op t v1 v2) = hsep [text "icmp", printIr op, printIr t, printIr v1 <> comma, printIr v2]
  
instance IrPrint (Fcmp Value) where  
  printIr (Fcmp op t v1 v2) = hsep [text "fcmp", printIr op, printIr t, printIr v1 <> comma, printIr v2]
  
instance IrPrint (Conversion TypedValue) where  
  printIr (Conversion op tv t) = hsep [printIr op, printIr tv, text "to", printIr t]
  
instance IrPrint (Select TypedValue) where  
  printIr (Select c t f) = text "select" <+> (commaSepList [printIr c, printIr t, printIr f])

instance IrPrint Expr where
  printIr (EgEp a) = printIr a
  printIr (EiC a) = printIr a
  printIr (EfC a) = printIr a
  printIr (Eb a) = printIr a
  printIr (Ec a) = printIr a
  printIr (Es a) = printIr a
  printIr (Ev a) = printIr a
  
instance IrPrint MemOp where  
  printIr (Allocate ma t s a) = 
    hsep [text "alloca", printIr ma, printIr t, commaSepMaybe s, commaSepMaybe a]
  printIr (Load v ptr align nonterm invar nonull) = 
    hsep [text "load", printIr v, printIr ptr, commaSepNonEmpty [printIr align, printIr nonterm, printIr invar, printIr nonull]]
  printIr (LoadAtomic (Atomicity st ord) v ptr align) = 
    hsep [text "load atomic", printIr v, printIr ptr, printIr st, printIr ord, commaSepMaybe align]
  printIr (Store b v addr align nonterm) = 
    hsep [text "store", printIr b, printIr v <> comma, printIr addr, commaSepMaybe align, commaSepMaybe nonterm]
  printIr (StoreAtomic (Atomicity st ord) b v ptr align) = 
    hsep [text "store atomic", printIr b, printIr v <> comma, printIr ptr, printIr st, printIr ord <> commaSepMaybe align]
  printIr (Fence b order) = hsep [text "fence", printIr b, printIr order]
  printIr (CmpXchg wk v p c n st sord ford) = 
    hsep [text "cmpxchg", printIr wk, printIr v, printIr p <> comma, printIr c <> comma, printIr n, printIr st, printIr sord, printIr ford]
  printIr (AtomicRmw v op p vl st ord) = 
    hsep [text "atomicrmw", printIr v, printIr op, printIr p <> comma, printIr vl, printIr st, printIr ord]
  
instance IrPrint Value where
  printIr (VgOl i) = printIr i
  printIr (Ve e) = printIr e
  printIr (Vc c) = printIr c

instance IrPrint TypedValue where
  printIr (TypedValue t v) = printIr t <+> printIr v
  
instance IrPrint TypedConst where  
  printIr (TypedConst t c) = printIr t <+> printIr c
  printIr TypedConstNull = text "null"

instance IrPrint TypedPointer where
    printIr (TypedPointer t v) = printIr t <+> printIr v

instance IrPrint Pointer where
    printIr (Pointer v) = printIr v

instance IrPrint FunName where
  printIr (FunNameGlobal s) = printIr s
  printIr (FunNameString s) = text s
  
instance IrPrint CallSite where
  printIr (CsFun cc ra rt ident params fa) = 
    hsep [printIr cc, hsep $ fmap printIr ra, printIr rt, printIr ident, parens (commaSepList $ fmap printIr params), hsep $ fmap printIr fa]
  printIr (CsAsm t se as dia s1 s2 params fa) = 
    hsep [printIr t, text "asm", printIr se, printIr as, printIr dia, printIr s1 <> comma, printIr s2, parens (commaSepList $ fmap printIr params), hsep $ fmap printIr fa]
  printIr (CsConversion ra t convert params fa) = 
    hsep [hsep $ fmap printIr ra, printIr t, printIr convert, parens (hsep $ fmap printIr params), hsep $ fmap printIr fa]

instance IrPrint Clause where
  printIr (Catch tv) = text "catch" <+> printIr tv
  printIr (Filter tc) = text "filter" <+> printIr tc
  printIr (Cco c) = printIr c

instance IrPrint (Conversion (Type, GlobalOrLocalId)) where
  printIr (Conversion op (t, g) dt) = printIr op <+> parens  (printIr t <+> printIr g <+> text "to" <+> printIr dt)
  
instance IrPrint PersFn where
    printIr (PersFnId g) = printIr g
    printIr (PersFnCast c) = printIr c
    printIr PersFnUndef = text "undef"                                     
    printIr PersFnNull = text "null"
    printIr (PersFnConst c) = printIr c


instance IrPrint (ExtractElem TypedValue) where
  printIr (ExtractElem tv1 tv2) = hsep [text "extractelement", printIr tv1 <> comma, printIr tv2]
  
instance IrPrint (InsertElem TypedValue) where  
  printIr (InsertElem vect tv idx) = 
    hsep [text "insertelement", printIr vect <> comma, printIr tv <> comma, printIr idx]
  
instance IrPrint (ShuffleVector TypedValue) where  
  printIr (ShuffleVector vect1 vect2 mask) = 
    hsep [text "shufflevector", printIr vect1 <> comma, printIr vect2 <> comma, printIr mask]
  
instance IrPrint (ExtractValue TypedValue) where  
  printIr (ExtractValue tv idxs) = 
    hsep [text "extractvalue", printIr tv <> comma, (commaSepList $ fmap text idxs)]
                                  
instance IrPrint (InsertValue TypedValue) where  
  printIr (InsertValue vect tv idx) = text "insertvalue" <+> hsep (punctuate comma ((printIr vect):(printIr tv):(fmap text idx)))

instance IrPrint Rhs where
  printIr (RmO a) = printIr a
  printIr (Re a) = printIr a
  printIr (Call tailc callSite) = printIr tailc <+> text "call" <+> printIr callSite
  printIr (ReE a) = printIr a
  printIr (RiE a) = printIr a
  printIr (RsV a) = printIr a
  printIr (ReV a) = printIr a
  printIr (RiV a) = printIr a
  printIr (VaArg tv t) = text "va_arg" <+> printIr tv <+> comma <+> printIr t    
  printIr (LandingPad rt pt tgl b clause) =  
    hsep [text "landingpad", printIr rt, text "personality", printIr pt, printIr tgl, printIr b, hsep $ fmap printIr clause]
  

instance IrPrint ActualParam where
  printIr (ActualParam t att1 align v att2) = 
    hsep [printIr t, hsep $ fmap printIr att1, printIr align, printIr v, hsep $ fmap printIr att2]
  

instance IrPrint Dbg where
  printIr (Dbg mv meta) = printIr mv <+> printIr meta


instance IrPrint PhiInst where
  printIr (PhiInst lhs t pairs) =  optSepToLlvm lhs equals <+> text "phi" 
                                  <+> printIr t <+> (commaSepList $ fmap tvToLLvm pairs)
    where tvToLLvm (h1,h2) = brackets (printIr h1 <+> comma <+> printIr h2)

instance IrPrint ComputingInst where
  printIr (ComputingInst lhs rhs) = optSepToLlvm lhs equals <+> printIr rhs
                          

instance IrPrint TerminatorInst where
  printIr (Return x) = text "ret" <+> (if null x then text "void" else commaSepList $ fmap printIr x)
  printIr (Br a) = text "br" <+> printIr a
  printIr (Cbr v t f) = text "br i1" <+> (commaSepList [printIr v, printIr t, printIr f])
  printIr (IndirectBr v l) = hsep [text "indirectbr", printIr v <> comma, brackets (commaSepList $ fmap printIr l)]
  printIr (Switch v d tbl) = 
    hsep [text "switch", printIr v <> comma, printIr d, brackets (hsep $ fmap (\(p1,p2) -> printIr p1 <> comma <+> printIr p2) tbl)]
  printIr (Invoke lhs callSite toL unwindL) = 
    hsep [optSepToLlvm lhs equals, text "invoke", printIr callSite, text "to", printIr toL, text "unwind", printIr unwindL]
  printIr Unreachable = text "unreachable"
  printIr (Resume a) = text "resume" <+> printIr a
  printIr Unwind = text "unwind"
             

instance IrPrint PhiInstWithDbg where
  printIr (PhiInstWithDbg ins dbgs) = commaSepList ((printIr ins):fmap printIr dbgs)

instance IrPrint TerminatorInstWithDbg where
  printIr (TerminatorInstWithDbg ins dbgs) = commaSepList ((printIr ins):fmap printIr dbgs)

instance IrPrint ComputingInstWithDbg where
  printIr (ComputingInstWithDbg ins dbgs) = commaSepList ((printIr ins):fmap printIr dbgs)


instance IrPrint Aliasee where
  printIr (AtV tv ) = printIr tv
  printIr (Ac c) = printIr c
  printIr (AgEp a) = printIr a
                      

instance IrPrint FunctionPrototype where
  printIr (FunctionPrototype fhLinkage fhVisibility fhCCoonc fhAttr fhRetType fhName fhParams fhd fhAttr1 fhSection
           fhCmd fhAlign fhGc fhPrefix fhPrologue) = 
    hsep [printIr fhLinkage, printIr fhVisibility, printIr fhCCoonc, hsep $ fmap printIr fhAttr
         , printIr fhRetType, printIr fhName, printIr fhParams, printIr fhd, hsep $ fmap printIr fhAttr1
         , printIr fhSection, printIr fhCmd, printIr fhAlign, printIr fhGc, printIr fhPrefix, printIr fhPrologue]
  


instance IrPrint Prefix where
  printIr (Prefix n) = text "prefix" <+> printIr n
  
instance IrPrint Prologue where  
  printIr (Prologue n) = text "prologue" <+> printIr n


instance IrPrint IcmpOp where
  printIr = P.print 

instance IrPrint FcmpOp where
  printIr = P.print

instance IrPrint ConvertOp where
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

instance IrPrint Alignment where
    printIr = P.print

instance IrPrint Gc where
    printIr = P.print

instance IrPrint GlobalType where
    printIr = P.print


instance IrPrint TypePrimitive where
  printIr = P.print


instance IrPrint Lstring where
  printIr = P.print
  
instance IrPrint GlobalOrLocalId where
  printIr = P.print
                    
instance IrPrint LocalId where
  printIr = P.print
                      
instance IrPrint GlobalId where
  printIr = P.print

  
instance IrPrint SimpleConstant where
  printIr = P.print
                          
instance IrPrint AtomicMemoryOrdering where
  printIr = P.print

instance IrPrint AtomicOp where
    printIr = P.print
    
instance IrPrint AddrSpace where
  printIr = P.print
    
instance IrPrint Type where
  printIr = P.print

instance IrPrint Fparam where
  printIr = P.print
  
instance IrPrint FormalParam where
  printIr = P.print

instance IrPrint FormalParamList where
  printIr = P.print

instance IrPrint TypeParamList where
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

instance IrPrint DollarId where
  printIr = P.print


instance IrPrint Comdat where
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
    
instance IrPrint AbiAlign where    
  printIr = P.print

instance IrPrint PrefAlign where
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
