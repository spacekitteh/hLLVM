{-# LANGUAGE FlexibleInstances #-}
module Llvm.VmCore.CoreIrWriter where
import Data.List
import Llvm.VmCore.CoreIr
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.SharedEntityWriter
import Llvm.VmCore.DataLayoutWriter

instance AsmWriter Label where
  toLlvm l = text (show l) 
  
instance AsmWriter LabelId where
  toLlvm (LabelString l) = toLlvm l
  toLlvm (LabelNumber l) = toLlvm l
  toLlvm (LabelQuoteNumber l) = toLlvm l
  toLlvm (LabelQuoteString l) = toLlvm l

instance AsmWriter PercentLabel where
  toLlvm (PercentLabel li) = char '%' <> (toLlvm li)
  
instance AsmWriter TargetLabel where
  toLlvm (TargetLabel id) = text "label" <+> toLlvm id
  

instance AsmWriter BlockLabel where
  toLlvm (BlockLabel l) = toLlvm l <> char ':'

instance AsmWriter ComplexConstant where
  toLlvm (Cstruct b ts) = let v = (braces (commaSepList $ fmap toLlvm ts))
                          in if b then angleBrackets v
                             else v
  toLlvm (Cvector ts) = angleBrackets (commaSepList $ fmap toLlvm ts)
  toLlvm (Carray ts) = brackets (commaSepList $ fmap toLlvm ts)


instance AsmWriter Exact where
  toLlvm Exact = text "exact"

instance AsmWriter NoWrap where
    toLlvm Nsw = text "nsw"
    toLlvm Nuw = text "nuw"
    toLlvm Nsuw = text "nsw nuw"


instance AsmWriter (IbinExpr Const) where  
  toLlvm e = let op = operatorOfIbinExpr e
                 (c1, c2) = operandOfIbinExpr e
                 t = typeOfIbinExpr e
                 cs = case e of
                        Add x _ _ _ -> optToLlvm x
                        Sub x _ _ _ -> optToLlvm x
                        Udiv x _ _ _ -> optToLlvm x
                        Sdiv x _ _ _ -> optToLlvm x
                        Shl x _ _ _ -> optToLlvm x
                        Lshr x _ _ _ -> optToLlvm x
                        Ashr x _ _ _ -> optToLlvm x
                        _ -> empty
             in text op <+> cs <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))


instance AsmWriter (FbinExpr Const) where  
  toLlvm e = let op = operatorOfFbinExpr e
                 (c1, c2) = operandOfFbinExpr e
                 t = typeOfFbinExpr e
                 cs = case e of
                        Fadd x _ _ _ -> toLlvm x
                        Fsub x _ _ _ -> toLlvm x
                        Fdiv x _ _ _ -> toLlvm x
                        Frem x _ _ _ -> toLlvm x
                        _ -> empty
             in text op <+> cs <+> parens (toLlvm (TypedConst t c1) <> comma <+> toLlvm (TypedConst t c2))


instance AsmWriter (BinExpr Const) where  
  toLlvm (Ie e) = toLlvm e
  toLlvm (Fe e) = toLlvm e


instance AsmWriter (Conversion TypedConst) where
  toLlvm (Conversion op tc t) = toLlvm op <+> parens (toLlvm tc <+> text "to" <+> toLlvm t)


instance AsmWriter (GetElemPtr TypedConst) where
  toLlvm (GetElemPtr b base indices) = text "getelementptr" 
                                       <+> toLlvm b
                                       <+> parens (commaSepList ((toLlvm base):fmap toLlvm indices))
                                       
instance AsmWriter (Select TypedConst) where
  toLlvm (Select cnd tc1 tc2) = text "select" <+> (parens (commaSepList [toLlvm cnd, toLlvm tc1, toLlvm tc2]))


instance AsmWriter (Icmp Const) where
  toLlvm (Icmp op t c1 c2) = 
    text "icmp" <+> toLlvm op <+> parens (commaSepList [toLlvm (TypedConst t c1), toLlvm (TypedConst t c2)])
  
instance AsmWriter (Fcmp Const) where
  toLlvm (Fcmp op t c1 c2) = 
    text "fcmp" <+> toLlvm op <+> parens (commaSepList [toLlvm (TypedConst t c1), toLlvm (TypedConst t c2)])
  

instance AsmWriter (ShuffleVector TypedConst) where
  toLlvm (ShuffleVector tc1 tc2 mask) = 
    text "shufflevector" <+> parens (commaSepList [toLlvm tc1, toLlvm tc2, toLlvm mask])


instance AsmWriter (ExtractValue TypedConst) where
  toLlvm (ExtractValue tc indices) = text "extractvalue" <+> 
                                     parens (commaSepList ((toLlvm tc:(fmap text indices))))
                                   
instance AsmWriter (InsertValue TypedConst) where
  toLlvm (InsertValue vect tc indices) = text "insertvalue" <+> 
                                         parens (commaSepList ((toLlvm vect:toLlvm tc:(fmap text indices))))
                                       
instance AsmWriter (ExtractElem TypedConst) where
  toLlvm (ExtractElem tc index) = text "extractelement" <+> 
                                  parens (toLlvm tc <> comma <+> toLlvm index)
                                
                                  
instance AsmWriter (InsertElem TypedConst) where                                  
  toLlvm (InsertElem tc1 tc2 index) = text "insertelement" <+> 
                                      parens (toLlvm tc1 <> comma <+> toLlvm tc2)
    
                                      
instance AsmWriter Const where
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
    CmL id -> toLlvm id

instance AsmWriter MdVar where 
  toLlvm (MdVar s) = char '!' <> (text s)
  
instance AsmWriter MdNode where
  toLlvm (MdNode s) = char '!' <> (text s)
  
instance AsmWriter MetaConst where
  toLlvm (MdConst c) = char '!' <> (toLlvm c)
  toLlvm (MdString s) = char '!' <> (toLlvm s)
  toLlvm (McMn n) = toLlvm n
  toLlvm (McMv v) = toLlvm v
  toLlvm (MdRef s) = text $ show s
                        
instance AsmWriter (GetElemPtr TypedValue) where
  toLlvm (GetElemPtr ib tv tcs) = text "getelementptr" <+> 
                                  toLlvm ib <+>
                                  toLlvm tv <+> (commaSepList $ fmap toLlvm tcs)
  

instance AsmWriter (IbinExpr Value) where  
  toLlvm e = let op = operatorOfIbinExpr e
                 (v1, v2) = operandOfIbinExpr e
                 t = typeOfIbinExpr e
                 cs = case e of
                        Add x _ _ _ -> optToLlvm x
                        Sub x _ _ _ -> optToLlvm x
                        Udiv x _ _ _ -> optToLlvm x
                        Sdiv x _ _ _ -> optToLlvm x
                        Shl x _ _ _ -> optToLlvm x
                        Lshr x _ _ _ -> optToLlvm x
                        Ashr x _ _ _ -> optToLlvm x
                        _ -> empty
             in 
               text op <+> cs 
               <+> toLlvm t 
               <+> toLlvm v1 <+> comma <+> toLlvm v2


instance AsmWriter (FbinExpr Value) where  
  toLlvm e = let op = operatorOfFbinExpr e
                 (v1, v2) = operandOfFbinExpr e
                 t = typeOfFbinExpr e
                 cs = case e of
                        Fadd x _ _ _ -> toLlvm x
                        Fsub x _ _ _ -> toLlvm x
                        Fdiv x _ _ _ -> toLlvm x
                        Frem x _ _ _ -> toLlvm x
                        _ -> empty
             in 
               text op <+> cs 
               <+> toLlvm t 
               <+> toLlvm v1 <+> comma <+> toLlvm v2

instance AsmWriter (BinExpr Value) where  
  toLlvm (Ie e) = toLlvm e
  toLlvm (Fe e) = toLlvm e
  
instance AsmWriter (Icmp Value) where  
  toLlvm (Icmp op t v1 v2) = text "icmp" 
                             <+> toLlvm op 
                             <+> toLlvm t 
                             <+> toLlvm v1 <> comma <+> toLlvm v2

  
instance AsmWriter (Fcmp Value) where  
  toLlvm (Fcmp op t v1 v2) = text "fcmp" 
                             <+> toLlvm op 
                             <+> toLlvm t
                             <+> toLlvm v1 <> comma <+> toLlvm v2
  
instance AsmWriter (Conversion TypedValue) where  
  toLlvm (Conversion op tv t) = toLlvm op <+> toLlvm tv <+> (text "to") <+> toLlvm t
  
instance AsmWriter (Select TypedValue) where  
  toLlvm (Select c t f) = text "select" <+> (commaSepList [toLlvm c, toLlvm t, toLlvm f])

instance AsmWriter Expr where
  toLlvm (EgEp a) = toLlvm a
  toLlvm (EiC a) = toLlvm a
  toLlvm (EfC a) = toLlvm a
  toLlvm (Eb a) = toLlvm a
  toLlvm (Ec a) = toLlvm a
  toLlvm (Es a) = toLlvm a
  toLlvm (Ev a) = toLlvm a
  
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



-- stOrdToLlvm st ord = (if st then " singlethread" else "") ++ sepOptToLlvm " " ord

instance AsmWriter MemOp where  
  toLlvm (Allocate ma t s a) = toLlvm ma <+> toLlvm t <+> sepOptToLlvm comma s <+> sepOptToLlvm comma a
--   toLlvm (Free tv) = "free " ++ toLlvm tv
  toLlvm (Load v ptr align nonterm invar nonull) = text "load" <+> toLlvm v
                              <+> toLlvm ptr <+> sepOptToLlvm comma align 
                              <+> sepOptToLlvm comma nonterm
                              <+> sepOptToLlvm comma invar
                              <+> sepOptToLlvm comma nonull
  toLlvm (LoadAtomic (Atomicity st ord) v ptr align) = 
    text "load atomic"
    <+> toLlvm v
    <+> toLlvm ptr 
    <+> toLlvm st
    <+> toLlvm ord
    <+> sepOptToLlvm comma align
  toLlvm (Store b v addr align nonterm) = text "store" <+> toLlvm b
                                          <+> toLlvm v <+> comma <+> toLlvm addr <+> sepOptToLlvm comma align
                                          <+> sepOptToLlvm comma nonterm 
  toLlvm (StoreAtomic (Atomicity st ord) b v ptr align) = 
    text "store atomic" 
    <+> toLlvm b
    <+> toLlvm v <+> comma
    <+> toLlvm ptr 
    <+> toLlvm st 
    <+> toLlvm ord
    <+> sepOptToLlvm comma align
  toLlvm (Fence b order) = text "fence" <+> toLlvm b <+> toLlvm order
  toLlvm (CmpXchg wk v p c n st sord ford) = 
    text "cmpxchg" <+> toLlvm wk
    <+> toLlvm v
    <+> toLlvm p <+> comma
    <+> toLlvm c <+> comma
    <+> toLlvm n 
    <+> toLlvm st
    <+> toLlvm sord
    <+> toLlvm ford
  toLlvm (AtomicRmw v op p vl st ord) = 
    text "atomicrmw" <+> toLlvm v
    <+> toLlvm op 
    <+> toLlvm p <+> comma
    <+> toLlvm vl 
    <+> toLlvm st
    <+> toLlvm ord
  

instance AsmWriter Value where
  toLlvm (VgOl i) = toLlvm i
  toLlvm (Ve e) = toLlvm e
  toLlvm (Vc c) = toLlvm c
--  toLlvm (ViA i) = toLlvm i
  toLlvm (InlineAsm se as s1 s2) = text "asm" 
                                   <+> (if se then text "sideeffect" else empty) 
                                   <+> (if as then text "alignstack" else empty)
                                   <+> text s1 <+> comma <+> text s2     

instance AsmWriter TypedValue where
  toLlvm (TypedValue t v) = toLlvm t <+> toLlvm v
  
instance AsmWriter TypedConst where  
  toLlvm (TypedConst t c) = toLlvm t <+> toLlvm c
  toLlvm TypedConstNull = text "null"

instance AsmWriter TypedPointer where
    toLlvm (TypedPointer t v) = toLlvm t <+> toLlvm v

instance AsmWriter Pointer where
    toLlvm (Pointer v) = toLlvm v

instance AsmWriter FunName where
  toLlvm (FunNameGlobal s) = toLlvm s
  toLlvm (FunNameString s) = text s
  
instance AsmWriter CallSite where
  toLlvm (CallFun cc ra rt ident params fa) = (maybe empty toLlvm cc) <+> (hsep $ fmap toLlvm ra)
                                              <+> toLlvm rt <+> toLlvm ident
                                              <+> parens (commaSepList $ fmap toLlvm params)
                                              <+> toLlvm fa
  toLlvm (CallAsm t se as dia s1 s2 params fa) = (toLlvm t) <+> text "asm"
                                                 <+> (if se then text "sideeffect" else empty)
                                                 <+> (if as then text "alignstack" else empty)
                                                 <+> toLlvm dia
                                                 <+> toLlvm s1 <+> comma <+> toLlvm s2
                                                 <+> parens (commaSepList $ fmap toLlvm params)
                                                 <+> toLlvm fa
  toLlvm (CallConversion ra t convert params fa) = (hsep $ fmap toLlvm ra)
                                                <+> (toLlvm t) 
                                                <+> toLlvm convert 
                                                <+> parens (hsep $ fmap toLlvm params)
                                                <+> toLlvm fa

instance AsmWriter Clause where
  toLlvm (Catch tv) = text "catch" <+> toLlvm tv
  toLlvm (Filter tc) = text "filter" <+> toLlvm tc
  toLlvm (Cco c) = toLlvm c

instance AsmWriter (Conversion (Type, GlobalOrLocalId)) where
  toLlvm (Conversion op (t, g) dt) = toLlvm op <+> 
                                     parens  (toLlvm t <+> toLlvm g <+> text "to" <+> toLlvm dt)
  
instance AsmWriter PersFn where
    toLlvm (PersFnId g) = toLlvm g
    toLlvm (PersFnCast c) = toLlvm c
    toLlvm PersFnUndef = text "undef"                                     
  


instance AsmWriter (ExtractElem TypedValue) where
  toLlvm (ExtractElem tv1 tv2) = text "extractelement" <+> toLlvm tv1 <+> comma <+> toLlvm tv2
  
instance AsmWriter (InsertElem TypedValue) where  
  toLlvm (InsertElem vect tv idx) = text "insertelement" <+> toLlvm vect <+> comma 
                                    <+> toLlvm tv <+> comma <+> toLlvm idx
  
instance AsmWriter (ShuffleVector TypedValue) where  
  toLlvm (ShuffleVector vect1 vect2 mask) = text "shufflevector" <+> toLlvm vect1 <+> comma
                                        <+> toLlvm vect2 <+> comma
                                        <+> toLlvm mask
  
instance AsmWriter (ExtractValue TypedValue) where  
  toLlvm (ExtractValue tv idxs) = text "extractvalue" <+> toLlvm tv 
                                  <+> comma <+> (commaSepList $ fmap text idxs)
                                  
instance AsmWriter (InsertValue TypedValue) where  
  toLlvm (InsertValue vect tv idx) = text "insertvalue" <+> hsep (punctuate comma ((toLlvm vect):(toLlvm tv):(fmap text idx)))

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
  toLlvm (LandingPad rt pt tgl b clause) =  text "landingpad" <+> toLlvm rt
                                            <+> text "personality" 
                                            <+> (toLlvm pt) 
                                            <+> (toLlvm tgl) 
                                            <+> (if b then text "cleanup" else empty)
                                            <+> listToDoc toLlvm clause (<+>)
  

instance AsmWriter ActualParam where
  toLlvm (ActualParam t att1 align v att2) = toLlvm t <+> (hsep $ fmap toLlvm att1)
                                             <+> (maybe empty toLlvm align)
                                             <+> toLlvm v <+> (hsep $ fmap toLlvm att2)
  

instance AsmWriter Dbg where
  toLlvm (Dbg mv meta) = toLlvm mv <+> toLlvm meta


instance AsmWriter PhiInst where
  toLlvm (PhiInst lhs t pairs) =  optSepToLlvm lhs equals <+> text "phi" 
                                  <+> toLlvm t <+> (commaSepList $ fmap tvToLLvm pairs)
    where tvToLLvm (h1,h2) = brackets (toLlvm h1 <+> comma <+> toLlvm h2)

instance AsmWriter ComputingInst where
  toLlvm (ComputingInst lhs rhs) = optSepToLlvm lhs equals <+> toLlvm rhs
                          

instance AsmWriter TerminatorInst where
  toLlvm (Return x) = text "ret" <+> (if null x then text "void" else commaSepList $ fmap toLlvm x)
  toLlvm (Br a) = text "br" <+> toLlvm a
  toLlvm (Cbr v t f) = text "br i1" <+> (commaSepList [toLlvm v, toLlvm t, toLlvm f])
  toLlvm (IndirectBr v l) = text "indirectbr" <+> toLlvm v <+> comma <+> brackets  (commaSepList $ fmap toLlvm l)
  toLlvm (Switch v d tbl) = text "switch" <+> toLlvm v <+> comma  <+> toLlvm d <+> 
                            brackets (listToDoc (\(p1,p2) -> toLlvm p1 <> comma <+> toLlvm p2) tbl (<+>))
  toLlvm (Invoke lhs callSite toL unwindL) = optSepToLlvm lhs equals <+>
                                             text "invoke" <+> toLlvm callSite 
                                             <+> text "to" <+> toLlvm toL 
                                             <+> text "unwind" <+> toLlvm unwindL
  toLlvm Unreachable = text "unreachable"
  toLlvm (Resume a) = text "resume" <+> toLlvm a
  toLlvm Unwind = text "unwind"
             
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
          fhCCoonc fhAttr fhRetType fhName fhParams fhd fhAttr1 fhSection
          fhCmd fhAlign fhGc fhPrefix fhPrologue
         ) = (maybe empty toLlvm fhLinkage) <+>  (maybe empty toLlvm fhVisibility) 
             <+> (maybe empty toLlvm fhCCoonc) <+> (hsep $ fmap toLlvm fhAttr)
             <+> toLlvm fhRetType <+> toLlvm fhName <+> toLlvm fhParams 
             <+> (maybe empty toLlvm fhd)
             <+> (toLlvm fhAttr1) 
             <+> (maybe empty toLlvm fhSection) 
             <+> (maybe empty toLlvm fhCmd)
             <+> (maybe empty toLlvm fhAlign)
             <+> (maybe empty toLlvm fhGc)
             <+> (maybe empty toLlvm fhPrefix)
             <+> (maybe empty toLlvm fhPrologue)
  


instance AsmWriter Prefix where
  toLlvm (Prefix n) = text "prefix" <+> toLlvm n
  
instance AsmWriter Prologue where  
  toLlvm (Prologue n) = text "prologue" <+> toLlvm n
