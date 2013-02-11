{-# LANGUAGE FlexibleInstances #-}
module Llvm.VmCore.CoreIrWriter where
import Data.List
import Llvm.VmCore.CoreIr
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.AtomicEntityWriter


instance AsmWriter Label where
  toLlvm l = (show l) 
  
instance AsmWriter LabelId where
  toLlvm (LabelString l) = toLlvm l
  toLlvm (LabelNumber l) = toLlvm l
  toLlvm (LabelQuoteNumber l) = toLlvm l
  toLlvm (LabelQuoteString l) = toLlvm l

instance AsmWriter PercentLabel where
  toLlvm (PercentLabel li) = "%" ++ (toLlvm li)
  
instance AsmWriter TargetLabel where
  toLlvm (TargetLabel id) = "label " ++ toLlvm id
  

instance AsmWriter BlockLabel where
  toLlvm (BlockLabel l) = toLlvm l ++ ":"  
  
  

{-
instance AsmWriter Type where
  toLlvm a = case a of 
    Tprimitive tp -> toLlvm tp
    Tmetadata -> "metadata"
    Topaque -> "opaque"
    Tname s -> '%':toLlvm s
    TquoteName s -> '%':'"':toLlvm s ++ "\""
    Tno i -> '%':show i
    TupRef i -> '\\':show i
    Tarray i t -> "[" ++ show i ++ " x " ++ toLlvm t ++ "]"
    Tvector i t -> "<" ++ show i ++ " x " ++ toLlvm t ++ ">"
    Tstruct b ts -> (if b then "<{" else "{") ++ 
                    listToLlvm "" ts ", " "" ++ 
                    (if b then "}>" else "}")
    Tpointer t addr -> toLlvm t ++ sepOptToLlvm " " addr ++ "*"
    Tfunction t fp atts -> toLlvm t ++ " " ++ toLlvm fp ++ listToLlvm " " atts ", " ""


instance AsmWriter TypeParamList where
  toLlvm (TypeParamList params b) = 
    "(" ++ listToLlvm "" params ", " "" ++ 
    (if b then (if null params then "" else ", ") ++ "..." else "") ++ ")"

instance AsmWriter Fparam where
  toLlvm (FimplicitParam) = ";implicit parameter\n"
  toLlvm (FexplicitParam x) = toLlvm x


instance AsmWriter FormalParam where
  toLlvm (FormalParam t att1 align id att2) =
    toLlvm t ++ listToLlvm " " att1 " " "" ++ sepOptToLlvm " " align
    ++ " " ++ toLlvm id ++ listToLlvm " " att2 " " ""
    

instance AsmWriter FormalParamList where
  toLlvm (FormalParamList params b atts) =
    "(" ++ listToLlvm "" params ", " "" ++ 
    (if b then (if null params then "" else ", ") ++ "..." else "") ++ ")" ++ 
    listToLlvm " " atts " " ""

-}


instance AsmWriter ComplexConstant where
  toLlvm (Cstruct b ts) = (if b then "<{" else "{") ++ listToLlvm "" ts ", " "" ++ 
                          (if b then "}>" else "}")
  toLlvm (Cvector ts) = "<" ++ listToLlvm "" ts ", " "" ++ ">"
  toLlvm (Carray ts) = "[" ++ listToLlvm "" ts ", " "" ++ "]"


instance AsmWriter Exact where
  toLlvm Exact = "exact"

instance AsmWriter NoWrap where
    toLlvm Nsw = "nsw"
    toLlvm Nuw = "nuw"
    toLlvm Nsuw = "nsw nuw"

instance AsmWriter (BinExpr Const) where  
  toLlvm e = let op = operatorOfBinExpr e
                 (c1, c2) = operandOfBinExpr e
                 t = typeOfBinExpr e
                 cs = case e of
                        Add x _ _ _ -> optToLlvm x
                        Sub x _ _ _ -> optToLlvm x
                        Udiv x _ _ _ -> optToLlvm x
                        Sdiv x _ _ _ -> optToLlvm x
                        Shl x _ _ _ -> optToLlvm x
                        Lshr x _ _ _ -> optToLlvm x
                        Ashr x _ _ _ -> optToLlvm x
                        _ -> ""
             in 
               op ++ " " ++ cs ++ "(" ++ toLlvm (TypedConst t c1)
                      ++ ", " ++ toLlvm (TypedConst t c2) ++ ")"


instance AsmWriter (Conversion TypedConst) where
  toLlvm (Conversion op tc t) = toLlvm op ++ "(" ++ toLlvm tc ++ " to " ++ toLlvm t ++ ")"


instance AsmWriter (GetElemPtr TypedConst) where
  toLlvm (GetElemPtr b base indices) = "getelementptr " 
                                       ++ (if b then "inbounds " else "")
                                       ++ "(" ++ toLlvm base
                                       ++ listToLlvm ", " indices ", " ""
                                       ++ ")"

instance AsmWriter (Select TypedConst) where
  toLlvm (Select cnd tc1 tc2) = "select (" ++ toLlvm cnd ++ ", " 
                              ++ toLlvm tc1 ++ ", " 
                              ++ toLlvm tc2 ++ ")"


instance AsmWriter (Icmp Const) where
  toLlvm (Icmp op t c1 c2) = "icmp " ++ toLlvm op 
                                       ++ "(" ++ toLlvm (TypedConst t c1) ++ ", " ++ toLlvm (TypedConst t c2) ++ ")"

instance AsmWriter (Fcmp Const) where
  toLlvm (Fcmp op t c1 c2) = "fcmp " ++ toLlvm op 
                                       ++ "(" ++ toLlvm (TypedConst t c1) ++ ", " ++ toLlvm (TypedConst t c2) ++ ")"
  

instance AsmWriter (ShuffleVector TypedConst) where
  toLlvm (ShuffleVector tc1 tc2 mask) = "shufflevector (" 
                                        ++ toLlvm tc1 ++ ", " ++ toLlvm tc2 ++ ", " 
                                        ++ toLlvm mask ++ ")"


instance AsmWriter (ExtractValue TypedConst) where
  toLlvm (ExtractValue tc indices) = "extractvalue (" 
                                   ++ toLlvm tc 
                                   ++ strListToString ", " indices ", " ""
                                   ++ ")"
                                   
instance AsmWriter (InsertValue TypedConst) where
  toLlvm (InsertValue vect tc indices) = "insertvalue (" 
                                       ++ toLlvm vect ++ ", " ++ toLlvm tc
                                       ++ strListToString ", " indices ", " ""
                                       ++ ")"
                                       
instance AsmWriter (ExtractElem TypedConst) where
  toLlvm (ExtractElem tc index) = "extractelement (" 
                                  ++ toLlvm tc ++ ", " 
                                  ++ toLlvm index ++ ")"
                                
                                  
instance AsmWriter (InsertElem TypedConst) where                                  
  toLlvm (InsertElem tc1 tc2 index) = "insertelement (" ++ toLlvm tc1 ++ ", " ++ toLlvm tc2 ++ ")"
    
                                      
instance AsmWriter Const where
  toLlvm x = case x of
    Ccp c -> toLlvm c
    Cca a -> toLlvm a
    Cl l -> toLlvm l
    CblockAddress g a -> "blockaddress(" ++ toLlvm g ++ ", " ++ toLlvm a ++ ")"
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
  toLlvm (MdVar s) = '!':s
  
instance AsmWriter MdNode where
  toLlvm (MdNode s) = '!':s
  
instance AsmWriter MetaConst where
  toLlvm (MdConst c) = '!':toLlvm c
  toLlvm (MdString s) = '!':toLlvm s
  toLlvm (McMn n) = toLlvm n
  toLlvm (McMv v) = toLlvm v
  toLlvm (MdRef s) = show s
                        
instance AsmWriter (GetElemPtr TypedValue) where
  toLlvm (GetElemPtr ib tv tcs) = "getelementptr " ++ 
                                  (if ib then "inbounds " else "") ++
                                  toLlvm tv ++ listToLlvm ", " tcs ", " ""
  

instance AsmWriter (BinExpr Value) where  
  toLlvm e = let op = operatorOfBinExpr e
                 (v1, v2) = operandOfBinExpr e
                 t = typeOfBinExpr e
                 cs = case e of
                        Add x _ _ _ -> optToLlvm x
                        Sub x _ _ _ -> optToLlvm x
                        Udiv x _ _ _ -> optToLlvm x
                        Sdiv x _ _ _ -> optToLlvm x
                        Shl x _ _ _ -> optToLlvm x
                        Lshr x _ _ _ -> optToLlvm x
                        Ashr x _ _ _ -> optToLlvm x
                        _ -> ""
             in 
               op ++ " " ++ cs ++ " " 
                      ++ toLlvm t ++ " "
                      ++ toLlvm v1 ++ ", " ++ toLlvm v2

  
instance AsmWriter (Icmp Value) where  
  toLlvm (Icmp op t v1 v2) = "icmp " ++ toLlvm op ++ " " ++ toLlvm t 
                             ++ " " ++ toLlvm v1 ++ ", " ++ toLlvm v2

  
instance AsmWriter (Fcmp Value) where  
  toLlvm (Fcmp op t v1 v2) = "fcmp " ++ toLlvm op ++ " " ++ toLlvm t
                             ++ " " ++ toLlvm v1 ++ ", " ++ toLlvm v2
  
instance AsmWriter (Conversion TypedValue) where  
  toLlvm (Conversion op tv t) = toLlvm op ++ " " ++ toLlvm tv ++ " to " ++ toLlvm t
  
instance AsmWriter (Select TypedValue) where  
  toLlvm (Select c t f) = "select " ++ toLlvm c ++ ", " ++ toLlvm t ++ ", " ++ toLlvm f

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
  toLlvm OnStack = "alloca"
  toLlvm InHeap = "malloc"
  
{-  
instance AsmWriter (AddrAttr l) where
  toLlvm (AddrAttr ptr align) = toLlvm ptr ++ sepOptToLlvm ", " align
-}



stOrdToLlvm st ord = (if st then " singlethread" else "") ++ sepOptToLlvm " " ord

instance AsmWriter MemOp where  
  toLlvm (Allocate ma t s a) = toLlvm ma ++ " " ++ toLlvm t ++ sepOptToLlvm ", " s ++ sepOptToLlvm ", " a
  toLlvm (Free tv) = "free " ++ toLlvm tv
  toLlvm (Load (NonAtomic b) ptr align) = "load " ++ (if b then "volatile " else "") 
                                          ++ toLlvm ptr ++ sepOptToLlvm ", " align
  toLlvm (Load (Atomic b st ord) ptr align) = "load atomic "
                                              ++ (if b then "volatile " else "") 
                                              ++ toLlvm ptr 
                                              ++ stOrdToLlvm st ord
                                              ++ sepOptToLlvm ", " align
  toLlvm (Store (NonAtomic b) v addr align) = "store " ++ (if b then "volatile " else "")  
                                              ++ toLlvm v ++ ", " ++ toLlvm addr ++ sepOptToLlvm ", " align
  toLlvm (Store (Atomic b st ord)  v ptr align) = "store atomic " 
                                                  ++ (if b then "volatile " else "") 
                                                  ++ toLlvm v ++ ", " 
                                                  ++ toLlvm ptr 
                                                  ++ stOrdToLlvm st ord
                                                  ++ sepOptToLlvm ", " align
  toLlvm (Fence b order) = "fence " ++ (if b then "singlethread " else "") ++ toLlvm order
  toLlvm (CmpXchg v p c n st ord) = "cmpxchg " ++ (if v then "volatile " else "") 
                                    ++ toLlvm p ++ ", "
                                    ++ toLlvm c ++ ", "
                                    ++ toLlvm n 
                                    ++ stOrdToLlvm st ord
  toLlvm (AtomicRmw v op p vl st ord) = "atomicrmw " ++ (if v then "volatile " else "")
                                        ++ toLlvm op ++ " "
                                        ++ toLlvm p ++ ", "
                                        ++ toLlvm vl 
                                        ++ stOrdToLlvm st ord                         
                                        

instance AsmWriter Value where
  toLlvm (VgOl i) = toLlvm i
  toLlvm (Ve e) = toLlvm e
  toLlvm (Vc c) = toLlvm c
--  toLlvm (ViA i) = toLlvm i
  toLlvm (InlineAsm se as s1 s2) = "asm " 
                                   ++ (if se then "sideeffect " else "") 
                                   ++ (if as then "alignstack " else "")
                                   ++ s1 ++ ", " ++ s2     

instance AsmWriter TypedValue where
  toLlvm (TypedValue t v) = toLlvm t ++ " " ++ toLlvm v
  
instance AsmWriter TypedConst where  
  toLlvm (TypedConst t c) = toLlvm t ++ " " ++ toLlvm c
  toLlvm TypedConstNull = "null"

instance AsmWriter TypedPointer where
    toLlvm (TypedPointer t v) = toLlvm t ++ " " ++ toLlvm v

instance AsmWriter Pointer where
    toLlvm (Pointer v) = toLlvm v

instance AsmWriter FunName where
  toLlvm (FunNameGlobal s) = toLlvm s
  toLlvm (FunNameString s) = s
  
instance AsmWriter CallSite where
  toLlvm (CallFun cc ra rt ident params fa) = optSepToLlvm cc " " ++ listToLlvm "" ra " " " "
                                              ++ toLlvm rt ++ " " ++ toLlvm ident
                                              ++ "(" ++ (listToLlvm "" params ", " "") ++ ")"
                                              ++ listToLlvm " " fa " " ""
  toLlvm (CallAsm t se as s1 s2 params fa) = (toLlvm t) ++ " asm "
                                             ++ (if se then "sideeffect " else "")
                                             ++ (if as then "alignstack " else "")
                                             ++ toLlvm s1 ++ ", " ++ toLlvm s2
                                             ++ "(" ++ (listToLlvm "" params ", " "") ++ ")"
                                             ++ listToLlvm " " fa " " ""
  toLlvm (CallConversion ra t convert params fa) = listToLlvm "" ra " " " " 
                                                ++ (toLlvm t) ++ " "
                                                ++ toLlvm convert 
                                                ++ "(" ++ (listToLlvm "" params ", " "") ++ ")"
                                                ++ listToLlvm " " fa " " ""

instance AsmWriter Clause where
  toLlvm (Catch tv) = "catch " ++ toLlvm tv
  toLlvm (Filter tc) = "filter " ++ toLlvm tc
  toLlvm (Cco c) = toLlvm c

instance AsmWriter (Conversion (Type, GlobalOrLocalId)) where
  toLlvm (Conversion op (t, g) dt) = toLlvm op ++ " (" ++ toLlvm t ++ " " 
                                       ++ toLlvm g ++ " to " ++ toLlvm dt ++ ")"
  
instance AsmWriter PersFn where
    toLlvm (PersFnId g) = toLlvm g
    toLlvm (PersFnCast c) = toLlvm c
    toLlvm PersFnUndef = "undef"                                     
  


instance AsmWriter (ExtractElem TypedValue) where
  toLlvm (ExtractElem tv1 tv2) = "extractelement " ++ toLlvm tv1 ++ ", " ++ toLlvm tv2
  
instance AsmWriter (InsertElem TypedValue) where  
  toLlvm (InsertElem vect tv idx) = "insertelement " ++ toLlvm vect ++ ", " 
                                   ++ toLlvm tv ++ ", " ++ toLlvm idx
  
instance AsmWriter (ShuffleVector TypedValue) where  
  toLlvm (ShuffleVector vect1 vect2 mask) = "shufflevector " ++ toLlvm vect1 ++ ", " 
                                        ++ toLlvm vect2 ++ ", "
                                        ++ toLlvm mask
  
instance AsmWriter (ExtractValue TypedValue) where  
  toLlvm (ExtractValue tv idxs) = "extractvalue " ++ toLlvm tv ++ listToString ", " id idxs ", " ""
  
instance AsmWriter (InsertValue TypedValue) where  
  toLlvm (InsertValue vect tv idx) = "insertvalue " ++ toLlvm vect ++ ", " 
                                     ++ toLlvm tv ++ strListToString ", " idx ", " ""

instance AsmWriter Rhs where
  toLlvm (RmO a) = toLlvm a
  toLlvm (Re a) = toLlvm a
  toLlvm (Call tail callSite) = (if tail then "tail " else "") ++ "call " ++ toLlvm callSite
  toLlvm (ReE a) = toLlvm a
  toLlvm (RiE a) = toLlvm a
  toLlvm (RsV a) = toLlvm a
  toLlvm (ReV a) = toLlvm a
  toLlvm (RiV a) = toLlvm a
  toLlvm (VaArg tv t) = "va_arg " ++ toLlvm tv ++ ", " ++ toLlvm t    
  toLlvm (LandingPad rt pt tgl b clause) =  "landingpad " ++ toLlvm rt
                                            ++ " personality " 
                                            ++ (toLlvm pt) ++ " "  
                                            ++ (toLlvm tgl) ++ " "
                                            ++ (if b then " cleanup " else "")
                                            ++ listToLlvm " " clause " " ""
  

instance AsmWriter ActualParam where
  toLlvm (ActualParam t att1 align v att2) = toLlvm t ++ listToLlvm " " att1 " " "" 
                                             ++ " " ++ optSepToLlvm align " " 
                                             ++ toLlvm v ++ listToLlvm " " att2 " " ""


instance AsmWriter Dbg where
  toLlvm (Dbg mv meta) = toLlvm mv ++ " " ++ toLlvm meta


instance AsmWriter PhiInst where
  toLlvm (PhiInst lhs t pairs) =  optSepToLlvm lhs " = " ++ " phi " 
                                  ++ toLlvm t ++ listToString " " tvToLLvm pairs ", " ""
    where tvToLLvm (h1,h2) = "[" ++ toLlvm h1 ++ ", " ++ toLlvm h2 ++ "]"

instance AsmWriter ComputingInst where
  toLlvm (ComputingInst lhs rhs) = optSepToLlvm lhs " = " ++ toLlvm rhs
                          

instance AsmWriter TerminatorInst where
  toLlvm (Return x) = "ret " ++ (if null x then "void" else listToLlvm "" x ", " "")
  toLlvm (Br a) = "br " ++ toLlvm a
  toLlvm (Cbr v t f) = "br i1 " ++ toLlvm v ++ ", " ++ toLlvm t ++ ", " ++ toLlvm f
  toLlvm (IndirectBr v l) = "indirectbr " ++ toLlvm v ++ ", [" ++  listToLlvm " " l ", " " " ++ "]"
  toLlvm (Switch v d tbl) = "switch " ++ toLlvm v ++ "," ++ toLlvm d ++ 
                            "[" ++ listToString " " (\(p1,p2) -> toLlvm p1 ++ ", " 
                                                                 ++ toLlvm p2) tbl " " " " ++  "]"
  toLlvm (Invoke lhs callSite toL unwindL) = optSepToLlvm lhs "=" ++ 
                                             "invoke " ++ toLlvm callSite ++ 
                                             " to " ++ toLlvm toL ++
                                             " unwind " ++ toLlvm unwindL
  toLlvm Unreachable = "unreachable"
  toLlvm (Resume a) = "resume " ++ toLlvm a
  toLlvm Unwind = "unwind"
             
instance AsmWriter TerminatorInstWithDbg where
  toLlvm (TerminatorInstWithDbg ins dbgs) = toLlvm ins ++ listToLlvm ", " dbgs ", " ""


instance AsmWriter ComputingInstWithDbg where
  toLlvm (ComputingInstWithDbg ins dbgs) = toLlvm ins ++ listToLlvm ", " dbgs ", " ""


instance AsmWriter Aliasee where
  toLlvm (AtV tv ) = toLlvm tv
  toLlvm (Ac c) = toLlvm c
  toLlvm (AgEp a) = toLlvm a
                      

instance AsmWriter FunctionPrototype where
  toLlvm (FunctionPrototype fhLinkage fhVisibility
          fhCCoonc fhAttr fhRetType fhName fhParams fhAttr1 fhSection
          fhAlign fhGc) = optSepToLlvm fhLinkage " " ++ optSepToLlvm fhVisibility " " ++
                          optSepToLlvm fhCCoonc " " ++ listToLlvm " " fhAttr " " " " ++
                          toLlvm fhRetType ++ " " ++ toLlvm fhName ++ " " ++ toLlvm fhParams ++
                          listToLlvm " " fhAttr1 " " " " ++
                          sepOptToLlvm " " fhSection ++ 
                          sepOptToLlvm " " fhAlign ++ 
                          sepOptToLlvm " " fhGc
  

