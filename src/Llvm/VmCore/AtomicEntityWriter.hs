module Llvm.VmCore.AtomicEntityWriter where
import Data.List
import Llvm.VmCore.AtomicEntity
import Llvm.VmCore.AsmWriter

instance AsmWriter IcmpOp where
  toLlvm IcmpEq = "eq"
  toLlvm IcmpNe = "ne"
  toLlvm IcmpUgt = "ugt"
  toLlvm IcmpUge = "uge"
  toLlvm IcmpUlt = "ult"
  toLlvm IcmpUle = "ule"
  toLlvm IcmpSgt = "sgt"
  toLlvm IcmpSge = "sge"
  toLlvm IcmpSlt = "slt"
  toLlvm IcmpSle = "sle"

instance AsmWriter FcmpOp where
  toLlvm FcmpTrue = "true"
  toLlvm FcmpFalse = "false"
  toLlvm FcmpOeq = "oeq"
  toLlvm FcmpOgt = "ogt"
  toLlvm FcmpOge = "oge"
  toLlvm FcmpOlt = "olt"
  toLlvm FcmpOle = "ole"
  toLlvm FcmpOne = "one"
  toLlvm FcmpOrd = "ord"
  toLlvm FcmpUeq = "ueq"
  toLlvm FcmpUgt = "ugt"
  toLlvm FcmpUge = "uge"
  toLlvm FcmpUlt = "ult"
  toLlvm FcmpUle = "ule"
  toLlvm FcmpUne = "une"
  toLlvm FcmpUno = "uno"

instance AsmWriter ConvertOp where
  toLlvm Trunc = "trunc"
  toLlvm Zext = "zext"
  toLlvm Sext = "sext"
  toLlvm FpTrunc = "fptrunc"
  toLlvm FpExt = "fpext"
  toLlvm FpToUi = "fptoui"
  toLlvm FpToSi = "fptosi"
  toLlvm UiToFp = "uitofp"
  toLlvm SiToFp = "sitofp"
  toLlvm PtrToInt = "ptrtoint"
  toLlvm IntToPtr = "inttoptr"
  toLlvm Bitcast = "bitcast"

instance AsmWriter Linkage where
  toLlvm Private = "private"
  toLlvm LinkerPrivate = "linker_private"
  toLlvm LinkerPrivateWeak = "linker_private_weak"
  toLlvm LinkerPrivateWeakDefAuto = "linker_private_weak_def_auto"
  toLlvm Internal = "internal"
  toLlvm External = "external"
  toLlvm AvailableExternally = "available_externally"
  toLlvm Linkonce = "linkonce"
  toLlvm Weak = "weak"
  toLlvm Common = "common"
  toLlvm Appending = "appending"
  toLlvm ExternWeak = "extern_weak"
  toLlvm LinkonceOdr = "linkonce_odr"
  toLlvm WeakOdr = "weak_odr"
  toLlvm DllImport = "dllimport"
  toLlvm DllExport = "dllexport"

instance AsmWriter Visibility where
  toLlvm Default = "default"
  toLlvm Hidden = "hidden"
  toLlvm Protected = "protected"

instance AsmWriter CallConv where
  toLlvm Ccc = "ccc"
  toLlvm FastCc = "fastcc"
  toLlvm ColdCc = "coldcc"
  toLlvm X86StdCall = "x86_stdcallcc"
  toLlvm X86FastCall = "x86_fastcallcc"
  toLlvm X86ThisCall = "x86_thiscallcc"
  toLlvm ArmApcs = "arm_apcscc"
  toLlvm ArmAapcs = "arm_aapcscc"
  toLlvm ArmAapcsVfp = "arm_aapcs_vfpcc"
  toLlvm Msp430Intr = "msp430_intrcc"
  toLlvm (Cc s) = "cc" ++ s

instance AsmWriter ParamAttr where
  toLlvm ZeroExt = "zeroext"
  toLlvm SignExt = "signext"
  toLlvm InReg = "inreg"
  toLlvm ByVal = "byval"
  toLlvm SRet = "sret"
  toLlvm NoAlias = "noalias"
  toLlvm NoCapture = "nocapture"
  toLlvm Nest = "nest"
  -- toLlvm (Fattr fa) = toLlvm fa


instance AsmWriter FunAttr where
  toLlvm AddressSafety = "address_safety"
  toLlvm (AlignStack n) = "alignstack(" ++ (show n) ++ ")"
  toLlvm AlwaysInline = "alwaysinline"
  toLlvm NonLazyBind = "nonlazybind"
--  toLlvm HotPatch = "hotpatch"
  toLlvm InlineHint = "inlinehint"
  toLlvm Naked = "naked"
  toLlvm NoImplicitFloat = "noimplicitfloat"
  toLlvm NoInline = "noinline"
  toLlvm NoRedZone = "noredzone"
  toLlvm NoReturn = "noreturn"
  toLlvm NoUnwind = "nounwind"
  toLlvm OptSize = "optsize"
  toLlvm ReadNone = "readnone"
  toLlvm ReadOnly = "readonly"
  toLlvm ReturnsTwice = "returns_twice"
  toLlvm Ssp = "ssp"
  toLlvm SspReq = "sspreq"
--  toLlvm SideEffect = "sideeffect"
  toLlvm UwTable = "uwtable"
  toLlvm UnnamedAddr = "unnamed_addr"

instance AsmWriter QuoteStr where
  toLlvm (QuoteStr x) = "\"" ++ x ++ "\""

instance AsmWriter PlainStr where
  toLlvm (PlainStr x) = x

instance AsmWriter Section where
  toLlvm (Section s) = "section " ++ (toLlvm s)

{-
instance AsmWriter TargetKind where
  toLlvm Triple = "triple"
  toLlvm Datalayout = "datalayout"
-}

instance AsmWriter Align where
    toLlvm (Align s) = "align " ++ (show s)

instance AsmWriter Gc where
    toLlvm (Gc s) = "gc " ++ (toLlvm s)

instance AsmWriter GlobalType where
    toLlvm (GlobalType s) = s


instance AsmWriter TypePrimitive where
  toLlvm a = case a of 
    TpI i -> "i" ++ show i
    TpF f -> "f" ++ show f
    TpV v -> "v" ++ show v
    TpVoid -> "void"
    TpHalf -> "half"
    TpFloat -> "float"
    TpDouble -> "double"
    TpFp128 -> "fp128"
    TpX86Fp80 -> "x86_fp80"
    TpPpcFp128 -> "ppc_fp128"
    TpX86Mmx -> "x86_mmx"
    TpLabel -> "label"                             

instance AsmWriter Lstring where
  toLlvm (Lstring s) = s
  
instance AsmWriter GlobalOrLocalId where
  toLlvm (GolG g) = toLlvm g
  toLlvm (GolL l) = toLlvm l
                    
instance AsmWriter LocalId where
  toLlvm (LocalIdNum s) = '%':(show s)
  toLlvm (LocalIdAlphaNum s) = '%':toLlvm s
  toLlvm (LocalIdQuoteStr s) = '%':'"':toLlvm s ++ "\""
                      
instance AsmWriter GlobalId where
  toLlvm (GlobalIdNum s) = '@':(show s)
  toLlvm (GlobalIdAlphaNum s) = '@':toLlvm s
  toLlvm (GlobalIdQuoteStr s) = '@':'"':toLlvm s ++ "\""

  
instance AsmWriter SimpleConstant where
  toLlvm x = case x of
    CpInt i -> i
    CpUhexInt i -> "u0x" ++ i
    CpShexInt i -> "s0x" ++ i
    CpFloat s -> s
    CpNull -> "null"
    CpUndef -> "undef"
    CpTrue -> "true"
    CpFalse -> "false"
    CpZero -> "zeroinitializer"
    CpGlobalAddr g -> toLlvm g
    CpStr s -> ('c':'"':s)++"\""
                          
instance AsmWriter FenceOrder where
  toLlvm Acquire = "acquire"
  toLlvm Release = "release"
  toLlvm AcqRel =  "acq_rel"
  toLlvm SeqCst = "seq_cst"
  toLlvm Unordered = "unordered"
  toLlvm Monotonic = "monotonic"


instance AsmWriter AtomicOp where
    toLlvm Axchg = "xchg"
    toLlvm Aadd = "add"
    toLlvm Asub = "sub"
    toLlvm Aand = "and"
    toLlvm Anand = "nand"
    toLlvm Aor = "or"
    toLlvm Axor = "xor"
    toLlvm Amax = "max"
    toLlvm Amin = "min"
    toLlvm Aumax = "umax"
    toLlvm Aumin = "umin"                   
    
instance AsmWriter AddrSpace where    
  toLlvm (AddrSpace n) = "addrspace(" ++ show n ++ ")"
  toLlvm AddrSpaceUnspecified = ""
    
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
    Tstruct b ts -> (case b of 
                        Packed -> "<{"  
                        Unpacked -> "{") ++ 
                    listToLlvm "" ts ", " "" ++ 
                    (case b of
                        Packed -> "}>" 
                        Unpacked -> "}")
    Tpointer t addr -> toLlvm t ++ toLlvm addr ++ "*"
    Tfunction t fp atts -> toLlvm t ++ " " ++ toLlvm fp ++ listToLlvm " " atts ", " ""
    
    
    

instance AsmWriter Fparam where
  toLlvm (FimplicitParam) = "; implicit param\n"
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


instance AsmWriter TypeParamList where
  toLlvm (TypeParamList params b) =
    "(" ++ listToLlvm "" params ", " "" ++ 
    (if b then (if null params then "" else ", ") ++ "..." else "") ++ ")"