module Llvm.VmCore.SharedEntityWriter where
import Data.List
import Llvm.VmCore.SharedEntity
import Llvm.VmCore.AsmWriter

instance AsmWriter IcmpOp where
  toLlvm IcmpEq = text "eq"
  toLlvm IcmpNe = text "ne"
  toLlvm IcmpUgt = text "ugt"
  toLlvm IcmpUge = text "uge"
  toLlvm IcmpUlt = text "ult"
  toLlvm IcmpUle = text "ule"
  toLlvm IcmpSgt = text "sgt"
  toLlvm IcmpSge = text "sge"
  toLlvm IcmpSlt = text "slt"
  toLlvm IcmpSle = text "sle"

instance AsmWriter FcmpOp where
  toLlvm FcmpTrue = text "true"
  toLlvm FcmpFalse = text "false"
  toLlvm FcmpOeq = text "oeq"
  toLlvm FcmpOgt = text "ogt"
  toLlvm FcmpOge = text "oge"
  toLlvm FcmpOlt = text "olt"
  toLlvm FcmpOle = text "ole"
  toLlvm FcmpOne = text "one"
  toLlvm FcmpOrd = text "ord"
  toLlvm FcmpUeq = text "ueq"
  toLlvm FcmpUgt = text "ugt"
  toLlvm FcmpUge = text "uge"
  toLlvm FcmpUlt = text "ult"
  toLlvm FcmpUle = text "ule"
  toLlvm FcmpUne = text "une"
  toLlvm FcmpUno = text "uno"

instance AsmWriter ConvertOp where
  toLlvm Trunc = text "trunc"
  toLlvm Zext = text "zext"
  toLlvm Sext = text "sext"
  toLlvm FpTrunc = text "fptrunc"
  toLlvm FpExt = text "fpext"
  toLlvm FpToUi = text "fptoui"
  toLlvm FpToSi = text "fptosi"
  toLlvm UiToFp = text "uitofp"
  toLlvm SiToFp = text "sitofp"
  toLlvm PtrToInt = text "ptrtoint"
  toLlvm IntToPtr = text "inttoptr"
  toLlvm Bitcast = text "bitcast"
  toLlvm AddrSpaceCast = text "addrspacecast"

instance AsmWriter Linkage where
--  toLlvm Private = "private"
  toLlvm LinkagePrivate = text "private"
--  toLlvm LinkerPrivateWeak = "linker_private_weak"
--  toLlvm LinkerPrivateWeakDefAuto = "linker_private_weak_def_auto"
  toLlvm Internal = text "internal"
  toLlvm AvailableExternally = text "available_externally"
  toLlvm External = text "external"
  toLlvm Linkonce = text "linkonce"
  toLlvm LinkageWeak = text "weak"
  toLlvm Common = text "common"
  toLlvm Appending = text "appending"
  toLlvm ExternWeak = text "extern_weak"
  toLlvm LinkonceOdr = text "linkonce_odr"
  toLlvm WeakOdr = text "weak_odr"
  


instance AsmWriter CallConv where
  toLlvm Ccc = text "ccc"
  toLlvm FastCc = text "fastcc"
  toLlvm ColdCc = text "coldcc"
  toLlvm WebkitJsCc = text "webkit_jscc"
  toLlvm AnyRegCc = text "anyregcc"
  toLlvm PreserveMostCc = text "preserve_mostcc"
  toLlvm PreserveAllCc = text "preserve_allcc"
  toLlvm (Cc s) = text "cc" <> text s
  toLlvm SpirKernel = text "spir_kernel"
  toLlvm SpirFunc = text "spir_func"
  toLlvm IntelOclBiCc = text "intel_ocl_bicc"
  toLlvm X86StdCallCc = text "x86_stdcallcc"
  toLlvm X86FastCallCc = text "x86_fastcallcc"
  toLlvm X86ThisCallCc = text "x86_thiscallcc"
  toLlvm ArmApcsCc = text "arm_apcscc"
  toLlvm ArmAapcsCc = text "arm_aapcscc"
  toLlvm ArmAapcsVfpCc = text "arm_aapcs_vfpcc"
  toLlvm Msp430IntrCc = text "msp430_intrcc"
  toLlvm PtxKernel = text "ptx_kernel"
  toLlvm PtxDevice = text "ptx_device"


instance AsmWriter Visibility where
  toLlvm Default = text "default"
  toLlvm Hidden = text "hidden"
  toLlvm Protected = text "protected"


instance AsmWriter DllStorage where
  toLlvm DllImport = text "dllimport"
  toLlvm DllExport = text "dllexport"
  
instance AsmWriter ThreadLocalStorage where  
  toLlvm x = let d = case x of 
                   TlsLocalDynamic -> text "localdynamic"
                   TlsInitialExec -> text "initialexec"
                   TlsLocalExec -> text "localexec"
                   TlsNone -> empty
             in if isEmpty d then text "thread_local"
                else text "thread_local" <+> parens d 

instance AsmWriter ParamAttr where
  toLlvm ZeroExt = text "zeroext"
  toLlvm SignExt = text "signext"
  toLlvm InReg = text "inreg"
  toLlvm ByVal = text "byval"
  toLlvm InAlloca = text "inalloca"
  toLlvm SRet = text "sret"
  toLlvm NoAlias = text "noalias"
  toLlvm NoCapture = text "nocapture"
  toLlvm Nest = text "nest"
  toLlvm Returned = text "returned"
  toLlvm NonNull = text "nonnull"
  toLlvm (Dereferenceable n) = (text "dereferenceable") <> (parens $ integer n)
  toLlvm PaReadOnly = text "readonly"
  toLlvm PaReadNone = text "readnone"
  toLlvm (PaAlign n) = text "align" <+> integer n


instance AsmWriter FunAttr where
  toLlvm (FaAlignStack n) = (text "alignstack") <> (parens $ integer n)
  toLlvm FaAlwaysInline = text "alwaysinline"
  toLlvm FaBuiltin = text "builtin"
  toLlvm FaCold = text "cold"
  toLlvm FaInlineHint = text "inlinehint"
  toLlvm FaJumpTable = text "jumptable"
  toLlvm FaMinSize = text "minsize"
  toLlvm FaNaked = text "naked"
  toLlvm FaNoBuiltin = text "nobuiltin"
  toLlvm FaNoDuplicate = text "noduplicate"
  toLlvm FaNoImplicitFloat = text "noimplicitfloat"
  toLlvm FaNoInline = text "noinline"
  toLlvm FaNonLazyBind = text "nonlazybind"
  toLlvm FaNoRedZone = text "noredzone"
  toLlvm FaNoReturn = text "noreturn"
  toLlvm FaNoUnwind = text "nounwind"
  toLlvm FaOptNone = text "optnone"
  toLlvm FaOptSize = text "optsize"
  toLlvm FaReadNone = text "readnone"
  toLlvm FaReadOnly = text "readonly"
  toLlvm FaReturnsTwice = text "returns_twice"
  toLlvm FaSanitizeAddress = text "sanitize_address"
  toLlvm FaSanitizeMemory = text "sanitize_memory"
  toLlvm FaSanitizeThread = text "sanitize_thread"
  toLlvm FaSsp = text "ssp"
  toLlvm FaSspReq = text "sspreq"
  toLlvm FaSspStrong = text "sspstrong"
  toLlvm FaUwTable = text "uwtable"
  toLlvm (FaPair s1 s2) = toLlvm s1 <> (maybe empty (\x -> equals <> toLlvm x) s2)
  toLlvm (FaAlign n) = text "align" <+> integer n
  
instance AsmWriter SelectionKind where  
  toLlvm Any = text "any"
  toLlvm ExactMatch = text "exactmatch"
  toLlvm Largest = text "largest"
  toLlvm NoDuplicates = text "noduplicates"
  toLlvm SameSize = text "samesize"
   
instance AsmWriter AddrNaming where
  toLlvm UnnamedAddr = text "unnamed_addr"
  toLlvm NamedAddr = empty

instance AsmWriter QuoteStr where
  toLlvm (QuoteStr x) = doubleQuotes $ text x

instance AsmWriter PlainStr where
  toLlvm (PlainStr x) = text x

instance AsmWriter Section where
  toLlvm (Section s) = text "section" <+> (toLlvm s)

instance AsmWriter Align where
    toLlvm (Align s) = text "align" <+>  (integer s)

instance AsmWriter Gc where
    toLlvm (Gc s) = text "gc" <+> (toLlvm s)

instance AsmWriter GlobalType where
    toLlvm (GlobalType s) = text s


instance AsmWriter TypePrimitive where
  toLlvm a = case a of 
    TpI i -> text "i" <> integer i
    TpF f -> text "f" <> integer f
    TpV v -> text "v" <> integer v
    TpVoid -> text "void"
    TpHalf -> text "half"
    TpFloat -> text "float"
    TpDouble -> text "double"
    TpFp128 -> text "fp128"
    TpX86Fp80 -> text "x86_fp80"
    TpPpcFp128 -> text "ppc_fp128"
    TpX86Mmx -> text "x86_mmx"
    TpLabel -> text "label"                             

instance AsmWriter Lstring where
  toLlvm (Lstring s) = text s
  
instance AsmWriter GlobalOrLocalId where
  toLlvm (GolG g) = toLlvm g
  toLlvm (GolL l) = toLlvm l
                    
instance AsmWriter LocalId where
  toLlvm (LocalIdNum s) = char '%'<>(integer s)
  toLlvm (LocalIdAlphaNum s) = char '%' <> toLlvm s
  toLlvm (LocalIdQuoteStr s) = char '%' <> (doubleQuotes $ toLlvm s)
                      
instance AsmWriter GlobalId where
  toLlvm (GlobalIdNum s) = char '@'<>(integer s)
  toLlvm (GlobalIdAlphaNum s) = char '@'<>toLlvm s
  toLlvm (GlobalIdQuoteStr s) = char '@'<> (doubleQuotes $ toLlvm s)

  
instance AsmWriter SimpleConstant where
  toLlvm x = case x of
    CpInt i -> text i
    CpUhexInt i -> text "u0x" <> (text i)
    CpShexInt i -> text "s0x" <> (text i)
    CpFloat s -> text s
    CpNull -> text "null"
    CpUndef -> text "undef"
    CpTrue -> text "true"
    CpFalse -> text "false"
    CpZero -> text "zeroinitializer"
    CpGlobalAddr g -> toLlvm g
    CpStr s -> char 'c'<> (doubleQuotes $ text s)
                          
instance AsmWriter FenceOrder where
  toLlvm Acquire = text "acquire"
  toLlvm Release = text "release"
  toLlvm AcqRel =  text "acq_rel"
  toLlvm SeqCst = text "seq_cst"
  toLlvm Unordered = text "unordered"
  toLlvm Monotonic = text "monotonic"


instance AsmWriter AtomicOp where
    toLlvm Axchg = text "xchg"
    toLlvm Aadd = text "add"
    toLlvm Asub = text "sub"
    toLlvm Aand = text "and"
    toLlvm Anand = text "nand"
    toLlvm Aor = text "or"
    toLlvm Axor = text "xor"
    toLlvm Amax = text "max"
    toLlvm Amin = text "min"
    toLlvm Aumax = text "umax"
    toLlvm Aumin = text "umin"                   
    
instance AsmWriter AddrSpace where
  toLlvm (AddrSpace n) = text "addrspace" <+> (parens $ integer n)
  toLlvm AddrSpaceUnspecified = empty
    
instance AsmWriter Type where
  toLlvm a = case a of 
    Tprimitive tp -> toLlvm tp
    Tmetadata -> text "metadata"
    Topaque -> text "opaque"
    Tname s -> char '%' <> toLlvm s
    TquoteName s -> char '%'<> (doubleQuotes $ toLlvm s)
    Tno i -> char '%'<> integer i
    TupRef i -> char '\\'<> integer i
    Tarray i t -> brackets (integer i <+> char 'x' <+> toLlvm t)
    Tvector i t -> char '<' <> integer i <+> char 'x' <+> toLlvm t <> char '>'
    Tstruct b ts -> let (start, end) = case b of 
                          Packed -> (char '<', char '>')
                          Unpacked -> (empty, empty)
                    in start <+> braces (hsep $ punctuate comma $ fmap toLlvm ts) <+> end
  
    Tpointer t addr -> toLlvm t <+> toLlvm addr <+> text "*"
    Tfunction t fp atts -> toLlvm t <+> toLlvm fp <+> (hsep $ punctuate comma $ fmap toLlvm atts)
    
    
    

instance AsmWriter Fparam where
  toLlvm (FimplicitParam) = text "; implicit param\n"
  toLlvm (FexplicitParam x) = toLlvm x
  

instance AsmWriter FormalParam where
  toLlvm (FormalParam t att1 align id att2) =
    (toLlvm t) <+> ((listToDoc toLlvm att1 (<+>))) <+> (sepOptToLlvm empty align)
    <+> (toLlvm id) <+> (listToDoc toLlvm att2 (<+>))

instance AsmWriter FormalParamList where
  toLlvm (FormalParamList params b atts) =
    parens ((hsep $ punctuate comma $ fmap toLlvm params) <+>
            (if b then (if null params then empty else comma) <+> text "..." else empty))
    <+> (listToDoc toLlvm atts (<+>))


instance AsmWriter TypeParamList where
  toLlvm (TypeParamList params b) =
    parens ((hsep $ punctuate comma $ fmap toLlvm params) <+>
            (if b then (if null params then empty else comma) <+> text "..." else empty))
    
instance AsmWriter InAllocaAttr where
  toLlvm s = case s of
    InAllocaAttr -> text "inalloca"
    
    
instance AsmWriter Volatile where
  toLlvm Volatile = text "volatile"

instance AsmWriter Weak where
  toLlvm Weak = text "weak"
  
instance AsmWriter SingleThread where
  toLlvm SingleThread = text "singlethread"

instance AsmWriter InBounds where
  toLlvm InBounds = text "inbounds"
  
instance AsmWriter a => AsmWriter (IsOrIsNot a) where
  toLlvm s = case s of
    Is x -> toLlvm x
    IsNot x -> empty

instance AsmWriter Nontemporal where    
  toLlvm (Nontemporal i) = char '!'<>(text "nontemporal") <+> char '!'<>(integer i)
  
instance AsmWriter InvariantLoad where
  toLlvm (InvariantLoad i) = char '!'<>(text "invariant.load") <+> char '!'<>(integer i)
  
instance AsmWriter Nonnull where
  toLlvm (Nonnull i) = char '!'<>(text "nonnull") <+> char '!'<>(integer i)
  
  
instance AsmWriter TailCalling where
  toLlvm x = case x of
    NonTailCall -> empty
    TailCall -> text "tail"
    MustTailCall -> text "musttail"
    
{-    
instance AsmWriter AddrNaming where    
  toLlvm x = case x of
    UnnamedAddr -> text "unnamed_addr"
    NamedAddr -> empty
-}

instance AsmWriter FunAttrCollection where
  toLlvm x = case x of
    FunAttrList l -> hsep $ fmap toLlvm l
    FunAttrGroup n -> char '#' <> (integer n)
    
    
instance AsmWriter DollarId where
  toLlvm (DollarId n) = char '$' <> (text n)
                                    
                                    
                        
instance AsmWriter Comdat where
  toLlvm (Comdat l) = text "comdat" <+> (maybe empty toLlvm l)
  
  
instance AsmWriter FastMathFlag where  
  toLlvm x = let s = case x of
                   Fmfnnan -> "nnan"
                   Fmfninf -> "ninf"
                   Fmfnsz -> "nsz"
                   Fmfarcp -> "arcp"
                   Fmffast -> "fast"
             in text s
                
instance AsmWriter FastMathFlags where                
  toLlvm (FastMathFlags l) = hsep $ fmap toLlvm l
  
instance AsmWriter ExternallyInitialized where  
  toLlvm ExternallyInitialized = text "externally_initialized"
  
  
instance AsmWriter AsmDialect where  
  toLlvm x = case x of
    AsmDialectAtt -> empty
    AsmDialectIntel -> text "inteldialect"
