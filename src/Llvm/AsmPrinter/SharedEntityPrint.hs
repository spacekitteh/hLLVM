{-# LANGUAGE NoImplicitPrelude #-}
module Llvm.AsmPrinter.SharedEntityPrint where
import Prelude (($),fmap, Maybe(..),maybe, (.),null)

import Llvm.AsmPrinter.Common -- Text.PrettyPrint
import Llvm.VmCore.DataLayout
import Llvm.VmCore.SharedEntity


class Print a where
  print :: a -> Doc
  
{-  
sepOptToLlvmX :: Print a => (Doc -> Doc) -> Maybe a -> Doc
sepOptToLlvmX _  (Nothing) = empty
sepOptToLlvmX sep (Just x) = sep $ print x
-}  
  
instance Print Endianness where
  print LittleEndian = char 'e'
  print BigEndian = char 'E'
  
instance Print LayoutAddrSpace where  
  print (LayoutAddrSpace n) = integer n
  print (LayoutAddrSpaceUnspecified) = empty
  
instance Print SizeInBit where  
  print (SizeInBit n) = integer n
  
instance Print AlignInBit where  
  print (AlignInBit n) = integer n
  
instance Print StackAlign where  
  print (StackAlign n) = print n
  print StackAlignUnspecified = empty
  
instance Print Mangling where
  print x = case x of
    ManglingE -> char 'e'
    ManglingM -> char 'm'
    ManglingO -> char 'o'
    ManglingW -> char 'w'
    
instance Print AbiAlign where    
  print (AbiAlign n) = print n

instance Print PrefAlign where
  print (PrefAlign n) = print n
  
  

instance Print LayoutSpec where  
  print ls = case ls of
    DlE x -> print x
    DlS x -> char 'S' <> (print x)
    DlP ls s a n -> char 'p' <> (print ls) 
                    <> char ':' <> (print s) 
                    <> char ':' <> (print a) 
                    <> sepMaybe print (char ':' <>) n
    DlI s a n -> char 'i' <> (print s)
                 <> char ':' <> (print a)
                 <> sepMaybe print (char ':' <>) n
    DlF s a n -> char 'f' <> (print s)
                 <> char ':' <> (print a)
                 <> sepMaybe print (char ':' <>) n
    DlV s a n -> char 'v' <> (print s)
                 <> char ':' <> (print a)
                 <> sepMaybe print (char ':' <>) n
    DlA s a n -> char 'a' <> (maybe empty print s)
                 <> char ':' <> (print a)
                 <> sepMaybe print (char ':' <>) n
    DlM m -> char 'm' <> char ':' <> (print m)
    DlN l -> char 'n' <> (hcat $ punctuate (char ':') $ fmap print l)
  
instance Print DataLayout where
  print (DataLayout l) = doubleQuotes (hcat $ punctuate (char '-') $ fmap print l)
  
instance Print IcmpOp where
  print IcmpEq = text "eq"
  print IcmpNe = text "ne"
  print IcmpUgt = text "ugt"
  print IcmpUge = text "uge"
  print IcmpUlt = text "ult"
  print IcmpUle = text "ule"
  print IcmpSgt = text "sgt"
  print IcmpSge = text "sge"
  print IcmpSlt = text "slt"
  print IcmpSle = text "sle"

instance Print FcmpOp where
  print FcmpTrue = text "true"
  print FcmpFalse = text "false"
  print FcmpOeq = text "oeq"
  print FcmpOgt = text "ogt"
  print FcmpOge = text "oge"
  print FcmpOlt = text "olt"
  print FcmpOle = text "ole"
  print FcmpOne = text "one"
  print FcmpOrd = text "ord"
  print FcmpUeq = text "ueq"
  print FcmpUgt = text "ugt"
  print FcmpUge = text "uge"
  print FcmpUlt = text "ult"
  print FcmpUle = text "ule"
  print FcmpUne = text "une"
  print FcmpUno = text "uno"

instance Print ConvertOp where
  print Trunc = text "trunc"
  print Zext = text "zext"
  print Sext = text "sext"
  print FpTrunc = text "fptrunc"
  print FpExt = text "fpext"
  print FpToUi = text "fptoui"
  print FpToSi = text "fptosi"
  print UiToFp = text "uitofp"
  print SiToFp = text "sitofp"
  print PtrToInt = text "ptrtoint"
  print IntToPtr = text "inttoptr"
  print Bitcast = text "bitcast"
  print AddrSpaceCast = text "addrspacecast"

instance Print Linkage where
  print LinkagePrivate = text "private"
  print LinkageInternal = text "internal"
  print LinkageAvailableExternally = text "available_externally"
  print LinkageExternal = text "external"
  print LinkageLinkonce = text "linkonce"
  print LinkageWeak = text "weak"
  print LinkageCommon = text "common"
  print LinkageAppending = text "appending"
  print LinkageExternWeak = text "extern_weak"
  print LinkageLinkonceOdr = text "linkonce_odr"
  print LinkageWeakOdr = text "weak_odr"
  


instance Print CallConv where
  print Ccc = text "ccc"
  print CcFast = text "fastcc"
  print CcCold = text "coldcc"
  print CcWebkitJs = text "webkit_jscc"
  print CcAnyReg = text "anyregcc"
  print CcPreserveMost = text "preserve_mostcc"
  print CcPreserveAll = text "preserve_allcc"
  print (Cc s) = text "cc" <> text s
  print CcSpirKernel = text "spir_kernel"
  print CcSpirFunc = text "spir_func"
  print CcIntelOclBi = text "intel_ocl_bicc"
  print CcX86StdCall = text "x86_stdcallcc"
  print CcX86FastCall = text "x86_fastcallcc"
  print CcX86ThisCall = text "x86_thiscallcc"
  print CcArmApcs = text "arm_apcscc"
  print CcArmAapcs = text "arm_aapcscc"
  print CcArmAapcsVfp = text "arm_aapcs_vfpcc"
  print CcMsp430Intr = text "msp430_intrcc"
  print CcPtxKernel = text "ptx_kernel"
  print CcPtxDevice = text "ptx_device"
  print CcX86_64_Win64 = text "x86_64_win64cc"
  print CcX86_64_SysV = text "x86_64_sysvcc"

instance Print Visibility where
  print VisDefault = text "default"
  print VisHidden = text "hidden"
  print VisProtected = text "protected"


instance Print DllStorageClass where
  print DscImport = text "dllimport"
  print DscExport = text "dllexport"
  
instance Print ThreadLocalStorage where  
  print x = let d = case x of 
                   TlsLocalDynamic -> text "localdynamic"
                   TlsInitialExec -> text "initialexec"
                   TlsLocalExec -> text "localexec"
                   TlsNone -> empty
             in if isEmpty d then text "thread_local"
                else text "thread_local" <+> parens d 

instance Print ParamAttr where
  print PaZeroExt = text "zeroext"
  print PaSignExt = text "signext"
  print PaInReg = text "inreg"
  print PaByVal = text "byval"
  print PaInAlloca = text "inalloca"
  print PaSRet = text "sret"
  print PaNoAlias = text "noalias"
  print PaNoCapture = text "nocapture"
  print PaNest = text "nest"
  print PaReturned = text "returned"
  print PaNonNull = text "nonnull"
  print (PaDereferenceable n) = (text "dereferenceable") <> (parens $ integer n)
  print PaReadOnly = text "readonly"
  print PaReadNone = text "readnone"
  print (PaAlign n) = text "align" <+> integer n


instance Print FunAttr where
  print (FaAlignStack n) = (text "alignstack") <> (parens $ integer n)
  print FaAlwaysInline = text "alwaysinline"
  print FaBuiltin = text "builtin"
  print FaCold = text "cold"
  print FaInlineHint = text "inlinehint"
  print FaJumpTable = text "jumptable"
  print FaMinSize = text "minsize"
  print FaNaked = text "naked"
  print FaNoBuiltin = text "nobuiltin"
  print FaNoDuplicate = text "noduplicate"
  print FaNoImplicitFloat = text "noimplicitfloat"
  print FaNoInline = text "noinline"
  print FaNonLazyBind = text "nonlazybind"
  print FaNoRedZone = text "noredzone"
  print FaNoReturn = text "noreturn"
  print FaNoUnwind = text "nounwind"
  print FaOptNone = text "optnone"
  print FaOptSize = text "optsize"
  print FaReadNone = text "readnone"
  print FaReadOnly = text "readonly"
  print FaReturnsTwice = text "returns_twice"
  print FaSanitizeAddress = text "sanitize_address"
  print FaSanitizeMemory = text "sanitize_memory"
  print FaSanitizeThread = text "sanitize_thread"
  print FaSsp = text "ssp"
  print FaSspReq = text "sspreq"
  print FaSspStrong = text "sspstrong"
  print FaUwTable = text "uwtable"
  print (FaPair s1 s2) = print s1 <> (maybe empty ((equals<>) . print) s2)
  print (FaAlign n) = text "align" <+> integer n
  print (FaGroup n) = char '#'<> (integer n)
  
instance Print SelectionKind where  
  print Any = text "any"
  print ExactMatch = text "exactmatch"
  print Largest = text "largest"
  print NoDuplicates = text "noduplicates"
  print SameSize = text "samesize"
   
instance Print AddrNaming where
  print UnnamedAddr = text "unnamed_addr"
  print NamedAddr = empty

instance Print DqString where
  print (DqString x) = doubleQuotes $ text x

{-
instance Print PlainStr where
  print (PlainStr x) = text x
-}

instance Print Section where
  print (Section s) = text "section" <+> (print s)

instance Print Alignment where
    print (Alignment s) = text "align" <+>  (integer s)

instance Print Gc where
    print (Gc s) = text "gc" <+> (print s)

instance Print GlobalType where
    print (GlobalType s) = text s


instance Print TypePrimitive where
  print a = case a of 
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

instance Print Lstring where
  print (Lstring s) = text s
  
instance Print GlobalOrLocalId where
  print (GolG g) = print g
  print (GolL l) = print l
                    
instance Print LocalId where
  print (LocalIdNum s) = char '%'<>(integer s)
  print (LocalIdAlphaNum s) = char '%' <> print s
  print (LocalIdDqString s) = char '%' <> (doubleQuotes $ print s)
                      
instance Print GlobalId where
  print (GlobalIdNum s) = char '@'<>(integer s)
  print (GlobalIdAlphaNum s) = char '@'<>print s
  print (GlobalIdDqString s) = char '@'<> (doubleQuotes $ print s)

  
instance Print SimpleConstant where
  print x = case x of
    CpInt i -> text i
    CpUhexInt i -> text "u0x" <> (text i)
    CpShexInt i -> text "s0x" <> (text i)
    CpFloat s -> text s
    CpNull -> text "null"
    CpUndef -> text "undef"
    CpTrue -> text "true"
    CpFalse -> text "false"
    CpZero -> text "zeroinitializer"
    CpGlobalAddr g -> print g
    CpStr s -> char 'c'<> (doubleQuotes $ text s)
                          
instance Print AtomicMemoryOrdering where
  print AmoAcquire = text "acquire"
  print AmoRelease = text "release"
  print AmoAcqRel =  text "acq_rel"
  print AmoSeqCst = text "seq_cst"
  print AmoUnordered = text "unordered"
  print AmoMonotonic = text "monotonic"


instance Print AtomicOp where
    print AoXchg = text "xchg"
    print AoAdd = text "add"
    print AoSub = text "sub"
    print AoAnd = text "and"
    print AoNand = text "nand"
    print AoOr = text "or"
    print AoXor = text "xor"
    print AoMax = text "max"
    print AoMin = text "min"
    print AoUmax = text "umax"
    print AoUmin = text "umin"                   
    
instance Print AddrSpace where
  print (AddrSpace n) = text "addrspace" <+> (parens $ integer n)
  print AddrSpaceUnspecified = empty
    
instance Print Type where
  print a = case a of 
    Tprimitive tp -> print tp
    Tmetadata -> text "metadata"
    Topaque -> text "opaque"
    Tname s -> char '%' <> print s
    TquoteName s -> char '%'<> (doubleQuotes $ print s)
    Tno i -> char '%'<> integer i
    TupRef i -> char '\\'<> integer i
    Tarray i t -> brackets (integer i <+> char 'x' <+> print t)
    Tvector i t -> char '<' <> integer i <+> char 'x' <+> print t <> char '>'
    Tstruct b ts -> let (start, end) = case b of 
                          Packed -> (char '<', char '>')
                          Unpacked -> (empty, empty)
                    in start <+> braces (hsep $ punctuate comma $ fmap print ts) <+> end
  
    Tpointer t addr -> print t <+> print addr <+> text "*"
    Tfunction t fp atts -> print t <+> print fp <+> (hsep $ punctuate comma $ fmap print atts)
    
instance Print Fparam where
  print (FimplicitParam) = text "; implicit param\n"
  print (FexplicitParam x) = print x
  
instance Print FormalParam where
  print (FormalParam t att1 align id att2) =
    (print t) <+> (hsep $ fmap print att1) <> (maybe empty ((comma <+>) . print) align)
    <+> (print id) <+> (hsep $ fmap print att2)

instance Print FormalParamList where
  print (FormalParamList params b atts) =
    parens ((hsep $ punctuate comma $ fmap print params) <+> print b) <+> (hsep $ fmap print atts)

instance Print TypeParamList where
  print (TypeParamList params b) = parens ((hsep $ punctuate comma $ fmap print params) <+> print b)
    
instance Print InAllocaAttr where
  print s = case s of
    InAllocaAttr -> text "inalloca"
    
    
instance Print Volatile where
  print Volatile = text "volatile"

instance Print Weak where
  print Weak = text "weak"
  
instance Print SingleThread where
  print SingleThread = text "singlethread"

instance Print InBounds where
  print InBounds = text "inbounds"
  
instance Print a => Print (IsOrIsNot a) where
  print s = case s of
    Is x -> print x
    IsNot _ -> empty

instance Print Nontemporal where    
  print (Nontemporal i) = char '!'<>(text "nontemporal") <+> char '!'<>(integer i)
  
instance Print InvariantLoad where
  print (InvariantLoad i) = char '!'<>(text "invariant.load") <+> char '!'<>(integer i)
  
instance Print Nonnull where
  print (Nonnull i) = char '!'<>(text "nonnull") <+> char '!'<>(integer i)
  
  
instance Print TailCall where
  print x = case x of
    TcNon -> empty
    TcTailCall -> text "tail"
    TcMustTailCall -> text "musttail"
    
instance Print DollarId where
  print (DollarIdNum n) = char '$' <> (integer n)
  print (DollarIdAlphaNum s) = char '$' <> (print s)
  print (DollarIdDqString s) = char '$' <> (doubleQuotes $ print s)
                                    
                        
instance Print Comdat where
  print (Comdat l) = text "comdat" <+> (maybe empty print l)
  
  
instance Print FastMathFlag where  
  print x = let s = case x of
                   Fmfnnan -> "nnan"
                   Fmfninf -> "ninf"
                   Fmfnsz -> "nsz"
                   Fmfarcp -> "arcp"
                   Fmffast -> "fast"
             in text s
                
instance Print FastMathFlags where                
  print (FastMathFlags l) = hsep $ fmap print l
  
instance Print ExternallyInitialized where  
  print ExternallyInitialized = text "externally_initialized"
  
  
instance Print AsmDialect where  
  print x = case x of
    AsmDialectAtt -> empty
    AsmDialectIntel -> text "inteldialect"
  
  
instance Print SideEffect where
  print SideEffect = text "sideeffect"
  
instance Print AlignStack where  
  print AlignStack = text "alignstack"
  
{-  
instance Print DoubleQuotedString where  
  print (DoubleQuotedString s) = doubleQuotes $ text s
-}

instance Print VarArgParam where
  print VarArgParam = text "..."