{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Llvm.Asm.Printer.SharedEntityPrint where
import Prelude (($),fmap, Maybe(..),maybe, (.),null,(++),error, show,fromIntegral,String,Integral)

import Llvm.Asm.Printer.Common 
import Llvm.Asm.SharedEntity
import Llvm.Util.Mapping (getValOrImplError)
import qualified Data.Map as M

class Print a where
  print :: a -> Doc

instance Print Endianness where
  print LittleEndian = char 'e'
  print BigEndian = char 'E'
  
instance Print LayoutAddrSpace where  
  print (LayoutAddrSpace n) = integral n
  print (LayoutAddrSpaceUnspecified) = empty
  
instance Print SizeInBit where  
  print (SizeInBit n) = integral n
  
instance Print AlignInBit where  
  print (AlignInBit n) = integral n
  
instance Print StackAlign where  
  print (StackAlign n) = print n
  print StackAlignUnspecified = char '0'
  
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
  

integral :: Integral a => a -> Doc
integral = integer . fromIntegral

instance Print LayoutSpec where  
  print ls = case ls of
      DlE x -> print x
      DlS x -> char 'S' <> (print x)
      DlLittleS s1 s2 s3 -> char 's' <> (maybe empty integral s1) 
                            <> sepMaybe integral colonSep s2
                            <> sepMaybe integral colonSep s3
      DlP as s a n -> char 'p' <> (print as) 
                      <> colonSep (print s) 
                      <> colonSep (print a) 
                      <> sepMaybe print colonSep n
      DlI s a n -> char 'i' <> (print s)
                   <> colonSep (print a)
                   <> sepMaybe print colonSep n
      DlF s a n -> char 'f' <> (print s)
                 <> colonSep (print a)
                 <> sepMaybe print colonSep n
      DlV s a n -> char 'v' <> (print s)
                   <> colonSep (print a)
                   <> sepMaybe print colonSep n
      DlA s a n -> char 'a' <> (maybe empty print s)
                   <> colonSep (print a)
                   <> sepMaybe print colonSep n
      DlM m -> char 'm' <> colonSep (print m)
      DlN l -> char 'n' <> (hcat $ punctuate (char ':') $ fmap print l)
    where 
      colonSep = (char ':' <>)
  
instance Print DataLayout where
  print (DataLayout l) = doubleQuotes (hcat $ punctuate (char '-') $ fmap print l)
  
instance Print IcmpOp where
  print x = text $ getValOrImplError (icmpOpMap, "icmpOpMap") x

instance Print FcmpOp where
  print x = text $ getValOrImplError (fcmpOpMap, "fcmpOpMap") x

{-
instance Print ConvertOp where
  print x = text $ getValOrImplError (convertOpMap, "convertOpMap") x
-}

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
  print CcWebkit_Js = text "webkit_jscc"
  print CcAnyReg = text "anyregcc"
  print CcPreserveMost = text "preserve_mostcc"
  print CcPreserveAll = text "preserve_allcc"
  print (Cc s) = text "cc" <> text s
  print CcSpir_Kernel = text "spir_kernel"
  print CcSpir_Func = text "spir_func"
  print CcIntel_Ocl_Bi = text "intel_ocl_bicc"
  print CcX86_StdCall = text "x86_stdcallcc"
  print CcX86_FastCall = text "x86_fastcallcc"
  print CcX86_ThisCall = text "x86_thiscallcc"
  print CcArm_Apcs = text "arm_apcscc"
  print CcArm_Aapcs = text "arm_aapcscc"
  print CcArm_Aapcs_Vfp = text "arm_aapcs_vfpcc"
  print CcMsp430_Intr = text "msp430_intrcc"
  print CcPtx_Kernel = text "ptx_kernel"
  print CcPtx_Device = text "ptx_device"
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

instance Print CallRetAttr where
  print CraZeroExt = text "zeroext"
  print CraSignExt = text "signext"
  print CraInReg = text "inreg"

instance Print CallFunAttr where
  print CfaNoreturn = text "noreturn"
  print CfaNounwind = text "nounwind"
  print CfaReadonly = text "readonly"
  print CfaReadnone = text "readnone"
  print CfaOptsize = text "optsize"
  print (CfaGroup n) = char '#'<> (integral n)  

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
  print (PaDereferenceable n) = (text "dereferenceable") <> (parens $ integral n)
  print PaReadOnly = text "readonly"
  print PaReadNone = text "readnone"
  print (PaAlign n) = text "align" <+> integral n


instance Print FunAttr where
  print (FaAlignStack n) = (text "alignstack") <> (parens $ integral n)
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
  print (FaAlign n) = text "align" <+> integral n
  print (FaGroup n) = char '#'<> (integral n)
  
instance Print SelectionKind where  
  print x = text $ getValOrImplError (selectionKindMap, "selectionKindMap") x
   
instance Print AddrNaming where
  print UnnamedAddr = text "unnamed_addr"
  print NamedAddr = empty

instance Print DqString where
  print (DqString x) = doubleQuotes $ text x

instance Print Section where
  print (Section s) = text "section" <+> (print s)

instance Print Alignment where
    print (Alignment s) = text "align" <+>  (integral s)

instance Print Gc where
    print (Gc s) = text "gc" <+> (print s)

instance Print GlobalType where
    print (GlobalType s) = text s


instance Print GlobalOrLocalId where
  print (GolG g) = print g
  print (GolL l) = print l
                    
instance Print LocalId where
  print (LocalIdNum s) = char '%'<>(integral s)
  print (LocalIdAlphaNum s) = char '%' <> text s
  print (LocalIdDqString s) = char '%' <> (doubleQuotes $ text s)
                      
instance Print GlobalId where
  print (GlobalIdNum s) = char '@'<>(integral s)
  print (GlobalIdAlphaNum s) = char '@'<> text s
  print (GlobalIdDqString s) = char '@'<> (doubleQuotes $ text s)

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
    CpZeroInitializer -> text "zeroinitializer"
    CpGlobalAddr g -> print g
    CpStr s -> char 'c'<> (doubleQuotes $ text s)
    CpBconst bc -> print bc
                          
instance Print BinaryConstant where
  print x = case x of
    BconstUint8 v -> integral v
    BconstUint16 v -> integral v
    BconstUint32 v -> integral v
    BconstUint64 v -> integral v
    BconstUint96 v -> integral v
    BconstUint128 v -> integral v
    BconstInt8 v -> integral v
    BconstInt16 v -> integral v
    BconstInt32 v -> integral v
    BconstInt64 v -> integral v
    BconstInt96 v -> integral v
    BconstInt128 v -> integral v

instance Print AtomicMemoryOrdering where
  print x = text $ getValOrImplError (atomicMemoryOrderingMap, "atomicMemoryOrderingMap") x

instance Print AtomicOp where
  print x = text $ getValOrImplError (atomicOpMap, "atomicOpMap") x
    
instance Print Fparam where
  print (FimplicitParam) = text "; implicit param\n"
  print (FexplicitParam x) = print x
  
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
  print (Nontemporal i) = char '!'<>(text "nontemporal") <+> char '!'<>(integral i)
  
instance Print InvariantLoad where
  print (InvariantLoad i) = char '!'<>(text "invariant.load") <+> char '!'<>(integral i)
  
instance Print Nonnull where
  print (Nonnull i) = char '!'<>(text "nonnull") <+> char '!'<>(integral i)
  
  
instance Print TailCall where
  print x = case x of
    TcNon -> empty
    TcTailCall -> text "tail"
    TcMustTailCall -> text "musttail"
    
instance Print DollarId where
  print (DollarIdNum n) = char '$' <> (integral n)
  print (DollarIdAlphaNum s) = char '$' <> (text s)
  print (DollarIdDqString s) = char '$' <> (doubleQuotes $ text s)
                                    
                        
instance Print Comdat where
  print (Comdat l) = text "comdat" <+> (maybe empty print l)
    
instance Print FastMathFlag where
  print x = text $ getValOrImplError (fastMathFlagMap, "fastMathFlagMap") x 
                
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
  
instance Print VarArgParam where
  print VarArgParam = text "..."
  
instance Print Cleanup where
  print Cleanup = text "cleanup"

instance Print Arch where
  print arch = case arch of
    Arch_i386 -> text "i386"
    Arch_i686 -> text "i686"
    Arch_x86 -> text "x86"
    Arch_x86_64 -> text "x86_64"
    Arch_PowerPc -> text "powerpc"
    Arch_PowerPc64 -> text "powerpc64"
    Arch_Arm s -> text "arm" <> text s
    Arch_ThumbV7 -> text "thumbv7"
    Arch_Itanium -> text "itanium"
    Arch_Mips s -> text "mips" <> text s
    Arch_String s -> text s
    
instance Print Vendor where
  print v = case v of
    Vendor_Pc -> text "pc"
    Vendor_Apple -> text "apple"
    Vendor_Unknown -> text "unknown"
    Vendor_String s -> text s
    
instance Print Os where    
  print v = case v of
    Os_Linux -> text "linux"
    Os_Windows -> text "windows"
    Os_Win32 -> text "win32"
    Os_Darwin s -> text "darwin" <> text s
    Os_FreeBsd s -> text "freebsd" <> text s
    Os_Macosx s -> text "macosx" <> text s
    Os_Ios s -> text "ios" <> text s
    Os_Mingw32 -> text "mingw32"
    Os_Unknown -> text "unknown"
    Os_String s -> text s
    
instance Print OsEnv where    
  print v = case v of
    OsEnv_Gnu -> text "gnu"
    OsEnv_String s -> text s

instance Print TargetTriple where
  print (TargetTriple arch ven os env) = 
    doubleQuotes (hcat $ punctuate (char '-') 
                  ([print arch] ++ mb ven ++ mb os ++ mb env))
    where mb v = maybe [] (\x -> [print x]) v

