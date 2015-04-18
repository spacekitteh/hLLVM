module Llvm.Data.Shared.AtomicEntity where
import qualified Data.Map as M
import Data.Word(Word32)

-- | Double Quoted String
data DqString = DqString String deriving (Eq, Ord, Show)

data IcmpOp = IcmpEq | IcmpNe | IcmpUgt | IcmpUge | IcmpUlt
            | IcmpUle | IcmpSgt | IcmpSge | IcmpSlt | IcmpSle
            deriving (Eq,Ord,Show)

icmpOpMap :: M.Map IcmpOp String
icmpOpMap = M.fromList [(IcmpEq, "eq"), (IcmpNe, "ne")
                       ,(IcmpUgt, "ugt"),(IcmpUge, "uge")
                       ,(IcmpUlt, "ult"),(IcmpUle, "ule")
                       ,(IcmpSgt, "sgt"), (IcmpSge, "sge")
                       ,(IcmpSlt, "slt"),(IcmpSle, "sle")
                       ]

data FcmpOp = FcmpTrue | FcmpFalse
            | FcmpOeq | FcmpOgt | FcmpOge
            | FcmpOlt | FcmpOle | FcmpOne | FcmpOrd
            | FcmpUeq | FcmpUgt | FcmpUge | FcmpUlt
            | FcmpUle | FcmpUne | FcmpUno
            deriving (Eq,Ord,Show)

fcmpOpMap :: M.Map FcmpOp String
fcmpOpMap = M.fromList [(FcmpTrue, "true"), (FcmpFalse, "false")
                       ,(FcmpOeq, "oeq"), (FcmpOgt, "ogt"), (FcmpOge, "oge")
                       ,(FcmpOlt, "olt"), (FcmpOle, "ole"), (FcmpOne, "one"), (FcmpOrd, "ord")
                       ,(FcmpUeq, "ueq"), (FcmpUgt, "ugt"), (FcmpUge, "uge")
                       ,(FcmpUlt, "ult"), (FcmpUle, "ule"), (FcmpUne, "une"), (FcmpUno, "uno")
                       ]

-- | Linkage Types <http://llvm.org/releases/3.5.0/docs/LangRef.html#linkage-types>
data Linkage = LinkagePrivate
             | LinkageInternal
             | LinkageAvailableExternally
             | LinkageLinkonce
             | LinkageCommon
             | LinkageWeak
             | LinkageAppending
             | LinkageExternWeak
             | LinkageLinkonceOdr
             | LinkageWeakOdr
             | LinkageExternal
             deriving (Eq,Ord,Show)

-- | Calling Conventions <http://llvm.org/releases/3.5.0/docs/LangRef.html#calling-conventions>
data CallConv = Ccc
              | CcFast
              | CcCold
              | CcGhc
              | CcHiPe
              | Cc String
              | CcWebkit_Js
              | CcAnyReg
              | CcPreserveMost
              | CcPreserveAll
              | CcFirstTarget
              | CcX86_StdCall
              | CcX86_FastCall
              | CcArm_Apcs
              | CcArm_Aapcs
              | CcArm_Aapcs_Vfp
              | CcMsp430_Intr
              | CcX86_ThisCall
              | CcPtx_Kernel
              | CcPtx_Device
              | CcSpir_Func
              | CcSpir_Kernel
              | CcIntel_Ocl_Bi
              | CcX86_64_SysV
              | CcX86_64_Win64
              | CcX86_VectorCall
              deriving (Eq,Ord,Show)

-- | Visibility Styles <http://llvm.org/releases/3.5.0/docs/LangRef.html#visibility-styles>
data Visibility = VisDefault | VisHidden | VisProtected
                  deriving (Eq,Ord,Show)

-- | DLL Storage Classes <http://llvm.org/releases/3.5.0/docs/LangRef.html#dll-storage-classes>
data DllStorageClass = DscImport
                     | DscExport deriving (Eq, Ord, Show)

-- | Thread Local Storage Models <http://llvm.org/releases/3.5.0/docs/LangRef.html#thread-local-storage-models>
data ThreadLocalStorage = TlsLocalDynamic
                        | TlsInitialExec
                        | TlsLocalExec
                        | TlsNone
                        deriving (Eq, Ord, Show)

-- | Parameter Attributes <http://llvm.org/releases/3.5.0/docs/LangRef.html#parameter-attributes>
data ParamAttr = PaZeroExt
               | PaSignExt
               | PaInReg
               | PaByVal
               | PaInAlloca
               | PaSRet
               | PaNoAlias
               | PaNoCapture
               | PaNest
               | PaReturned
               | PaNonNull
               | PaDereferenceable Word32 -- Integer
               | PaReadOnly
               | PaReadNone
               | PaAlign Word32 -- Integer
                 deriving (Eq,Ord,Show)

data CallRetAttr = CraZeroExt
                 | CraSignExt
                 | CraInReg
                 deriving (Eq, Ord, Show)

data CallFunAttr = CfaNoreturn
                 | CfaNounwind
                 | CfaReadonly
                 | CfaReadnone
                 | CfaOptsize
                 | CfaGroup Word32
                 deriving (Eq, Ord, Show)

-- | Function Attributes <http://llvm.org/releases/3.5.0/docs/LangRef.html#function-attributes>
data FunAttr = FaAlignStack Word32 -- Integer
             | FaAlwaysInline
             | FaBuiltin
             | FaCold
             | FaInlineHint
             | FaJumpTable
             | FaMinSize
             | FaNaked
             | FaNoBuiltin
             | FaNoDuplicate
             | FaNoImplicitFloat
             | FaNoInline
             | FaNonLazyBind
             | FaNoRedZone
             | FaNoReturn
             | FaNoUnwind
             | FaOptNone
             | FaOptSize
             | FaReadNone
             | FaReadOnly
             | FaReturnsTwice
             | FaSanitizeAddress
             | FaSanitizeMemory
             | FaSanitizeThread
             | FaSsp
             | FaSspReq
             | FaSspStrong
             | FaUwTable
             | FaPair DqString (Maybe DqString)
             | FaAlign Word32 -- Integer
             | FaGroup Word32 -- Integer
             deriving (Eq,Ord,Show)


data SelectionKind = Any
                   | ExactMatch
                   | Largest
                   | NoDuplicates
                   | SameSize
                   deriving (Eq, Ord, Show)

selectionKindMap :: M.Map  SelectionKind String
selectionKindMap = M.fromList [(Any, "any"), (ExactMatch, "exactmatch"), (Largest, "largest"), (NoDuplicates, "noduplicates"), (SameSize, "samesize")]

data Section = Section DqString deriving (Eq,Ord,Show)

data Alignment = Alignment Word32 deriving (Eq,Ord,Show)
type MaybeAlignment = Maybe Alignment

data Gc = Gc DqString deriving (Eq,Ord,Show)
data GlobalType = GlobalType String deriving (Eq,Ord,Show)

data AddrNaming = UnnamedAddr
                | NamedAddr deriving (Eq, Ord, Show)


data GlobalOrLocalId = GolG GlobalId
                     | GolL LocalId
                       deriving (Eq,Ord,Show)

data LocalId = LocalIdNum Word32 -- Integer
             | LocalIdAlphaNum String
             | LocalIdDqString String
             deriving (Eq,Ord,Show)


data DollarId = DollarIdNum Word32
              | DollarIdAlphaNum String
              | DollarIdDqString String
              deriving (Eq, Ord, Show)

data Comdat = Comdat (Maybe DollarId) deriving (Eq, Ord, Show)

data GlobalId = GlobalIdNum Word32
              | GlobalIdAlphaNum String
              | GlobalIdDqString String
              deriving (Eq,Ord,Show)

-- | Atomic Memory Ordering Constraints <http://llvm.org/releases/3.5.0/docs/LangRef.html#atomic-memory-ordering-constraints>
data AtomicMemoryOrdering = AmoAcquire | AmoRelease | AmoAcqRel
                          | AmoSeqCst | AmoUnordered | AmoMonotonic
                          deriving (Eq,Ord,Show)

atomicMemoryOrderingMap :: M.Map AtomicMemoryOrdering String
atomicMemoryOrderingMap = M.fromList [(AmoAcquire, "acquire"), (AmoRelease, "release")
                                     ,(AmoAcqRel, "acq_rel"), (AmoSeqCst, "seq_cst")
                                     ,(AmoUnordered, "unordered"), (AmoMonotonic, "monotonic")
                                     ]

-- | atomicrmw operation <http://llvm.org/releases/3.5.0/docs/LangRef.html#id175>
data AtomicOp = AoXchg | AoAdd | AoSub | AoAnd | AoNand
              | AoOr | AoXor
              | AoMax | AoMin | AoUmax | AoUmin
              deriving (Eq,Ord,Show)

atomicOpMap :: M.Map AtomicOp String
atomicOpMap = M.fromList [(AoXchg, "xchg"), (AoAdd, "add"), (AoSub, "sub")
                         ,(AoAnd, "and"), (AoNand, "nand")
                         ,(AoOr, "or"), (AoXor, "xor")
                         ,(AoMax, "max"), (AoMin, "min"), (AoUmax, "umax"), (AoUmin, "umin")]


{- syntatical representation: c"...." -}
{-- data Cstring = Cstring String deriving (Eq,Ord,Show) -}


data Atomicity = Atomicity (IsOrIsNot SingleThread) AtomicMemoryOrdering
               deriving (Eq, Ord, Show)

data Nontemporal = Nontemporal Word32 deriving (Eq, Ord, Show)
type MaybeNontemporal = Maybe Nontemporal

data InvariantLoad = InvariantLoad Word32 deriving (Eq, Ord, Show)

data Nonnull = Nonnull Word32 deriving (Eq, Ord, Show)

data Volatile = Volatile deriving (Eq, Ord, Show)

data SingleThread = SingleThread deriving (Eq, Ord, Show)

data InAllocaAttr = InAllocaAttr deriving (Eq, Ord, Show)

data TailCall = TcNon
              | TcTailCall
              | TcMustTailCall deriving (Eq, Ord, Show)

data Weak = Weak deriving (Eq, Ord, Show)


data IsOrIsNot a = Is a
                 | IsNot a
                 deriving (Eq, Ord, Show)

data InBounds = InBounds deriving (Eq, Ord, Show)

{- short hand notations -}
isInBounds :: IsOrIsNot InBounds
isInBounds = Is InBounds

isNotInBounds :: IsOrIsNot InBounds
isNotInBounds = IsNot InBounds

data FastMathFlag = Fmfnnan
                  | Fmfninf
                  | Fmfnsz
                  | Fmfarcp
                  | Fmffast
                  deriving (Eq, Ord, Show)

fastMathFlagMap :: M.Map FastMathFlag String
fastMathFlagMap = M.fromList [(Fmfnnan, "nnan"), (Fmfninf, "ninf"), (Fmfnsz, "nsz"), (Fmfarcp, "arcp"), (Fmffast, "fast")]

data FastMathFlags = FastMathFlags [FastMathFlag] deriving (Eq, Ord, Show)

data ExternallyInitialized = ExternallyInitialized deriving (Eq, Ord, Show)

data AsmDialect = AsmDialectAtt
                | AsmDialectIntel
                deriving (Eq, Ord, Show)

data SideEffect = SideEffect deriving (Eq, Ord, Show)
data AlignStack = AlignStack deriving (Eq, Ord, Show)

data Cleanup = Cleanup deriving (Eq, Ord, Show)


data Arch = Arch_i386
          | Arch_i686
          | Arch_x86
          | Arch_x86_64
          | Arch_PowerPc
          | Arch_PowerPc64
          | Arch_Arm String
          | Arch_ThumbV7
          | Arch_Itanium
          | Arch_Mips String
          | Arch_String String
          deriving (Eq, Ord, Show)

data Vendor = Vendor_Pc
            | Vendor_Apple
            | Vendor_Unknown
            | Vendor_String String
            deriving (Eq, Ord, Show)

data Os = Os_Linux
        | Os_Windows
        | Os_Win32
        | Os_FreeBsd String
        | Os_Darwin String
        | Os_Macosx String
        | Os_Ios String
        | Os_Mingw32
        | Os_Unknown
        | Os_String String
        deriving (Eq, Ord, Show)

data OsEnv = OsEnv_Gnu
           | OsEnv_String String
           deriving (Eq, Ord, Show)

data TargetTriple = TargetTriple Arch (Maybe Vendor) (Maybe Os) (Maybe OsEnv) deriving (Eq, Ord, Show)


data Fparam = FimplicitParam
            | FexplicitParam LocalId
            deriving (Eq,Ord,Show)

data Packing = Packed
             | Unpacked
             deriving (Eq, Ord, Show)

data VarArgParam = VarArgParam deriving (Eq, Ord, Show)
