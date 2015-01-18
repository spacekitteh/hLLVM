module Llvm.VmCore.SharedEntity where
import qualified Data.Map as M

-- | Double Quoted String
data DqString = DqString String deriving (Eq, Ord, Show)

data IcmpOp = IcmpEq | IcmpNe | IcmpUgt | IcmpUge | IcmpUlt 
            | IcmpUle | IcmpSgt | IcmpSge | IcmpSlt | IcmpSle
            deriving (Eq,Ord,Show)

icmpOpMap :: M.Map IcmpOp String
icmpOpMap = M.fromList [(IcmpEq, "eq"), (IcmpNe, "ne"), (IcmpUgt, "ugt"),(IcmpUge, "uge"), (IcmpUlt, "ult")
                       ,(IcmpUle, "ule") ,(IcmpSgt, "sgt"), (IcmpSge, "sge"), (IcmpSlt, "slt"),(IcmpSle, "sle")]

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
                       ,(FcmpUeq, "ueq"), (FcmpUgt, "ugt"), (FcmpUge, "uge"), (FcmpUlt, "ult") 
                       ,(FcmpUle, "ule"), (FcmpUne, "une"), (FcmpUno, "uno") 
                       ]

data ConvertOp = Trunc | Zext | Sext | FpTrunc | FpExt | FpToUi 
               | FpToSi | UiToFp | SiToFp | PtrToInt | IntToPtr
               | Bitcast | AddrSpaceCast
               deriving (Eq,Ord,Show)

convertOpMap :: M.Map ConvertOp String
convertOpMap = M.fromList [(Trunc, "trunc"), (Zext, "zext"), (Sext, "sext")
                          ,(FpTrunc, "fptrunc"), (FpExt, "fpext"), (FpToUi, "fptoui") 
                          ,(FpToSi, "fptosi"), (UiToFp, "uitofp"), (SiToFp, "sitofp")
                          ,(PtrToInt, "ptrtoint"), (IntToPtr, "inttoptr"), (Bitcast, "bitcast")
                          ,(AddrSpaceCast, "addrspacecast")]

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
               | PaDereferenceable Integer
               | PaReadOnly
               | PaReadNone
               | PaAlign Integer
                 deriving (Eq,Ord,Show)

-- | Function Attributes <http://llvm.org/releases/3.5.0/docs/LangRef.html#function-attributes>
data FunAttr = FaAlignStack Integer
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
             | FaAlign Integer
             | FaGroup Integer
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

data Alignment = Alignment Integer deriving (Eq,Ord,Show)
data Gc = Gc DqString deriving (Eq,Ord,Show)
data GlobalType = GlobalType String deriving (Eq,Ord,Show)
data AddrSpace = AddrSpace Integer 
               | AddrSpaceUnspecified deriving (Eq,Ord,Show)
                                            

data AddrNaming = UnnamedAddr
                | NamedAddr deriving (Eq, Ord, Show)

data TypePrimitive = TpI Integer
                   | TpF Integer 
                   | TpV Integer 
                   | TpVoid 
                   | TpHalf | TpFloat | TpDouble | TpFp128 | TpX86Fp80 | TpPpcFp128 
                   | TpX86Mmx 
                   | TpNull
                   | TpLabel
                   deriving (Eq,Ord,Show)

data GlobalOrLocalId = GolG GlobalId
                     | GolL LocalId
                       deriving (Eq,Ord,Show)
                    
newtype Lstring = Lstring String deriving (Eq,Ord,Show)                                

data LocalId = LocalIdNum Integer
             | LocalIdAlphaNum Lstring
             | LocalIdDqString Lstring
             deriving (Eq,Ord,Show)
                      
data DollarId = DollarIdNum Integer
              | DollarIdAlphaNum Lstring 
              | DollarIdDqString Lstring 
              deriving (Eq, Ord, Show)                      

data Comdat = Comdat (Maybe DollarId) deriving (Eq, Ord, Show)

-- | this implies %1 == %"1" when localId is converted to Lstring, I'm not 100% sure this is correct
localIdToLstring :: LocalId -> Lstring                      
localIdToLstring (LocalIdNum s) = Lstring $ show s
localIdToLstring (LocalIdAlphaNum s) = s
localIdToLstring (LocalIdDqString s) = s
                                            
data GlobalId = GlobalIdNum Integer
              | GlobalIdAlphaNum Lstring
              | GlobalIdDqString Lstring
              deriving (Eq,Ord,Show)
                       
globalIdToLstring :: GlobalId -> Lstring                       
globalIdToLstring (GlobalIdNum s) = Lstring $ show s
globalIdToLstring (GlobalIdAlphaNum s) = s
globalIdToLstring (GlobalIdDqString s) = s

-- | Atomic Memory Ordering Constraints <http://llvm.org/releases/3.5.0/docs/LangRef.html#atomic-memory-ordering-constraints>
data AtomicMemoryOrdering = AmoAcquire | AmoRelease | AmoAcqRel | AmoSeqCst 
                          | AmoUnordered | AmoMonotonic
                          deriving (Eq,Ord,Show)
                                   
atomicMemoryOrderingMap :: M.Map AtomicMemoryOrdering String                                   
atomicMemoryOrderingMap = M.fromList [(AmoAcquire, "acquire"), (AmoRelease, "release"), (AmoAcqRel, "acq_rel"), (AmoSeqCst, "seq_cst")
                                     ,(AmoUnordered, "unordered"), (AmoMonotonic, "monotonic")] 

-- | atomicrmw operation <http://llvm.org/releases/3.5.0/docs/LangRef.html#id175>
data AtomicOp = AoXchg | AoAdd | AoSub | AoAnd | AoNand | AoOr | AoXor 
              | AoMax | AoMin | AoUmax | AoUmin
              deriving (Eq,Ord,Show)
                       
atomicOpMap :: M.Map AtomicOp String                       
atomicOpMap = M.fromList [(AoXchg, "xchg"), (AoAdd, "add"), (AoSub, "sub"), (AoAnd, "and"), (AoNand, "nand"), (AoOr, "or"), (AoXor, "xor")
                         ,(AoMax, "max"), (AoMin, "min"), (AoUmax, "umax"), (AoUmin, "umin")]
                                                                                                                            
                         
{- syntatical representation: c"...." -}                         
{-- data Cstring = Cstring String deriving (Eq,Ord,Show) -}

                    
-- | Simple Constants <http://llvm.org/releases/3.0/docs/LangRef.html#simpleconstants>  
data SimpleConstant = CpInt String
                    | CpUhexInt String
                    | CpShexInt String
                    | CpFloat String
                    -- | null pointer constant
                    | CpNull
                    | CpUndef
                    -- | true::i1 
                    | CpTrue
                    -- | false::i1
                    | CpFalse
                    | CpZero
                    | CpGlobalAddr GlobalId
                    | CpStr String
                    deriving (Eq, Ord, Show)
                             
data Atomicity = Atomicity (IsOrIsNot SingleThread) AtomicMemoryOrdering
               deriving (Eq, Ord, Show)
                        
data Nontemporal = Nontemporal Integer deriving (Eq, Ord, Show)
                          
data InvariantLoad = InvariantLoad Integer deriving (Eq, Ord, Show)
                            
data Nonnull = Nonnull Integer deriving (Eq, Ord, Show)
                             
data Volatile = Volatile deriving (Eq, Ord, Show)

data SingleThread = SingleThread deriving (Eq, Ord, Show)

data InAllocaAttr = InAllocaAttr deriving (Eq, Ord, Show)
  
data TailCall = TcNon
              | TcTailCall
              | TcMustTailCall deriving (Eq, Ord, Show)

isVoidType :: Type -> Bool
isVoidType (Tprimitive TpVoid) = True
isVoidType (Tfunction t _ _) = isVoidType t
isVoidType (Tpointer t _) = isVoidType t
isVoidType _ = False
 
data Packing = Packed                
             | Unpacked
             deriving (Eq, Ord, Show)
                      
data Type = Tprimitive TypePrimitive
          | Tmetadata
          | Topaque 
          | Tname Lstring
          | TquoteName Lstring
          | Tno Integer
          | TupRef Integer
          | Tarray Integer Type
          | Tvector Integer Type
          | Tstruct Packing [Type]
          | Tpointer Type AddrSpace
          | Tfunction Type TypeParamList [FunAttr]
          -- | deref a type will strip off Tpointer, this is a syntatical
          -- | representation that should be evaluated later.
          | Tderef Type 
          deriving (Eq,Ord,Show)
                   
data Fparam = FimplicitParam 
            | FexplicitParam LocalId 
            deriving (Eq,Ord,Show)
              
data FormalParam = FormalParam Type [ParamAttr] (Maybe Alignment) Fparam [ParamAttr]
                 deriving (Eq,Ord,Show)

data FormalParamList = FormalParamList [FormalParam] (Maybe VarArgParam) [FunAttr] deriving (Eq,Ord,Show)

data TypeParamList = TypeParamList [Type] (Maybe VarArgParam) deriving (Eq,Ord,Show)

data Weak = Weak deriving (Eq, Ord, Show)


data IsOrIsNot a = Is a
                 | IsNot a
                 deriving (Eq, Ord, Show)
                          
data InBounds = InBounds deriving (Eq, Ord, Show)

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

data VarArgParam = VarArgParam deriving (Eq, Ord, Show)

data Cleanup = Cleanup deriving (Eq, Ord, Show)


