module Llvm.VmCore.AtomicEntity where

data IcmpOp = IcmpEq | IcmpNe | IcmpUgt | IcmpUge | IcmpUlt 
            | IcmpUle | IcmpSgt | IcmpSge | IcmpSlt | IcmpSle
              deriving (Eq,Ord,Show)
                     
data FcmpOp = FcmpTrue | FcmpFalse
            | FcmpOeq | FcmpOgt | FcmpOge
            | FcmpOlt | FcmpOle | FcmpOne | FcmpOrd
            | FcmpUeq | FcmpUgt | FcmpUge | FcmpUlt
            | FcmpUle | FcmpUne | FcmpUno
              deriving (Eq,Ord,Show)
                     
data ConvertOp = Trunc | Zext | Sext | FpTrunc | FpExt | FpToUi 
               | FpToSi | UiToFp | SiToFp | PtrToInt | IntToPtr
               | Bitcast
                 deriving (Eq,Ord,Show)

data Linkage = 
    -- | private <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_private>
    Private
    -- | linker_private <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_linker_private>
    | LinkerPrivate
    -- | linker_private_weak <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_linker_private_weak>
    | LinkerPrivateWeak
    -- | linker_private_weak_def_auto <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_linker_private_weak_def_auto>
    | LinkerPrivateWeakDefAuto
    -- | internal <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_internal>
    | Internal    
    -- | available_externally <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_available_externally>
    | AvailableExternally
    -- | link_once <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_linkonce>
    | Linkonce
    -- | common <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_common>
    | Common
    -- | weak <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_weak>
    | Weak    
    -- | appending <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_appending>
    | Appending
    -- | extern_weak <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_extern_weak>
    | ExternWeak
    -- | linkonce_odr <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_linkonce_odr>
    | LinkonceOdr
    -- | weak_odr <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_weak_odr>
    | WeakOdr
    -- | external <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_external>
    | External
    -- | dllimport <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_dllimport>
    | DllImport
    -- | dllexport <http://llvm.org/releases/3.0/docs/LangRef.html#linkage_dllexport>
    | DllExport
      deriving (Eq,Ord,Show)

-- | Visibility Styles <http://llvm.org/releases/3.0/docs/LangRef.html#visibility>
data Visibility = Default | Hidden | Protected
                  deriving (Eq,Ord,Show)
                
-- | Calling Convenctions <http://llvm.org/releases/3.0/docs/LangRef.html#callingconv>    
data CallConv = Ccc | FastCc | ColdCc | Cc String
              | X86StdCall | X86FastCall | X86ThisCall
              | ArmApcs | ArmAapcs | ArmAapcsVfp
              | Msp430Intr 
                deriving (Eq,Ord,Show)



-- | Parameter Attributes <http://llvm.org/releases/3.0/docs/LangRef.html#paramattrs>
data ParamAttr = ZeroExt | SignExt
               | InReg | ByVal | SRet
               | NoAlias | NoCapture
               | Nest 
                 deriving (Eq,Ord,Show)

-- | Function Attributes <http://llvm.org/releases/3.0/docs/LangRef.html#fnattrs>
data FunAttr = AddressSafety
             | AlignStack Integer
             | AlwaysInline
             | NonLazyBind
             | InlineHint
             | Naked
             | NoImplicitFloat
             | NoInline
             | NoRedZone
             | NoReturn
             | NoUnwind
             | OptSize
             | ReadNone
             | ReadOnly
             | ReturnsTwice
             | Ssp
             | SspReq
             | UwTable
             | UnnamedAddr
             deriving (Eq,Ord,Show)




data Section = Section QuoteStr deriving (Eq,Ord,Show)
data QuoteStr = QuoteStr String deriving (Eq,Ord,Show)
                       

data PlainStr = PlainStr String deriving (Eq,Ord,Show)
data Align = Align Integer deriving (Eq,Ord,Show)
data Gc = Gc QuoteStr deriving (Eq,Ord,Show)
data GlobalType = GlobalType String deriving (Eq,Ord,Show)
data AddrSpace = AddrSpace Integer 
               | AddrSpaceUnspecified deriving (Eq,Ord,Show)
                                            


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
             | LocalIdQuoteStr Lstring
             deriving (Eq,Ord,Show)
                      
                      
localIdToLstring :: LocalId -> Lstring                      
localIdToLstring (LocalIdNum s) = Lstring $ show s
localIdToLstring (LocalIdAlphaNum s) = s
localIdToLstring (LocalIdQuoteStr s) = s
                                            
data GlobalId = GlobalIdNum Integer
              | GlobalIdAlphaNum Lstring
              | GlobalIdQuoteStr Lstring
              deriving (Eq,Ord,Show)
                       
globalIdToLstring :: GlobalId -> Lstring                       
globalIdToLstring (GlobalIdNum s) = Lstring $ show s
globalIdToLstring (GlobalIdAlphaNum s) = s
globalIdToLstring (GlobalIdQuoteStr s) = s

data FenceOrder = Acquire | Release | AcqRel | SeqCst 
                | Unordered | Monotonic
                  deriving (Eq,Ord,Show)

data AtomicOp = Axchg | Aadd | Asub | Aand | Anand | Aor | Axor 
              | Amax | Amin | Aumax | Aumin
                deriving (Eq,Ord,Show)
                         
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
                             
data Atomicity = Atomic { volatile::Bool, singlethread::Bool, fence::Maybe FenceOrder }
               | NonAtomic { volatile::Bool }
               deriving (Eq,Ord,Show)
                        

data MemArea = OnStack | InHeap deriving (Eq,Ord,Show)


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
              
data FormalParam = FormalParam
                   { formalParamType :: Type
                   , formalParamPreAttr :: [ParamAttr]
                   , formalParamAlign :: Maybe Align
                   , formalParamId :: Fparam -- Maybe LocalId
                   , formalParamPostAttr :: [ParamAttr]
                   } deriving (Eq,Ord,Show)

data FormalParamList = FormalParamList [FormalParam] 
                       Bool [FunAttr] deriving (Eq,Ord,Show)
                                               
data TypeParamList = TypeParamList [Type] Bool deriving (Eq,Ord,Show)

