{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Llvm.Data.CoreIr
       ( module Llvm.Data.CoreIr
       , module Llvm.Data.Shared
       , module Llvm.Data.IrType
       , module Data.Word
       , Label
       ) where
import Llvm.Data.Shared
import Llvm.Data.IrType
import Compiler.Hoopl (Label)
import Data.Int
import Data.Word (Word8, Word16, Word32, Word64)
import Data.DoubleWord

data NoWrap =
  -- | No Signed Wrap
  Nsw
  -- | No Unsigned Wrap
  | Nuw
  -- | No Signed and Unsigned Wrap
  | Nsuw deriving (Eq,Ord,Show)

data Exact = Exact deriving (Eq,Ord,Show)

data Conversion s v where {
  Trunc :: T (Type s I) v -> Type s I -> Conversion s v;
  Zext :: T (Type s I) v -> Type s I -> Conversion s v;
  Sext :: T (Type s I) v -> Type s I -> Conversion s v;
  FpTrunc ::  T (Type s F) v -> Type s F -> Conversion s v;
  FpExt :: T (Type s F) v -> Type s F -> Conversion s v;
  FpToUi :: T (Type s F) v -> Type s I -> Conversion s v;
  FpToSi :: T (Type s F) v -> Type s I -> Conversion s v;
  UiToFp :: T (Type s I) v -> Type s F -> Conversion s v;
  SiToFp :: T (Type s I) v -> Type s F -> Conversion s v;
  PtrToInt :: T (Type s P) v -> Type s I -> Conversion s v;
  IntToPtr :: T (Type s I) v -> Type s P -> Conversion s v;
  Bitcast :: T Dtype v -> Dtype -> Conversion s v;
  AddrSpaceCast :: T (Type s P) v -> Type s P -> Conversion s v;
  } deriving (Eq, Ord, Show)

data GetElementPtr s v = GetElementPtr (IsOrIsNot InBounds) (T (Type s P) v) [T (Type s I) v]
                       deriving (Eq,Ord,Show)

data Select s r v = Select (Either (T (Type ScalarB I) v) (T (Type s I) v)) (T (Type s r) v) (T (Type s r) v)
                  deriving (Eq,Ord,Show)

data Icmp s v = Icmp IcmpOp (IntOrPtrType s) v v deriving (Eq,Ord,Show)

data Fcmp s v = Fcmp FcmpOp (Type s F) v v deriving (Eq,Ord,Show)

{- vector operations -}
data ShuffleVector r v = ShuffleVector (T (Type VectorB r) v) (T (Type VectorB r) v) (T (Type VectorB I) v)
                       deriving (Eq,Ord,Show)

data ExtractElement r v = ExtractElement (T (Type VectorB r) v) (T (Type ScalarB I) v)
                        deriving (Eq,Ord,Show)

data InsertElement r v = InsertElement (T (Type VectorB r) v) (T (Type ScalarB r) v) (T (Type ScalarB I) v)
                       deriving (Eq,Ord,Show)

{- aggregate operations -}
data ExtractValue v = ExtractValue (T (Type RecordB D) v) [Word32] deriving (Eq,Ord,Show)

data InsertValue v = InsertValue (T (Type RecordB D) v) (T Dtype v) [Word32] deriving (Eq,Ord,Show)

data Const where {
  C_u8 :: Word8 -> Const;
  C_u16 :: Word16 -> Const;
  C_u32 :: Word32 -> Const;
  C_u64 :: Word64 -> Const;
  C_u96 :: Word96 -> Const;
  C_u128 :: Word128 -> Const;
  C_s8 :: Int8 -> Const;
  C_s16 :: Int16 -> Const;
  C_s32 :: Int32 -> Const;
  C_s64 :: Int64 -> Const;
  C_s96 :: Int96 -> Const;
  C_s128 :: Int128 -> Const;
  C_int :: String -> Const;
  C_uhex_int :: String -> Const;
  C_shex_int :: String -> Const;
  C_float :: String -> Const;
  C_null :: Const;
  C_undef :: Const;
  C_true :: Const;
  C_false :: Const;
  C_zeroinitializer :: Const;
  C_globalAddr :: GlobalId -> Const;
  C_str :: String -> Const;
  C_struct :: Packing -> [TypedConstOrNull] -> Const;
  C_vector :: [TypedConstOrNull] -> Const;
  C_vectorN :: Word32 -> TypedConstOrNull -> Const;
  C_array :: [TypedConstOrNull] -> Const;
  C_arrayN :: Word32 -> TypedConstOrNull -> Const;
  C_localId :: LocalId -> Const;
  C_labelId :: Label -> Const;
  C_block :: GlobalId -> Label -> Const;

  C_add :: Maybe NoWrap -> Type ScalarB I -> Const -> Const -> Const;
  C_sub :: Maybe NoWrap -> Type ScalarB I -> Const -> Const -> Const;
  C_mul :: Maybe NoWrap -> Type ScalarB I -> Const -> Const -> Const;
  C_udiv :: Maybe Exact -> Type ScalarB I -> Const -> Const -> Const;
  C_sdiv :: Maybe Exact -> Type ScalarB I -> Const -> Const -> Const;
  C_urem :: Type ScalarB I -> Const -> Const -> Const;
  C_srem :: Type ScalarB I -> Const -> Const -> Const;
  C_shl :: Maybe NoWrap -> Type ScalarB I -> Const -> Const -> Const;
  C_lshr :: Maybe Exact -> Type ScalarB I -> Const -> Const -> Const;
  C_ashr :: Maybe Exact -> Type ScalarB I -> Const -> Const -> Const;
  C_and :: Type ScalarB I -> Const -> Const -> Const;
  C_or :: Type ScalarB I -> Const -> Const -> Const;
  C_xor :: Type ScalarB I -> Const -> Const -> Const;

  C_add_V :: Maybe NoWrap -> Type VectorB I -> Const -> Const -> Const;
  C_sub_V :: Maybe NoWrap -> Type VectorB I -> Const -> Const -> Const;
  C_mul_V :: Maybe NoWrap -> Type VectorB I -> Const -> Const -> Const;
  C_udiv_V :: Maybe Exact -> Type VectorB I -> Const -> Const -> Const;
  C_sdiv_V :: Maybe Exact -> Type VectorB I -> Const -> Const -> Const;
  C_urem_V :: Type VectorB I -> Const -> Const -> Const;
  C_srem_V :: Type VectorB I -> Const -> Const -> Const;
  C_shl_V :: Maybe NoWrap -> Type VectorB I -> Const -> Const -> Const;
  C_lshr_V :: Maybe Exact -> Type VectorB I -> Const -> Const -> Const;
  C_ashr_V :: Maybe Exact -> Type VectorB I -> Const -> Const -> Const;
  C_and_V :: Type VectorB I -> Const -> Const -> Const;
  C_or_V :: Type VectorB I -> Const -> Const -> Const;
  C_xor_V :: Type VectorB I -> Const -> Const -> Const;

  C_fadd :: FastMathFlags -> Type ScalarB F -> Const -> Const -> Const;
  C_fsub :: FastMathFlags -> Type ScalarB F -> Const -> Const -> Const;
  C_fmul :: FastMathFlags -> Type ScalarB F -> Const -> Const -> Const;
  C_fdiv :: FastMathFlags -> Type ScalarB F -> Const -> Const -> Const;
  C_frem :: FastMathFlags -> Type ScalarB F -> Const -> Const -> Const;

  C_fadd_V :: FastMathFlags -> Type VectorB F -> Const -> Const -> Const;
  C_fsub_V :: FastMathFlags -> Type VectorB F -> Const -> Const -> Const;
  C_fmul_V :: FastMathFlags -> Type VectorB F -> Const -> Const -> Const;
  C_fdiv_V :: FastMathFlags -> Type VectorB F -> Const -> Const -> Const;
  C_frem_V :: FastMathFlags -> Type VectorB F -> Const -> Const -> Const;

  C_trunc :: T (Type ScalarB I) Const -> Type ScalarB I -> Const;
  C_zext :: T (Type ScalarB I) Const -> Type ScalarB I -> Const;
  C_sext :: T (Type ScalarB I) Const -> Type ScalarB I -> Const;
  C_fptrunc ::  T (Type ScalarB F) Const -> Type ScalarB F -> Const;
  C_fpext :: T (Type ScalarB F) Const -> Type ScalarB F -> Const;
  C_fptoui :: T (Type ScalarB F) Const -> Type ScalarB I -> Const;
  C_fptosi :: T (Type ScalarB F) Const -> Type ScalarB I -> Const;
  C_uitofp :: T (Type ScalarB I) Const -> Type ScalarB F -> Const;
  C_sitofp :: T (Type ScalarB I) Const -> Type ScalarB F -> Const;
  C_ptrtoint :: T (Type ScalarB P) Const -> Type ScalarB I -> Const;
  C_inttoptr :: T (Type ScalarB I) Const -> Type ScalarB P -> Const;
  C_bitcast :: T Dtype Const -> Dtype -> Const;
  C_addrspacecast :: T (Type ScalarB P) Const -> Type ScalarB P -> Const;

  C_trunc_V :: T (Type VectorB I) Const -> Type VectorB I -> Const;
  C_zext_V :: T (Type VectorB I) Const -> Type VectorB I -> Const;
  C_sext_V :: T (Type VectorB I) Const -> Type VectorB I -> Const;
  C_fptrunc_V ::  T (Type VectorB F) Const -> Type VectorB F -> Const;
  C_fpext_V :: T (Type VectorB F) Const -> Type VectorB F -> Const;
  C_fptoui_V :: T (Type VectorB F) Const -> Type VectorB I -> Const;
  C_fptosi_V :: T (Type VectorB F) Const -> Type VectorB I -> Const;
  C_uitofp_V :: T (Type VectorB I) Const -> Type VectorB F -> Const;
  C_sitofp_V :: T (Type VectorB I) Const -> Type VectorB F -> Const;
  C_ptrtoint_V :: T (Type VectorB P) Const -> Type VectorB I -> Const;
  C_inttoptr_V :: T (Type VectorB I) Const -> Type VectorB P -> Const;
  C_addrspacecast_V :: T (Type VectorB P) Const -> Type VectorB P -> Const;

  C_getelementptr :: IsOrIsNot InBounds -> T (Type ScalarB P) Const -> [T (Type ScalarB I) Const] -> Const;
  C_getelementptr_V :: IsOrIsNot InBounds -> T (Type VectorB P) Const -> [T (Type VectorB I) Const] -> Const;

  C_select_I :: Select ScalarB I Const -> Const;
  C_select_F :: Select ScalarB F Const -> Const;
  C_select_P :: Select ScalarB P Const -> Const;

  C_select_First :: T (Type ScalarB I) Const -> T (Type FirstClassB D) Const -> T (Type FirstClassB D) Const -> Const;

  C_select_VI :: Select VectorB I Const -> Const;
  C_select_VF :: Select VectorB F Const -> Const;
  C_select_VP :: Select VectorB P Const -> Const;

  C_icmp :: Icmp ScalarB Const -> Const;
  C_icmp_V :: Icmp VectorB Const -> Const;

  C_fcmp :: Fcmp ScalarB Const -> Const;
  C_fcmp_V :: Fcmp VectorB Const -> Const;

  C_shufflevector_I :: ShuffleVector I Const -> Const;
  C_shufflevector_F :: ShuffleVector F Const -> Const;
  C_shufflevector_P :: ShuffleVector P Const -> Const;

  C_extractelement_I :: ExtractElement I Const -> Const;
  C_extractelement_F :: ExtractElement F Const -> Const;
  C_extractelement_P :: ExtractElement P Const -> Const;

  C_insertelement_I :: InsertElement I Const -> Const;
  C_insertelement_F :: InsertElement F Const -> Const;
  C_insertelement_P :: InsertElement P Const -> Const;

  C_extractvalue :: ExtractValue Const -> Const;
  C_insertvalue :: InsertValue Const -> Const;
  } deriving (Eq, Ord, Show)

data MdVar = MdVar String deriving (Eq,Ord,Show)
data MdNode = MdNode String deriving (Eq,Ord,Show)
data MetaConst = McStruct [MetaKindedConst]
               | McString DqString
               | McMn MdNode
               | McMv MdVar
               | McRef LocalId
               | McSimple Const
               deriving (Eq,Ord,Show)

data MetaKindedConst = MetaKindedConst MetaKind MetaConst
                     | UnmetaKindedNull
                     deriving (Eq, Ord, Show)

data FunName = FunNameGlobal GlobalOrLocalId
             | FunNameString String
             deriving (Eq,Ord,Show)

data CallSiteType = CallSiteRet Rtype
                  | CallSiteFun (Type CodeFunB X) AddrSpace
                  deriving (Eq, Ord, Show)

data CallSite = CsFun (Maybe CallConv) [ParamAttr] CallSiteType FunName [ActualParam] [FunAttr]
              | CsAsm CallSiteType (Maybe SideEffect) (Maybe AlignStack) AsmDialect DqString DqString [ActualParam] [FunAttr]
              | CsConversion [ParamAttr] CallSiteType (Conversion ScalarB Const) [ActualParam] [FunAttr]
              | CsConversionV [ParamAttr] CallSiteType (Conversion VectorB Const) [ActualParam] [FunAttr]
              deriving (Eq,Ord,Show)

data Clause = Catch (T Dtype Value)
            | Filter TypedConstOrNull
            | CcoS (Conversion ScalarB Value)
            | CcoV (Conversion VectorB Value)
            deriving (Eq,Ord,Show)

data PersFn = PersFnId GlobalOrLocalId
            | PersFnCastS (Conversion ScalarB GlobalOrLocalId)
            | PersFnCastV (Conversion VectorB GlobalOrLocalId)
            | PersFnUndef
            | PersFnNull
            | PersFnConst Const
            deriving (Eq, Ord, Show)

data Dbg = Dbg MdVar MetaConst deriving (Eq, Ord, Show)

data PhiInst = PhiInst LocalId Ftype [(Value, Label)] deriving (Eq, Ord, Show)

data PhiInstWithDbg = PhiInstWithDbg PhiInst [Dbg] deriving (Eq, Ord, Show)

i_alloca :: FileLoc -> CInst
i_alloca loc = I_alloca { result = errorLoc loc "please assign a new variable name"
                        , inAllocaAttr = IsNot InAllocaAttr
                        , dtype = errorLoc loc "please specify the allocated date type"
                        , size = Nothing
                        , alignment = Nothing
                        }

i_getelementptr :: FileLoc -> CInst
i_getelementptr loc = I_getelementptr { result = errorLoc loc "please assign a new variable name"
                                      , inBounds = IsNot InBounds
                                      , pointer = errorLoc loc "please assign a pointer"
                                      , indices = []
                                      }


i_store :: FileLoc -> CInst
i_store loc = I_store { volatile = IsNot Volatile
                      , storedvalue = errorLoc loc "please specified the stored value"
                      , pointer = errorLoc loc " please assign a pointer"
                      , alignment = Nothing
                      , nontemporal = Nothing
                      }

data CInst where {
  I_alloca :: { inAllocaAttr :: IsOrIsNot InAllocaAttr
              , dtype :: Dtype
              , size :: Maybe (T (Type ScalarB I) Value)
              , alignment :: Maybe Alignment
              , result :: LocalId
              } -> CInst;

  I_load :: { volatile :: IsOrIsNot Volatile
            , pointer :: T (Type ScalarB P) Value
            , alignment :: Maybe Alignment
            , temporal :: Maybe Nontemporal
            , invariantLoad :: Maybe InvariantLoad
            , nonull :: Maybe Nonnull
            , result :: LocalId
            } -> CInst;

  I_loadatomic :: { atomicity :: Atomicity
                  , volatile :: IsOrIsNot Volatile
                  , pointer :: T (Type ScalarB P) Value
                  , alignment :: Maybe Alignment
                  , result :: LocalId
                  } -> CInst;

  I_store :: { volatile :: IsOrIsNot Volatile
             , storedvalue :: T Dtype Value
             , pointer :: T (Type ScalarB P) Value
             , alignment :: Maybe Alignment
             , nontemporal :: Maybe Nontemporal 
             } -> CInst;

  I_storeatomic :: { atomicity :: Atomicity
                   , volatile :: IsOrIsNot Volatile
                   , storedvalue :: T Dtype Value
                   , pointer :: T (Type ScalarB P) Value
                   , alignment :: Maybe Alignment 
                   } -> CInst;

  I_fence :: { singleThread :: IsOrIsNot SingleThread
             , ordering :: AtomicMemoryOrdering } -> CInst;

  I_cmpxchg_I :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer ::  T (Type ScalarB P) Value
                 , cmpi :: T (Type ScalarB I) Value
                 , newi :: T (Type ScalarB I) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId } -> CInst;

  I_cmpxchg_F :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer :: T (Type ScalarB P) Value
                 , cmpf :: T (Type ScalarB F) Value
                 , newf :: T (Type ScalarB F) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId } -> CInst;

  I_cmpxchg_P :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer :: T (Type ScalarB P) Value
                 , cmpp :: T (Type ScalarB P) Value
                 , newp :: T (Type ScalarB P) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId } -> CInst;

  I_atomicrmw :: { volatile :: IsOrIsNot Volatile
                 , atomicOp :: AtomicOp
                 , pointer :: T (Type ScalarB P) Value
                 , val :: T (Type ScalarB I) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , ordering :: AtomicMemoryOrdering
                 , result :: LocalId } -> CInst;

  I_call_fun :: { tailCall :: TailCall
                , callConv :: Maybe CallConv
                , paramAttrs :: [ParamAttr]
                , callSiteType :: CallSiteType
                , calleeName :: FunName
                , actualParams :: [ActualParam]
                , funAttrs :: [FunAttr]
                , callReturn :: Maybe LocalId } -> CInst;

  I_call_other :: { tailCall :: TailCall
                  , callSite :: CallSite
                  , callReturn :: Maybe LocalId } -> CInst;

  I_extractelement_I :: { vectorI :: T (Type VectorB I) Value
                        , index :: T (Type ScalarB I) Value
                        , result :: LocalId } -> CInst;

  I_extractelement_F :: { vectorF :: T (Type VectorB F) Value
                        , index :: T (Type ScalarB I) Value
                        , result :: LocalId } -> CInst;

  I_extractelement_P :: { vectorP :: T (Type VectorB P) Value
                        , index :: T (Type ScalarB I) Value
                        , result :: LocalId } -> CInst;

  I_insertelement_I :: { vectorI :: T (Type VectorB I) Value
                       , elementI :: T (Type ScalarB I) Value
                       , index :: T (Type ScalarB I) Value
                       , result :: LocalId } -> CInst;

  I_insertelement_F :: { vectorF :: T (Type VectorB F) Value
                       , elementF :: T (Type ScalarB F) Value
                       , index :: T (Type ScalarB I) Value
                       , result :: LocalId
                       } -> CInst;

  I_insertelement_P :: { vectorP :: T (Type VectorB P) Value
                       , elementP :: T (Type ScalarB P) Value
                       , index :: T (Type ScalarB I) Value
                       , result :: LocalId
                       } -> CInst;

  I_shufflevector_I :: { vector1I :: T (Type VectorB I) Value
                       , vector2I :: T (Type VectorB I) Value
                       , vectorIdx :: T (Type VectorB I) Value
                       , result :: LocalId
                       } -> CInst;

  I_shufflevector_F :: { vector1F :: T (Type VectorB F) Value
                       , vector2F :: T (Type VectorB F) Value
                       , vectorIdx :: T (Type VectorB I) Value
                       , result :: LocalId
                       } -> CInst;

  I_shufflevector_P :: { vector1P :: T (Type VectorB P) Value
                       , vector2P :: T (Type VectorB P) Value
                       , vectorIdx :: T (Type VectorB I) Value
                       , result :: LocalId
                       } -> CInst;

  I_extractvalue :: T (Type RecordB D) Value -> [Word32] -> LocalId ->  CInst;
  I_insertvalue :: T (Type RecordB D) Value -> T Dtype Value -> [Word32] -> LocalId ->  CInst;

  I_va_arg :: { dv :: T Dtype Value 
              , typeD :: Dtype 
              , result :: LocalId 
              } -> CInst;
  
  I_va_start :: { pointer :: T (Type ScalarB P) Value } -> CInst;
  I_va_end :: { pointer :: T (Type ScalarB P) Value } -> CInst;

  I_landingpad :: Dtype -> Dtype -> PersFn -> Maybe Cleanup -> [Clause] -> LocalId -> CInst;

  I_getelementptr :: { inBounds :: IsOrIsNot InBounds
                     , pointer :: T (Type ScalarB P) Value
                     , indices :: [T (Type ScalarB I) Value]
                     , result :: LocalId 
                     } -> CInst;

  I_getelementptr_V :: { inBounds :: IsOrIsNot InBounds
                       , vpointer :: T (Type VectorB P) Value
                       , vindices :: [T (Type VectorB I) Value]
                       , result :: LocalId 
                       } -> CInst;

  {- Scalar Integer cmp -}
  I_icmp :: { icmpOp :: IcmpOp
            , icmpType :: IntOrPtrType ScalarB
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> CInst;

  {- Vector Integer cmp -}
  I_icmp_V :: { icmpOp :: IcmpOp
              , icmpTypeV :: IntOrPtrType VectorB
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> CInst;

  {- Scalar Float cmp -}
  I_fcmp :: { fcmpOp :: FcmpOp
            , fcmpTypeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> CInst;

  I_fcmp_V :: { fcmpOp :: FcmpOp 
              , fcmpTypeVF :: Type VectorB F 
              , operand1 :: Value 
              , operand2 :: Value 
              , result :: LocalId 
              } -> CInst;

  {-- int bin exp --}
  I_add :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId 
           } -> CInst;

  I_sub :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> CInst;

  I_mul :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId 
           } -> CInst;

  I_udiv :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId 
            } -> CInst;

  I_sdiv :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId 
            } -> CInst;

  I_urem :: { typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId 
            } -> CInst;

  I_srem :: { typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId 
            } -> CInst;

  I_shl :: { flagW :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId 
           } -> CInst;

  I_lshr :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId 
            } -> CInst;

  I_ashr :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId 
            } -> CInst;

  I_and :: { typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId 
           } -> CInst;

  I_or :: { typeI :: Type ScalarB I
          , operand1 :: Value
          , operand2 :: Value
          , result :: LocalId 
          } -> CInst;

  I_xor :: { typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId 
           } -> CInst;

  {-- int bin vector exp --}
  I_add_V :: (Maybe NoWrap) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_sub_V :: (Maybe NoWrap) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_mul_V :: (Maybe NoWrap) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_udiv_V :: (Maybe Exact) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_sdiv_V :: (Maybe Exact) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_urem_V :: Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_srem_V :: Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_shl_V :: (Maybe NoWrap) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_lshr_V :: (Maybe Exact) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_ashr_V :: (Maybe Exact) -> Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_and_V :: Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_or_V :: Type VectorB I -> Value -> Value -> LocalId -> CInst;
  I_xor_V :: Type VectorB I -> Value -> Value -> LocalId -> CInst;

  {- float bin exp -}
  I_fadd :: FastMathFlags -> Type ScalarB F -> Value -> Value -> LocalId -> CInst;
  I_fsub :: FastMathFlags -> Type ScalarB F -> Value -> Value -> LocalId -> CInst;
  I_fmul :: FastMathFlags -> Type ScalarB F -> Value -> Value -> LocalId -> CInst;
  I_fdiv :: FastMathFlags -> Type ScalarB F -> Value -> Value -> LocalId -> CInst;
  I_frem :: FastMathFlags -> Type ScalarB F -> Value -> Value -> LocalId -> CInst;

  {- float bin exp -}
  I_fadd_V :: FastMathFlags -> Type VectorB F -> Value -> Value -> LocalId -> CInst;
  I_fsub_V :: FastMathFlags -> Type VectorB F -> Value -> Value -> LocalId -> CInst;
  I_fmul_V :: FastMathFlags -> Type VectorB F -> Value -> Value -> LocalId -> CInst;
  I_fdiv_V :: FastMathFlags -> Type VectorB F -> Value -> Value -> LocalId -> CInst;
  I_frem_V :: FastMathFlags -> Type VectorB F -> Value -> Value -> LocalId -> CInst;

  {-- Scalar conversion --}
  I_trunc :: { srcI :: T (Type ScalarB I) Value
             , toI :: Type ScalarB I
             , result :: LocalId } -> CInst;

  I_zext :: { srcI :: T (Type ScalarB I) Value
            , toI :: Type ScalarB I
            , result :: LocalId } -> CInst;

  I_sext :: { srcI :: T (Type ScalarB I) Value
            , toI :: Type ScalarB I
            , result :: LocalId } -> CInst;

  I_fptrunc ::  { srcF :: T (Type ScalarB F) Value
                , toF :: Type ScalarB F
                , result :: LocalId } -> CInst;

  I_fpext ::  { srcF :: T (Type ScalarB F) Value
              , toF :: Type ScalarB F
              , result :: LocalId } -> CInst;

  I_fptoui :: { srcF :: T (Type ScalarB F) Value
              , toI :: Type ScalarB I
              , result :: LocalId } -> CInst;

  I_fptosi :: { srcF ::T (Type ScalarB F) Value
              , toI :: Type ScalarB I
              , result :: LocalId } -> CInst;

  I_uitofp :: { srcI :: T (Type ScalarB I) Value
              , toF :: Type ScalarB F
              , result :: LocalId } -> CInst;

  I_sitofp :: { srcI :: T (Type ScalarB I) Value
              , toF :: Type ScalarB F
              , result :: LocalId } -> CInst;

  I_ptrtoint :: { srcP :: T (Type ScalarB P) Value
                , toI :: Type ScalarB I
                , result :: LocalId } -> CInst;

  I_inttoptr :: { srcI :: T (Type ScalarB I) Value
                , toP :: Type ScalarB P
                , result :: LocalId } -> CInst;

  I_addrspacecast :: T (Type ScalarB P) Value -> Type ScalarB P -> LocalId -> CInst;


  I_bitcast :: { srcP :: T (Type ScalarB P) Value
               , toP :: Type ScalarB P
               , result :: LocalId } -> CInst;

  I_bitcast_D :: { srcD :: T Dtype Value
                 , toD :: Dtype
                 , result :: LocalId } -> CInst;

  {-- Vector conversion --}
  I_trunc_V :: T (Type VectorB I) Value -> Type VectorB I -> LocalId -> CInst;
  I_zext_V :: T (Type VectorB I) Value -> Type VectorB I -> LocalId -> CInst;
  I_sext_V :: T (Type VectorB I) Value -> Type VectorB I -> LocalId -> CInst;
  I_fptrunc_V :: T (Type VectorB F) Value -> Type VectorB F -> LocalId -> CInst;
  I_fpext_V :: T (Type VectorB F) Value -> Type VectorB F -> LocalId -> CInst;
  I_fptoui_V :: T (Type VectorB F) Value -> Type VectorB I -> LocalId -> CInst;
  I_fptosi_V :: T (Type VectorB F) Value -> Type VectorB I -> LocalId -> CInst;
  I_uitofp_V :: T (Type VectorB I) Value -> Type VectorB F -> LocalId -> CInst;
  I_sitofp_V :: T (Type VectorB I) Value -> Type VectorB F -> LocalId -> CInst;

  I_ptrtoint_V :: { srcVP :: T (Type VectorB P) Value
                  , toVI :: Type VectorB I
                  , result :: LocalId } -> CInst;

  I_inttoptr_V :: { srcVI :: T (Type VectorB I) Value
                  , toVP :: Type VectorB P
                  , result :: LocalId } -> CInst;

  I_addrspacecast_V :: { srcVP :: T (Type VectorB P) Value
                       , toVP :: Type VectorB P
                       , result :: LocalId } -> CInst;

  I_select_I :: T (Type ScalarB I) Value -> T (Type ScalarB I) Value -> T (Type ScalarB I) Value -> LocalId -> CInst;
  I_select_F :: T (Type ScalarB I) Value -> T (Type ScalarB F) Value -> T (Type ScalarB F) Value -> LocalId -> CInst;
  I_select_P :: T (Type ScalarB I) Value -> T (Type ScalarB P) Value -> T (Type ScalarB P) Value -> LocalId -> CInst;

  I_select_VI :: Either (T (Type ScalarB I) Value) (T (Type VectorB I) Value)
                 -> T (Type VectorB I) Value -> T (Type VectorB I) Value -> LocalId -> CInst;
  I_select_VF :: Either (T (Type ScalarB I) Value) (T (Type VectorB I) Value)
                 -> T (Type VectorB F) Value -> T (Type VectorB F) Value -> LocalId -> CInst;

  I_select_VP :: { condV :: Either (T (Type ScalarB I) Value) (T (Type VectorB I) Value)
                 , trueVP :: T (Type VectorB P) Value
                 , falseVP :: T (Type VectorB P) Value
                 , result :: LocalId } -> CInst;

  I_select_First :: { cond :: T (Type ScalarB I) Value
                    , trueFirst :: T (Type FirstClassB D) Value
                    , falseFirst :: T (Type FirstClassB D) Value
                    , result :: LocalId } -> CInst;

  {-- llvm intrinsic function calls --}
  I_llvm_memcpy:: { memlen :: MemLen
                  , dest :: T (Type ScalarB P) Value
                  , src :: T (Type ScalarB P) Value
                  , len :: T (Type ScalarB I) Value
                  , align :: T (Type ScalarB I) Value
                  , isvolatile :: T (Type ScalarB I) Value
                  } -> CInst;
  
  I_llvm_dbg_declare :: [ActualParam] -> CInst;
  I_llvm_dbg_value :: [ActualParam] -> CInst;
  } deriving (Eq, Ord, Show)

data MemLen = MemLenI32
            | MemLenI64 deriving (Eq, Ord, Show)

{- -}
data CInstWithDbg = CInstWithDbg CInst [Dbg] deriving (Eq, Ord, Show)

data TerminatorInst = Unreachable
                    | RetVoid
                    | Return [T Dtype Value]
                    | Br Label
                    | Cbr Value Label Label
                    | IndirectBr (T (Type ScalarB P) Value) [Label]
                    | Switch (T (Type ScalarB I) Value) Label [((T (Type ScalarB I) Value), Label)]
                    | Invoke CallSite Label Label (Maybe LocalId)
                    | InvokeCmd CallSite Label Label
                    | Resume (T Dtype Value)
                    | Unwind
                    deriving (Eq, Ord, Show)

data TerminatorInstWithDbg = TerminatorInstWithDbg TerminatorInst [Dbg] deriving (Eq, Ord, Show)

data ActualParam = ActualParamData Dtype [ParamAttr] (Maybe Alignment) Value [ParamAttr]
                 | ActualParamLabel (Type CodeLabelB X) [ParamAttr] (Maybe Alignment) Value [ParamAttr]
                 | ActualParamMeta MetaKindedConst
                 deriving (Eq,Ord,Show)

data Value = Val_ssa LocalId
           | Val_const Const
           deriving (Eq,Ord,Show)

data T t v = T t v deriving (Eq, Ord, Show)

data Aliasee = AtV (T Dtype Value)
             | Ac (Conversion ScalarB Const)
             | AcV (Conversion VectorB Const)
             | Agep (GetElementPtr ScalarB Const)
             | AgepV (GetElementPtr VectorB Const)
             deriving (Eq, Ord, Show)

data FunctionPrototype = FunctionPrototype { fp_linkage :: Maybe Linkage
                                           , fp_visibility :: Maybe Visibility
                                           , fp_dllstorage :: Maybe DllStorageClass
                                           , fp_call_conv :: Maybe CallConv
                                           , fp_param_attrs :: [ParamAttr]
                                           , fp_ret_type :: Rtype
                                           , fp_fun_name :: GlobalId
                                           , fp_param_list :: FormalParamList
                                           , fp_addr_naming :: Maybe AddrNaming
                                           , fp_fun_attrs :: [FunAttr]
                                           , fp_section :: Maybe Section
                                           , fp_comdat :: Maybe Comdat
                                           , fp_alignment :: Maybe Alignment
                                           , fp_gc :: Maybe Gc
                                           , fp_prefix :: Maybe Prefix
                                           , fp_prologue :: Maybe Prologue
                                           } deriving (Eq,Ord,Show)

data Prefix = Prefix TypedConstOrNull deriving (Eq, Ord, Show)
data Prologue = Prologue TypedConstOrNull deriving (Eq, Ord, Show)

data TypedConstOrNull = TypedConst (T Dtype Const)
                      | UntypedNull deriving (Eq, Ord, Show)


instance Ucast Const Const where
  ucast = id

instance Ucast Const Value where
  ucast = Val_const

instance Ucast Value Value where
  ucast = id

instance Dcast Value Value where
  dcast _ = id

instance Dcast Value Const where
  dcast lc x = case x of
    Val_const v -> v
    _ -> dcastError lc "Const" x

instance (Ucast t s, Ucast u v) => Ucast (T t u) (T s v) where
  ucast (T t u) = T (ucast t) (ucast u)

instance (Dcast s t, Dcast v u) => Dcast (T s v) (T t u) where
  dcast lc (T s v) = T (dcast lc s) (dcast lc v)
