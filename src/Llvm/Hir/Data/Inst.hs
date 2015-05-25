{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Llvm.Hir.Data.Inst
       ( module Llvm.Hir.Data.Inst
       , module Llvm.Asm.SharedEntity
       , module Llvm.Hir.Data.Type
       , module Data.Word
       , Label
       ) where
import Llvm.Asm.SharedEntity
import Llvm.Hir.Data.Type
import Compiler.Hoopl (Label)
import Data.Int
import Data.Word (Word8, Word16, Word32, Word64)
import Data.DoubleWord
import Llvm.ErrorLoc

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

data FunPtr = FunId GlobalId
            | FunIdBitcast (T Dtype Const) Dtype
            | FunIdInttoptr (T Dtype Const) Dtype
            | FunSsa LocalId
            | Fun_null
            | Fun_undef
            deriving (Eq, Ord, Show)

data AsmCode = AsmCode { asm_dialect :: AsmDialect
                       , asm_dqstring1 :: DqString
                       , asm_dqstring2 :: DqString
                       } deriving (Eq, Ord, Show)

data CallSiteType = CallSiteTypeRet Rtype
                  | CallSiteTypeFun (Type CodeFunB X) AddrSpace
                  deriving (Eq, Ord, Show)

data CallFunInterface = CallFunInterface { cfi_tail :: TailCall
                                         , cfi_conv :: CallConv
                                         , cfi_retAttrs :: [RetAttr]
                                         , cfi_type :: CallSiteType
                                         , cfi_actualParams :: [ActualParam]
                                         , cfi_funAttrs :: [FunAttr]
                                         } 
                      | CallFunInterface2 { cfi_tail :: TailCall
                                          , cfi_conv :: CallConv
                                          , cfi_retAttrs :: [RetAttr]
                                          , cfi_type :: CallSiteType
                                          , cfi_firstParamAsRet :: FirstParamAsRet
                                          , cfi_actualParams :: [ActualParam]
                                          , cfi_funAttrs :: [FunAttr]
                                          } deriving (Eq, Ord, Show)

data InvokeFunInterface = InvokeFunInterface { ifi_conv :: CallConv
                                             , ifi_retAttrs :: [RetAttr]
                                             , ifi_type :: CallSiteType
                                             , ifi_actualParams :: [ActualParam]
                                             , ifi_funAttrs :: [FunAttr]
                                             }
                        | InvokeFunInterface2 { ifi_conv :: CallConv
                                              , ifi_retAttrs :: [RetAttr]
                                              , ifi_type :: CallSiteType
                                              , ifi_firstParamAsRet :: FirstParamAsRet
                                              , ifi_actualParams :: [ActualParam]
                                              , ifi_funAttrs :: [FunAttr]
                                              } deriving (Eq, Ord, Show)

data CallAsmInterface = CallAsmInterface { cai_type :: CallSiteType
                                         , cai_sideeffect :: Maybe SideEffect
                                         , cai_alignstack :: Maybe AlignStack
                                         , cai_actualParams :: [ActualParam]
                                         , cai_funAttrs :: [FunAttr]
                                         } deriving (Eq, Ord, Show)


data Clause = Catch (T Dtype Value)
            | Filter TypedConstOrNull
            | CcoS (Conversion ScalarB Value)
            | CcoV (Conversion VectorB Value)
            deriving (Eq,Ord,Show)

data Dbg = Dbg MdVar MetaConst deriving (Eq, Ord, Show)

-- | Phi Instrunction merge ssa values from multiple incomming dataflows.
-- | It is a standalone type due to its unique position, always immediately following up Lnode, in a basic block
data Pinst = Pinst { ftype :: Ftype
                   , flowins :: [(Value, Label)]
                   , flowout :: LocalId
                   } deriving (Eq, Ord, Show)

i_alloca :: FileLoc -> Cinst
i_alloca loc = I_alloca { result = errorLoc loc "please assign a new variable name"
                        , inAllocaAttr = IsNot InAllocaAttr
                        , dtype = errorLoc loc "please specify the allocated date type"
                        , size = Nothing
                        , alignment = Nothing
                        }

i_getelementptr :: FileLoc -> Cinst
i_getelementptr loc = I_getelementptr { result = errorLoc loc "please assign a new variable name"
                                      , inBounds = IsNot InBounds
                                      , pointer = errorLoc loc "please assign a pointer"
                                      , indices = []
                                      }


i_store :: FileLoc -> Cinst
i_store loc = I_store { volatile = IsNot Volatile
                      , storedvalue = errorLoc loc "please specified the stored value"
                      , pointer = errorLoc loc " please assign a pointer"
                      , alignment = Nothing
                      , nontemporal = Nothing
                      }

-- | Computation Instructions compute and cause side effects
data Cinst where {
  I_alloca :: { inAllocaAttr :: IsOrIsNot InAllocaAttr
              , dtype :: Dtype
              , size :: Maybe (T (Type ScalarB I) Value)
              , alignment :: Maybe Alignment
              , result :: LocalId
              } -> Cinst;

  I_load :: { volatile :: IsOrIsNot Volatile
            , pointer :: T (Type ScalarB P) Value
            , alignment :: Maybe Alignment
            , temporal :: Maybe Nontemporal
            , invariantLoad :: Maybe InvariantLoad
            , nonull :: Maybe Nonnull
            , result :: LocalId
            } -> Cinst;

  I_loadatomic :: { atomicity :: Atomicity
                  , volatile :: IsOrIsNot Volatile
                  , pointer :: T (Type ScalarB P) Value
                  , alignment :: Maybe Alignment
                  , result :: LocalId
                  } -> Cinst;

  I_store :: { volatile :: IsOrIsNot Volatile
             , storedvalue :: T Dtype Value
             , pointer :: T (Type ScalarB P) Value
             , alignment :: Maybe Alignment
             , nontemporal :: Maybe Nontemporal
             } -> Cinst;

  I_storeatomic :: { atomicity :: Atomicity
                   , volatile :: IsOrIsNot Volatile
                   , storedvalue :: T Dtype Value
                   , pointer :: T (Type ScalarB P) Value
                   , alignment :: Maybe Alignment
                   } -> Cinst;

  I_fence :: { singleThread :: IsOrIsNot SingleThread
             , ordering :: AtomicMemoryOrdering
             } -> Cinst;

  I_cmpxchg_I :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer ::  T (Type ScalarB P) Value
                 , cmpi :: T (Type ScalarB I) Value
                 , newi :: T (Type ScalarB I) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst;

  I_cmpxchg_F :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer :: T (Type ScalarB P) Value
                 , cmpf :: T (Type ScalarB F) Value
                 , newf :: T (Type ScalarB F) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst;

  I_cmpxchg_P :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer :: T (Type ScalarB P) Value
                 , cmpp :: T (Type ScalarB P) Value
                 , newp :: T (Type ScalarB P) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst;

  I_atomicrmw :: { volatile :: IsOrIsNot Volatile
                 , atomicOp :: AtomicOp
                 , pointer :: T (Type ScalarB P) Value
                 , val :: T (Type ScalarB I) Value
                 , singlethread :: IsOrIsNot SingleThread
                 , ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst;

  I_call_fun :: { call_ptr :: FunPtr
                , call_fun_interface :: CallFunInterface
                , call_return :: Maybe LocalId
                } -> Cinst;

  I_call_asm :: { call_asmcode :: AsmCode
                , call_asm_interface :: CallAsmInterface
                , call_return :: Maybe LocalId
                } -> Cinst;

  I_extractelement_I :: { vectorI :: T (Type VectorB I) Value
                        , index :: T (Type ScalarB I) Value
                        , result :: LocalId
                        } -> Cinst;

  I_extractelement_F :: { vectorF :: T (Type VectorB F) Value
                        , index :: T (Type ScalarB I) Value
                        , result :: LocalId
                        } -> Cinst;

  I_extractelement_P :: { vectorP :: T (Type VectorB P) Value
                        , index :: T (Type ScalarB I) Value
                        , result :: LocalId
                        } -> Cinst;

  I_insertelement_I :: { vectorI :: T (Type VectorB I) Value
                       , elementI :: T (Type ScalarB I) Value
                       , index :: T (Type ScalarB I) Value
                       , result :: LocalId
                       } -> Cinst;

  I_insertelement_F :: { vectorF :: T (Type VectorB F) Value
                       , elementF :: T (Type ScalarB F) Value
                       , index :: T (Type ScalarB I) Value
                       , result :: LocalId
                       } -> Cinst;

  I_insertelement_P :: { vectorP :: T (Type VectorB P) Value
                       , elementP :: T (Type ScalarB P) Value
                       , index :: T (Type ScalarB I) Value
                       , result :: LocalId
                       } -> Cinst;

  I_shufflevector_I :: { vector1I :: T (Type VectorB I) Value
                       , vector2I :: T (Type VectorB I) Value
                       , vectorIdx :: T (Type VectorB I) Value
                       , result :: LocalId
                       } -> Cinst;

  I_shufflevector_F :: { vector1F :: T (Type VectorB F) Value
                       , vector2F :: T (Type VectorB F) Value
                       , vectorIdx :: T (Type VectorB I) Value
                       , result :: LocalId
                       } -> Cinst;

  I_shufflevector_P :: { vector1P :: T (Type VectorB P) Value
                       , vector2P :: T (Type VectorB P) Value
                       , vectorIdx :: T (Type VectorB I) Value
                       , result :: LocalId
                       } -> Cinst;

  I_extractvalue :: { record :: T (Type RecordB D) Value
                    , windices :: [Word32]
                    , result :: LocalId
                    } ->  Cinst;

  I_insertvalue :: { record :: T (Type RecordB D) Value
                   , element :: T Dtype Value
                   , windices :: [Word32]
                   , result :: LocalId
                   } ->  Cinst;

  I_landingpad :: { resultType :: Dtype
                  , persFnType :: Dtype
                  , persFn :: FunPtr -- PersFn
                  , cleanup :: Maybe Cleanup
                  , clauses :: [Clause]
                  , result :: LocalId
                  } -> Cinst;

  I_getelementptr :: { inBounds :: IsOrIsNot InBounds
                     , pointer :: T (Type ScalarB P) Value
                     , indices :: [T (Type ScalarB I) Value]
                     , result :: LocalId
                     } -> Cinst;

  I_getelementptr_V :: { inBounds :: IsOrIsNot InBounds
                       , vpointer :: T (Type VectorB P) Value
                       , vindices :: [T (Type VectorB I) Value]
                       , result :: LocalId
                       } -> Cinst;

  {- Scalar Integer cmp -}
  I_icmp :: { icmpOp :: IcmpOp
            , icmpType :: IntOrPtrType ScalarB
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  {- Vector Integer cmp -}
  I_icmp_V :: { icmpOp :: IcmpOp
              , icmpTypeV :: IntOrPtrType VectorB
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  {- Scalar Float cmp -}
  I_fcmp :: { fcmpOp :: FcmpOp
            , fcmpTypeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_fcmp_V :: { fcmpOp :: FcmpOp
              , fcmpTypeVF :: Type VectorB F
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  {-- int bin exp --}
  I_add :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> Cinst;

  I_sub :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> Cinst;

  I_mul :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> Cinst;

  I_udiv :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_sdiv :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_urem :: { typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_srem :: { typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_shl :: { flagW :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> Cinst;

  I_lshr :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_ashr :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_and :: { typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> Cinst;

  I_or :: { typeI :: Type ScalarB I
          , operand1 :: Value
          , operand2 :: Value
          , result :: LocalId
          } -> Cinst;

  I_xor :: { typeI :: Type ScalarB I
           , operand1 :: Value
           , operand2 :: Value
           , result :: LocalId
           } -> Cinst;

  {-- int bin vector exp --}
  I_add_V :: { flagI :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: Value
             , operand2 :: Value
             , result :: LocalId
             } -> Cinst;

  I_sub_V :: { flagI :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: Value
             , operand2 :: Value
             , result :: LocalId
             } -> Cinst;

  I_mul_V :: { flagI :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: Value
             , operand2 :: Value
             , result :: LocalId
             } -> Cinst;

  I_udiv_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_sdiv_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_urem_V :: { typeVI :: Type VectorB I
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_srem_V :: { typeVI :: Type VectorB I
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_shl_V :: { flagW :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: Value
             , operand2 :: Value
             , result :: LocalId
             } -> Cinst;

  I_lshr_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_ashr_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_and_V :: { typeVI :: Type VectorB I
             , operand1 :: Value
             , operand2 :: Value
             , result :: LocalId
             } -> Cinst;

  I_or_V :: { typeVI :: Type VectorB I
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_xor_V :: { typeVI :: Type VectorB I
             , operand1 :: Value
             , operand2 :: Value
             , result :: LocalId
             } -> Cinst;

  {- float bin exp -}
  I_fadd :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_fsub :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_fmul :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_fdiv :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  I_frem :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: Value
            , operand2 :: Value
            , result :: LocalId
            } -> Cinst;

  {- float bin exp -}
  I_fadd_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_fsub_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_fmul_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_fdiv_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  I_frem_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: Value
              , operand2 :: Value
              , result :: LocalId
              } -> Cinst;

  {-- Scalar conversion --}
  I_trunc :: { srcI :: T (Type ScalarB I) Value
             , toI :: Type ScalarB I
             , result :: LocalId
             } -> Cinst;

  I_zext :: { srcI :: T (Type ScalarB I) Value
            , toI :: Type ScalarB I
            , result :: LocalId
            } -> Cinst;

  I_sext :: { srcI :: T (Type ScalarB I) Value
            , toI :: Type ScalarB I
            , result :: LocalId
            } -> Cinst;

  I_fptrunc ::  { srcF :: T (Type ScalarB F) Value
                , toF :: Type ScalarB F
                , result :: LocalId
                } -> Cinst;

  I_fpext ::  { srcF :: T (Type ScalarB F) Value
              , toF :: Type ScalarB F
              , result :: LocalId
              } -> Cinst;

  I_fptoui :: { srcF :: T (Type ScalarB F) Value
              , toI :: Type ScalarB I
              , result :: LocalId
              } -> Cinst;

  I_fptosi :: { srcF ::T (Type ScalarB F) Value
              , toI :: Type ScalarB I
              , result :: LocalId
              } -> Cinst;

  I_uitofp :: { srcI :: T (Type ScalarB I) Value
              , toF :: Type ScalarB F
              , result :: LocalId
              } -> Cinst;

  I_sitofp :: { srcI :: T (Type ScalarB I) Value
              , toF :: Type ScalarB F
              , result :: LocalId
              } -> Cinst;

  I_ptrtoint :: { srcP :: T (Type ScalarB P) Value
                , toI :: Type ScalarB I
                , result :: LocalId
                } -> Cinst;

  I_inttoptr :: { srcI :: T (Type ScalarB I) Value
                , toP :: Type ScalarB P
                , result :: LocalId
                } -> Cinst;

  I_addrspacecast :: { srcP :: T (Type ScalarB P) Value
                     , toP :: Type ScalarB P
                     , result :: LocalId
                     } -> Cinst;

  I_bitcast :: { srcP :: T (Type ScalarB P) Value
               , toP :: Type ScalarB P
               , result :: LocalId
               } -> Cinst;

  I_bitcast_D :: { srcD :: T Dtype Value
                 , toD :: Dtype
                 , result :: LocalId
                 } -> Cinst;

  {-- Vector conversion --}
  I_trunc_V :: { srcVI :: T (Type VectorB I) Value
               , toVI :: Type VectorB I
               , result :: LocalId
               } -> Cinst;

  I_zext_V :: { srcVI :: T (Type VectorB I) Value
              , toVI :: Type VectorB I
              , result :: LocalId
              } -> Cinst;

  I_sext_V :: { srcVI :: T (Type VectorB I) Value
              , toVI :: Type VectorB I
              , result :: LocalId
              } -> Cinst;

  I_fptrunc_V :: { srcVF :: T (Type VectorB F) Value
                 , toVF :: Type VectorB F
                 , result :: LocalId
                 } -> Cinst;

  I_fpext_V :: { srcVF :: T (Type VectorB F) Value
               , toVF :: Type VectorB F
               , result :: LocalId
               } -> Cinst;

  I_fptoui_V :: { srcVF :: T (Type VectorB F) Value
                , toVI :: Type VectorB I
                , result :: LocalId
                } -> Cinst;

  I_fptosi_V :: { srcVF :: T (Type VectorB F) Value
                , toVI :: Type VectorB I
                , result :: LocalId
                } -> Cinst;

  I_uitofp_V :: { srcVI :: T (Type VectorB I) Value
                , toVF :: Type VectorB F
                , result :: LocalId
                } -> Cinst;

  I_sitofp_V :: { srcVI :: T (Type VectorB I) Value
                , toVF :: Type VectorB F
                , result :: LocalId
                } -> Cinst;

  I_ptrtoint_V :: { srcVP :: T (Type VectorB P) Value
                  , toVI :: Type VectorB I
                  , result :: LocalId } -> Cinst;

  I_inttoptr_V :: { srcVI :: T (Type VectorB I) Value
                  , toVP :: Type VectorB P
                  , result :: LocalId } -> Cinst;

  I_addrspacecast_V :: { srcVP :: T (Type VectorB P) Value
                       , toVP :: Type VectorB P
                       , result :: LocalId
                       } -> Cinst;

  I_select_I :: { cond :: T (Type ScalarB I) Value
                , trueI :: T (Type ScalarB I) Value
                , falseI :: T (Type ScalarB I) Value
                , result :: LocalId
                } -> Cinst;

  I_select_F :: { cond :: T (Type ScalarB I) Value
                , trueF :: T (Type ScalarB F) Value
                , falseF :: T (Type ScalarB F) Value
                , result :: LocalId
                } -> Cinst;

  I_select_P :: { cond :: T (Type ScalarB I) Value
                , trueP :: T (Type ScalarB P) Value
                , falseP :: T (Type ScalarB P) Value
                , result :: LocalId
                } -> Cinst;

  I_select_VI :: { condVI :: Either (T (Type ScalarB I) Value) (T (Type VectorB I) Value)
                 , trueVI :: T (Type VectorB I) Value
                 , falseVI :: T (Type VectorB I) Value
                 , result :: LocalId
                 } -> Cinst;

  I_select_VF :: { condVF :: Either (T (Type ScalarB I) Value) (T (Type VectorB I) Value)
                 , trueVF :: T (Type VectorB F) Value
                 , falseVF :: T (Type VectorB F) Value
                 , result :: LocalId
                 } -> Cinst;

  I_select_VP :: { condV :: Either (T (Type ScalarB I) Value) (T (Type VectorB I) Value)
                 , trueVP :: T (Type VectorB P) Value
                 , falseVP :: T (Type VectorB P) Value
                 , result :: LocalId
                 } -> Cinst;

  I_select_First :: { cond :: T (Type ScalarB I) Value
                    , trueFirst :: T (Type FirstClassB D) Value
                    , falseFirst :: T (Type FirstClassB D) Value
                    , result :: LocalId
                    } -> Cinst;

  {-- llvm intrinsic function calls --}
  I_va_arg :: { dv :: T Dtype Value
              , typeD :: Dtype
              , result :: LocalId
              } -> Cinst;

  I_llvm_va_start :: { arglist :: Value {- i8 * -} } -> Cinst;
  I_llvm_va_end :: { arglist :: Value  {- i8 * -} } -> Cinst;
  I_llvm_va_copy :: { destarglist :: Value {- i8 * -}
                    , srcarglist :: Value {- i8 * -}
                    } -> Cinst;

  I_llvm_gcroot :: { ptrloc :: Value
                   , metadata :: Value {- i8 * -}
                   } -> Cinst;

  I_llvm_gcread :: { reference :: Value
                   , readFrom :: Value
                   } -> Cinst;

  I_llvm_gcwrite :: { p1 :: Value
                    , obj :: Value
                    , p2 :: Value
                    } -> Cinst;

  I_llvm_returnaddress :: { level :: Value {- i32 -} } -> Cinst;
  I_llvm_frameaddress :: { level :: Value {- i32 -} } -> Cinst;
  I_llvm_frameescape :: [Value] -> Cinst;
  I_llvm_framerecover :: Value -> Value -> Value -> Cinst;

  I_llvm_read_register :: { memLen :: MemLen
                          , meta :: MetaKindedConst
                          } -> Cinst;
  I_llvm_write_register :: { memLen :: MemLen
                           , meta :: MetaKindedConst
                           , value :: Value
                           } -> Cinst;
  I_llvm_stacksave :: { result :: LocalId } -> Cinst;
  I_llvm_stackrestore :: { pointer :: T (Type ScalarB P) Value } -> Cinst;
  I_llvm_prefetch :: Value -> Value -> Value -> Value -> Cinst;
  I_llvm_pcmarker :: Value -> Cinst;
  I_llvm_readcyclecounter :: { result :: LocalId } -> Cinst;
  I_llvm_clear_cache :: Value -> Value -> Cinst;
  I_llvm_instprof_increment :: Value -> Value -> Value -> Value -> Cinst;

  I_llvm_memcpy :: { memlen :: MemLen
                   , dest :: T (Type ScalarB P) Value
                   , src :: T (Type ScalarB P) Value
                   , len :: T (Type ScalarB I) Value
                   , align :: T (Type ScalarB I) Value
                   , isvolatile :: T (Type ScalarB I) Value
                   } -> Cinst;

  I_llvm_memmove :: { memlen :: MemLen
                    , dest :: T (Type ScalarB P) Value
                    , src :: T (Type ScalarB P) Value
                    , len :: T (Type ScalarB I) Value
                    , align :: T (Type ScalarB I) Value
                    , isvolatile :: T (Type ScalarB I) Value
                    } -> Cinst;

  I_llvm_memset :: { memlen :: MemLen
                   , dest :: T (Type ScalarB P) Value
                   , setValue :: T (Type ScalarB I) Value
                   , len :: T (Type ScalarB I) Value
                   , align :: T (Type ScalarB I) Value
                   , isvolatile :: T (Type ScalarB I) Value
                   } -> Cinst;

  I_llvm_libm_una :: { muop :: LibmUnaryExp
                     , result :: LocalId
                     } -> Cinst;

  I_llvm_libm_bin :: { mbop :: LibmBinaryExp
                     , result :: LocalId
                     } -> Cinst;

  I_llvm_powi :: { realOperand :: T (Type ScalarB F) Value
                 , intOperand :: T (Type ScalarB I) Value
                 , result :: LocalId
                 } -> Cinst;

  I_llvm_bitset_test :: Value -> Value -> LocalId -> Cinst;
  I_llvm_donothing :: Cinst;
  } deriving (Eq, Ord, Show)


data LibmUnaryExp = Sqrt (Type ScalarB F) Value
                  | Sin (Type ScalarB F)  Value
                  | Cos (Type ScalarB F) Value
                  | Exp (Type ScalarB F) Value
                  | Exp2 (Type ScalarB F) Value
                  | Log (Type ScalarB F) Value
                  | Log10 (Type ScalarB F) Value
                  | Log2 (Type ScalarB F) Value
                  | Fabs (Type ScalarB F) Value
                  | Floor (Type ScalarB F) Value
                  | Ceil (Type ScalarB F) Value
                  | Ftrunc (Type ScalarB F) Value
                  | Rint (Type ScalarB F) Value
                  | NearByInt (Type ScalarB F) Value
                  | Round (Type ScalarB F) Value
                  deriving (Eq, Ord, Show)

data LibmBinaryExp = Pow (Type ScalarB F) Value Value
                   | Minnum (Type ScalarB F) Value Value
                   | Maxnum (Type ScalarB F) Value Value
                   | CopySign (Type ScalarB F) Value Value
                   deriving (Eq, Ord, Show)

data MemLen = MemLenI32
            | MemLenI64 deriving (Eq, Ord, Show)

-- | LLVM metadata instructions
data Minst = Minst CallSiteType GlobalId [MetaParam] (Maybe LocalId) deriving (Eq, Ord, Show)

data MetaParam = MetaParamMeta MetaKindedConst
               | MetaParamData Dtype [ParamAttr] (Maybe Alignment) Value [ParamAttr]
               deriving (Eq, Ord, Show)
{- -}
-- | Terminator instructions cause control flow transferring and
-- | side effects (which is unfortunately difficult to seperate out)
data Tinst = T_unreachable
           | T_ret_void
           | T_return [T Dtype Value]
           | T_br Label
           | T_cbr { condition :: Value
                   , trueL :: Label
                   , falseL :: Label
                   }
           | T_indirectbr (T (Type ScalarB P) Value) [Label]
           | T_switch { defaultcase :: (T (Type ScalarB I) Value, Label)
                      , othercases :: [(T (Type ScalarB I) Value, Label)]
                      }
           | T_invoke { invoke_ptr :: FunPtr
                      , invoke_fun_interface :: InvokeFunInterface
                      , invoke_normal_label :: Label
                      , invoke_exception_label :: Label
                      , invoke_return :: Maybe LocalId
                      }
           | T_invoke_asm { invoke_asmcode :: AsmCode
                          , invoke_asm_interface :: CallAsmInterface
                          , invoke_normal_label :: Label
                          , invoke_exception_label :: Label
                          , invoke_return :: Maybe LocalId
                          }
           | T_resume (T Dtype Value)
           | T_unwind
           deriving (Eq, Ord, Show)

data ActualParam = ActualParamData Dtype [ParamAttr] (Maybe Alignment) Value
                 | ActualParamByVal Dtype [ParamAttr] (Maybe Alignment) Value
                 | ActualParamLabel (Type CodeLabelB X) [ParamAttr] (Maybe Alignment) Label
                 deriving (Eq,Ord,Show)

data FirstParamAsRet = FirstParamAsRet Dtype [ParamAttr] (Maybe Alignment) Value
                     deriving (Eq,Ord,Show)

data Value = Val_ssa LocalId
           | Val_const Const
           deriving (Eq,Ord,Show)

data T t v = T t v deriving (Eq, Ord, Show)

data Aliasee = AliaseeTv (T Dtype Value)
             | AliaseeConversion (Conversion ScalarB Const)
             | AliaseeConversionV (Conversion VectorB Const)
             | AliaseeGEP (GetElementPtr ScalarB Const)
             | AliaseeGEPV (GetElementPtr VectorB Const)
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


