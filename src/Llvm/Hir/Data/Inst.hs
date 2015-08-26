{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Llvm.Hir.Data.Inst
       ( module Llvm.Hir.Data.Inst
       , module Llvm.Asm.SharedEntity
       , module Llvm.Hir.Data.Type
       , module Data.Word
       , Label
       ) where
import Llvm.Asm.SharedEntity hiding (GlobalId (..), Comdat(..), DollarId(..)) 
import Llvm.Hir.Data.Type
import Compiler.Hoopl (Label)
import Data.Int
import Data.Word (Word8, Word16, Word32, Word64)
import Data.DoubleWord
import Llvm.ErrorLoc

class (Ord n, Eq n, Show n) => AbsName n where
  prepend :: String -> n -> n
  append :: n -> String -> n
  mkName :: String -> n
  
instance AbsName () where  
  prepend _ _ = error "AbsName () cannot be prepended." 
  append _ _ = error "AbsName () cannot be appended."
  mkName _ = error "AbsName () cannot be newed."
  

{- Global name which bind to a static address at link and load time -}
data Gname = Gname String deriving (Eq, Ord, Show)

{- It is required that g is an instance of AbsName -}                         
instance AbsName Gname where
  prepend prefix n = case n of
    Gname n -> Gname (prefix ++ n)

  append n suffix = case n of
    Gname n -> Gname (n ++ suffix)
    
  mkName = Gname

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

data GetElementPtr s v idx = GetElementPtr (IsOrIsNot InBounds) (T (Type s P) v) [T (Type s I) idx]
                           deriving (Eq,Ord,Show)

data Select s r v = Select (Either (T (Type ScalarB I) v) (T (Type s I) v)) 
                    (T (Type s r) v) (T (Type s r) v) deriving (Eq,Ord,Show)

data Icmp s v = Icmp IcmpOp (IntOrPtrType s) v v deriving (Eq,Ord,Show)

data Fcmp s v = Fcmp FcmpOp (Type s F) v v deriving (Eq,Ord,Show)

{- vector operations -}
data ShuffleVector r v = ShuffleVector (T (Type VectorB r) v) (T (Type VectorB r) v) 
                         (T (Type VectorB I) v) deriving (Eq,Ord,Show)

data ExtractElement r v = ExtractElement (T (Type VectorB r) v) (T (Type ScalarB I) v)
                        deriving (Eq,Ord,Show)

data InsertElement r v = InsertElement (T (Type VectorB r) v) (T (Type ScalarB r) v) 
                         (T (Type ScalarB I) v) deriving (Eq,Ord,Show)

{- aggregate operations -}
data ExtractValue v = ExtractValue (T (Type RecordB D) v) [Word32] deriving (Eq,Ord,Show)

data InsertValue v = InsertValue (T (Type RecordB D) v) (T Dtype v) [Word32] deriving (Eq,Ord,Show)

data Const g = C_u8 Word8
             | C_u16 Word16
             | C_u32 Word32
             | C_u64 Word64
             | C_u96 Word96
             | C_u128 Word128
             | C_s8 Int8
             | C_s16 Int16
             | C_s32 Int32
             | C_s64 Int64
             | C_s96 Int96
             | C_s128 Int128
             | C_int String
             | C_uhex_int String
             | C_shex_int String
             | C_float String
             | C_null
             | C_undef
             | C_true
             | C_false
             | C_zeroinitializer
             | C_globalAddr g
             | C_str String
             | C_struct Packing [TypedConstOrNull g]
             | C_vector [TypedConstOrNull g]
             | C_vectorN  Word32 (TypedConstOrNull g)
             | C_array [TypedConstOrNull g]
             | C_arrayN Word64 (TypedConstOrNull g)
             | C_labelId Label
             | C_block g Label
             | C_add (Maybe NoWrap) (Type ScalarB I) (Const g) (Const g)
             | C_sub (Maybe NoWrap) (Type ScalarB I) (Const g) (Const g)
             | C_mul (Maybe NoWrap) (Type ScalarB I) (Const g) (Const g)
             | C_udiv (Maybe Exact) (Type ScalarB I) (Const g) (Const g)
             | C_sdiv (Maybe Exact) (Type ScalarB I) (Const g) (Const g)
             | C_urem (Type ScalarB I) (Const g) (Const g)
             | C_srem (Type ScalarB I) (Const g) (Const g)
             | C_shl (Maybe NoWrap) (Type ScalarB I) (Const g) (Const g)
             | C_lshr (Maybe Exact) (Type ScalarB I) (Const g) (Const g)
             | C_ashr (Maybe Exact) (Type ScalarB I) (Const g) (Const g)
             | C_and (Type ScalarB I) (Const g) (Const g)
             | C_or (Type ScalarB I) (Const g) (Const g)
             | C_xor (Type ScalarB I) (Const g) (Const g)
             | C_add_V (Maybe NoWrap) (Type VectorB I) (Const g) (Const g)
             | C_sub_V (Maybe NoWrap) (Type VectorB I) (Const g) (Const g)
             | C_mul_V (Maybe NoWrap) (Type VectorB I) (Const g) (Const g)
             | C_udiv_V (Maybe Exact) (Type VectorB I) (Const g) (Const g)
             | C_sdiv_V (Maybe Exact) (Type VectorB I) (Const g) (Const g)
             | C_urem_V (Type VectorB I) (Const g) (Const g)
             | C_srem_V (Type VectorB I) (Const g) (Const g)
             | C_shl_V (Maybe NoWrap) (Type VectorB I) (Const g) (Const g)
             | C_lshr_V (Maybe Exact) (Type VectorB I) (Const g) (Const g)
             | C_ashr_V (Maybe Exact) (Type VectorB I) (Const g) (Const g)
             | C_and_V (Type VectorB I) (Const g) (Const g)
             | C_or_V (Type VectorB I) (Const g) (Const g)
             | C_xor_V (Type VectorB I) (Const g) (Const g)
             | C_fadd FastMathFlags (Type ScalarB F) (Const g) (Const g)
             | C_fsub FastMathFlags (Type ScalarB F) (Const g) (Const g)
             | C_fmul FastMathFlags (Type ScalarB F) (Const g) (Const g)
             | C_fdiv FastMathFlags (Type ScalarB F) (Const g) (Const g)
             | C_frem FastMathFlags (Type ScalarB F) (Const g) (Const g)
             | C_fadd_V FastMathFlags (Type VectorB F) (Const g) (Const g)
             | C_fsub_V FastMathFlags (Type VectorB F) (Const g) (Const g)
             | C_fmul_V FastMathFlags (Type VectorB F) (Const g) (Const g)
             | C_fdiv_V FastMathFlags (Type VectorB F) (Const g) (Const g)
             | C_frem_V FastMathFlags (Type VectorB F) (Const g) (Const g)
             | C_trunc (T (Type ScalarB I) (Const g)) (Type ScalarB I)
             | C_zext (T (Type ScalarB I) (Const g)) (Type ScalarB I)
             | C_sext (T (Type ScalarB I) (Const g)) (Type ScalarB I)
             | C_fptrunc (T (Type ScalarB F) (Const g)) (Type ScalarB F)
             | C_fpext (T (Type ScalarB F) (Const g)) (Type ScalarB F)
             | C_fptoui (T (Type ScalarB F) (Const g)) (Type ScalarB I)
             | C_fptosi (T (Type ScalarB F) (Const g)) (Type ScalarB I)
             | C_uitofp (T (Type ScalarB I) (Const g)) (Type ScalarB F)
             | C_sitofp (T (Type ScalarB I) (Const g)) (Type ScalarB F)
             | C_ptrtoint (T (Type ScalarB P) (Const g)) (Type ScalarB I)
             | C_inttoptr (T (Type ScalarB I) (Const g)) (Type ScalarB P)
             | C_bitcast (T Dtype (Const g)) Dtype
             | C_addrspacecast (T (Type ScalarB P) (Const g)) (Type ScalarB P)
             | C_trunc_V (T (Type VectorB I) (Const g)) (Type VectorB I)
             | C_zext_V (T (Type VectorB I) (Const g)) (Type VectorB I)
             | C_sext_V (T (Type VectorB I) (Const g)) (Type VectorB I)
             | C_fptrunc_V (T (Type VectorB F) (Const g)) (Type VectorB F)
             | C_fpext_V (T (Type VectorB F) (Const g)) (Type VectorB F)
             | C_fptoui_V (T (Type VectorB F) (Const g)) (Type VectorB I)
             | C_fptosi_V (T (Type VectorB F) (Const g)) (Type VectorB I)
             | C_uitofp_V (T (Type VectorB I) (Const g)) (Type VectorB F)
             | C_sitofp_V (T (Type VectorB I) (Const g)) (Type VectorB F)
             | C_ptrtoint_V (T (Type VectorB P) (Const g)) (Type VectorB I)
             | C_inttoptr_V (T (Type VectorB I) (Const g)) (Type VectorB P)
             | C_addrspacecast_V (T (Type VectorB P) (Const g)) (Type VectorB P)
             | C_getelementptr (IsOrIsNot InBounds) (T (Type ScalarB P) (Const g)) [T (Type ScalarB I) (Const g)]
             | C_getelementptr_V (IsOrIsNot InBounds) (T (Type VectorB P) (Const g)) [T (Type VectorB I) (Const g)]
             | C_select_I (Select ScalarB I (Const g))
             | C_select_F (Select ScalarB F (Const g))
             | C_select_P (Select ScalarB P (Const g))
             | C_select_First (T (Type ScalarB I) (Const g)) (T (Type FirstClassB D) (Const g)) (T (Type FirstClassB D) (Const g))
             | C_select_VI (Select VectorB I (Const g))
             | C_select_VF (Select VectorB F (Const g))
             | C_select_VP (Select VectorB P (Const g))
             | C_icmp (Icmp ScalarB (Const g))
             | C_icmp_V (Icmp VectorB (Const g))
             | C_fcmp (Fcmp ScalarB (Const g))
             | C_fcmp_V (Fcmp VectorB (Const g))
             | C_shufflevector_I (ShuffleVector I (Const g))
             | C_shufflevector_F (ShuffleVector F (Const g))
             | C_shufflevector_P (ShuffleVector P (Const g))
             | C_extractelement_I (ExtractElement I (Const g))
             | C_extractelement_F (ExtractElement F (Const g))
             | C_extractelement_P (ExtractElement P (Const g))
             | C_insertelement_I (InsertElement I (Const g))
             | C_insertelement_F (InsertElement F (Const g))
             | C_insertelement_P (InsertElement P (Const g))
             | C_extractvalue (ExtractValue (Const g))
             | C_insertvalue (InsertValue (Const g))
             deriving (Eq, Ord, Show)



data MdName = MdName String deriving (Eq,Ord,Show)
data MdNode = MdNode Word32 deriving (Eq,Ord,Show)

data MdRef = MdRefName MdName
           | MdRefNode MdNode
           deriving (Eq, Ord, Show)

data MetaConst g = McStruct [MetaKindedConst g]
                 | McString DqString
                 | McMdRef MdRef
                 | McSsa LocalId
                 | McSimple (Const g)
                 deriving (Eq,Ord,Show)

data MetaKindedConst g = MetaKindedConst MetaKind (MetaConst g)
                       | UnmetaKindedNull
                       deriving (Eq, Ord, Show)

data FunPtr g = FunId g
              | FunIdBitcast (T Dtype (Const g)) Dtype
              | FunIdInttoptr (T Dtype (Const g)) Dtype
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

data CallFunInterface g = CallFunInterface { cfi_tail :: TailCall
                                           , cfi_castType :: Maybe (Type ScalarB P)
                                           , cfi_signature :: FunSignature (Value g)
                                           , cfi_funAttrs :: [FunAttr]
                                           } deriving (Eq, Ord, Show)

data InvokeFunInterface g = InvokeFunInterface { ifi_castType :: Maybe (Type ScalarB P)
                                               , ifi_signature :: FunSignature (Value g)
                                               , ifi_funAttrs :: [FunAttr]
                                               } deriving (Eq, Ord, Show)

data CallAsmInterface g = CallAsmInterface { cai_type :: Type CodeFunB X 
                                           , cai_sideeffect :: Maybe SideEffect
                                           , cai_alignstack :: Maybe AlignStack
                                           , cai_actualParams :: [FunOperand (Value g)]
                                           , cai_funAttrs :: [FunAttr]
                                           } deriving (Eq, Ord, Show)


data Clause g = Catch (T Dtype (Value g))
              | Filter (TypedConstOrNull g)
              | CcoS (Conversion ScalarB (Value g))
              | CcoV (Conversion VectorB (Value g))
              deriving (Eq,Ord,Show)

data Dbg g = Dbg MdRef (MetaConst g) deriving (Eq, Ord, Show)

-- | Phi Instrunction merge ssa values from multiple incomming dataflows.
-- | It is a standalone type due to its unique position, always immediately following up Lnode, in a basic block
data Pinst g = Pinst { ftype :: Ftype
                     , flowins :: [(Value g, Label)]
                     , flowout :: LocalId
                     } deriving (Eq, Ord, Show)


-- | Computation Instructions compute and cause side effects
data Cinst g where {
  I_alloca :: { inAllocaAttr :: IsOrIsNot InAllocaAttr
              , dtype :: Dtype
              , size :: Maybe (T (Type ScalarB I) (Value g))
              , alignment :: Maybe AlignInByte
              , result :: LocalId
              } -> Cinst g;

  I_load :: { volatile :: IsOrIsNot Volatile
            , pointer :: T (Type ScalarB P) (Value g)
            , alignment :: Maybe AlignInByte
            , temporal :: Maybe Nontemporal
            , invariantLoad :: Maybe InvariantLoad
            , nonull :: Maybe Nonnull
            , result :: LocalId
            } -> Cinst g;

  I_loadatomic :: { atomicity :: Atomicity
                  , volatile :: IsOrIsNot Volatile
                  , pointer :: T (Type ScalarB P) (Value g)
                  , alignment :: Maybe AlignInByte
                  , result :: LocalId
                  } -> Cinst g;

  I_store :: { volatile :: IsOrIsNot Volatile
             , storedvalue :: T Dtype (Value g)
             , pointer :: T (Type ScalarB P) (Value g)
             , alignment :: Maybe AlignInByte
             , nontemporal :: Maybe Nontemporal
             } -> Cinst g;

  I_storeatomic :: { atomicity :: Atomicity
                   , volatile :: IsOrIsNot Volatile
                   , storedvalue :: T Dtype (Value g)
                   , pointer :: T (Type ScalarB P) (Value g)
                   , alignment :: Maybe AlignInByte
                   } -> Cinst g;

  I_fence :: { singleThread :: IsOrIsNot SingleThread
             , ordering :: AtomicMemoryOrdering
             } -> Cinst g;

  I_cmpxchg_I :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer ::  T (Type ScalarB P) (Value g)
                 , cmpi :: T (Type ScalarB I) (Value g)
                 , newi :: T (Type ScalarB I) (Value g)
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst g;

  I_cmpxchg_F :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer :: T (Type ScalarB P) (Value g)
                 , cmpf :: T (Type ScalarB F) (Value g)
                 , newf :: T (Type ScalarB F) (Value g)
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst g;

  I_cmpxchg_P :: { weak :: IsOrIsNot Weak
                 , volatile :: IsOrIsNot Volatile
                 , pointer :: T (Type ScalarB P) (Value g)
                 , cmpp :: T (Type ScalarB P) (Value g)
                 , newp :: T (Type ScalarB P) (Value g)
                 , singlethread :: IsOrIsNot SingleThread
                 , success_ordering :: AtomicMemoryOrdering
                 , failure_ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst g;

  I_atomicrmw :: { volatile :: IsOrIsNot Volatile
                 , atomicOp :: AtomicOp
                 , pointer :: T (Type ScalarB P) (Value g)
                 , val :: T (Type ScalarB I) (Value g)
                 , singlethread :: IsOrIsNot SingleThread
                 , ordering :: AtomicMemoryOrdering
                 , result :: LocalId
                 } -> Cinst g;

  I_call_fun :: { call_ptr :: FunPtr g
                , call_fun_interface :: CallFunInterface g
                , call_return :: Maybe LocalId
                } -> Cinst g;

  I_call_asm :: { call_asmcode :: AsmCode
                , call_asm_interface :: CallAsmInterface g
                , call_return :: Maybe LocalId
                } -> Cinst g;

  I_extractelement_I :: { vectorI :: T (Type VectorB I) (Value g)
                        , index :: T (Type ScalarB I) (Value g)
                        , result :: LocalId
                        } -> Cinst g;

  I_extractelement_F :: { vectorF :: T (Type VectorB F) (Value g)
                        , index :: T (Type ScalarB I) (Value g)
                        , result :: LocalId
                        } -> Cinst g;

  I_extractelement_P :: { vectorP :: T (Type VectorB P) (Value g)
                        , index :: T (Type ScalarB I) (Value g)
                        , result :: LocalId
                        } -> Cinst g;

  I_insertelement_I :: { vectorI :: T (Type VectorB I) (Value g)
                       , elementI :: T (Type ScalarB I) (Value g)
                       , index :: T (Type ScalarB I) (Value g)
                       , result :: LocalId
                       } -> Cinst g;

  I_insertelement_F :: { vectorF :: T (Type VectorB F) (Value g)
                       , elementF :: T (Type ScalarB F) (Value g)
                       , index :: T (Type ScalarB I) (Value g)
                       , result :: LocalId
                       } -> Cinst g;

  I_insertelement_P :: { vectorP :: T (Type VectorB P) (Value g)
                       , elementP :: T (Type ScalarB P) (Value g)
                       , index :: T (Type ScalarB I) (Value g)
                       , result :: LocalId
                       } -> Cinst g;

  I_shufflevector_I :: { vector1I :: T (Type VectorB I) (Value g)
                       , vector2I :: T (Type VectorB I) (Value g)
                       , vectorIdx :: T (Type VectorB I) (Value g)
                       , result :: LocalId
                       } -> Cinst g;

  I_shufflevector_F :: { vector1F :: T (Type VectorB F) (Value g)
                       , vector2F :: T (Type VectorB F) (Value g)
                       , vectorIdx :: T (Type VectorB I) (Value g)
                       , result :: LocalId
                       } -> Cinst g;

  I_shufflevector_P :: { vector1P :: T (Type VectorB P) (Value g)
                       , vector2P :: T (Type VectorB P) (Value g)
                       , vectorIdx :: T (Type VectorB I) (Value g)
                       , result :: LocalId
                       } -> Cinst g;

  I_extractvalue :: { record :: T (Type RecordB D) (Value g)
                    , windices :: [Word32]
                    , result :: LocalId
                    } ->  Cinst g;

  I_insertvalue :: { record :: T (Type RecordB D) (Value g)
                   , element :: T Dtype (Value g)
                   , windices :: [Word32]
                   , result :: LocalId
                   } ->  Cinst g;

  I_landingpad :: { resultType :: Dtype
                  , persFnType :: Dtype
                  , persFn :: FunPtr g
                  , cleanup :: Maybe Cleanup
                  , clauses :: [Clause g]
                  , result :: LocalId
                  } -> Cinst g;

  I_getelementptr :: { inBounds :: IsOrIsNot InBounds
                     , pointer :: T (Type ScalarB P) (Value g)
                     , indices :: [T (Type ScalarB I) (Value g)]
                     , result :: LocalId
                     } -> Cinst g;

  I_getelementptr_V :: { inBounds :: IsOrIsNot InBounds
                       , vpointer :: T (Type VectorB P) (Value g)
                       , vindices :: [T (Type VectorB I) (Value g)]
                       , result :: LocalId
                       } -> Cinst g;

  {- Scalar Integer cmp -}
  I_icmp :: { icmpOp :: IcmpOp
            , icmpType :: IntOrPtrType ScalarB
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  {- Vector Integer cmp -}
  I_icmp_V :: { icmpOp :: IcmpOp
              , icmpTypeV :: IntOrPtrType VectorB
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  {- Scalar Float cmp -}
  I_fcmp :: { fcmpOp :: FcmpOp
            , fcmpTypeF :: Type ScalarB F
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_fcmp_V :: { fcmpOp :: FcmpOp
              , fcmpTypeVF :: Type VectorB F
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  {-- int bin exp --}
  I_add :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: (Value g)
           , operand2 :: (Value g)
           , result :: LocalId
           } -> Cinst g;

  I_sub :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: (Value g)
           , operand2 :: (Value g)
           , result :: LocalId
           } -> Cinst g;

  I_mul :: { flagI :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: (Value g)
           , operand2 :: (Value g)
           , result :: LocalId
           } -> Cinst g;

  I_udiv :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_sdiv :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_urem :: { typeI :: Type ScalarB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_srem :: { typeI :: Type ScalarB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_shl :: { flagW :: Maybe NoWrap
           , typeI :: Type ScalarB I
           , operand1 :: (Value g)
           , operand2 :: (Value g)
           , result :: LocalId
           } -> Cinst g;

  I_lshr :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_ashr :: { flagE :: Maybe Exact
            , typeI :: Type ScalarB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_and :: { typeI :: Type ScalarB I
           , operand1 :: (Value g)
           , operand2 :: (Value g)
           , result :: LocalId
           } -> Cinst g;

  I_or :: { typeI :: Type ScalarB I
          , operand1 :: (Value g)
          , operand2 :: (Value g)
          , result :: LocalId
          } -> Cinst g;

  I_xor :: { typeI :: Type ScalarB I
           , operand1 :: (Value g)
           , operand2 :: (Value g)
           , result :: LocalId
           } -> Cinst g;

  {-- int bin vector exp --}
  I_add_V :: { flagI :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: (Value g)
             , operand2 :: (Value g)
             , result :: LocalId
             } -> Cinst g;

  I_sub_V :: { flagI :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: (Value g)
             , operand2 :: (Value g)
             , result :: LocalId
             } -> Cinst g;

  I_mul_V :: { flagI :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: (Value g)
             , operand2 :: (Value g)
             , result :: LocalId
             } -> Cinst g;

  I_udiv_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_sdiv_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_urem_V :: { typeVI :: Type VectorB I
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_srem_V :: { typeVI :: Type VectorB I
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_shl_V :: { flagW :: Maybe NoWrap
             , typeVI :: Type VectorB I
             , operand1 :: (Value g)
             , operand2 :: (Value g)
             , result :: LocalId
             } -> Cinst g;

  I_lshr_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_ashr_V :: { flagE :: Maybe Exact
              , typeVI :: Type VectorB I
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_and_V :: { typeVI :: Type VectorB I
             , operand1 :: (Value g)
             , operand2 :: (Value g)
             , result :: LocalId
             } -> Cinst g;

  I_or_V :: { typeVI :: Type VectorB I
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_xor_V :: { typeVI :: Type VectorB I
             , operand1 :: (Value g)
             , operand2 :: (Value g)
             , result :: LocalId
             } -> Cinst g;

  {- float bin exp -}
  I_fadd :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_fsub :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_fmul :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_fdiv :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  I_frem :: { flagF :: FastMathFlags
            , typeF :: Type ScalarB F
            , operand1 :: (Value g)
            , operand2 :: (Value g)
            , result :: LocalId
            } -> Cinst g;

  {- float bin exp -}
  I_fadd_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_fsub_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_fmul_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_fdiv_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  I_frem_V :: { flagF :: FastMathFlags
              , typeVF :: Type VectorB F
              , operand1 :: (Value g)
              , operand2 :: (Value g)
              , result :: LocalId
              } -> Cinst g;

  {-- Scalar conversion --}
  I_trunc :: { srcI :: T (Type ScalarB I) (Value g)
             , toI :: Type ScalarB I
             , result :: LocalId
             } -> Cinst g;

  I_zext :: { srcI :: T (Type ScalarB I) (Value g)
            , toI :: Type ScalarB I
            , result :: LocalId
            } -> Cinst g;

  I_sext :: { srcI :: T (Type ScalarB I) (Value g)
            , toI :: Type ScalarB I
            , result :: LocalId
            } -> Cinst g;

  I_fptrunc ::  { srcF :: T (Type ScalarB F) (Value g)
                , toF :: Type ScalarB F
                , result :: LocalId
                } -> Cinst g;

  I_fpext ::  { srcF :: T (Type ScalarB F) (Value g)
              , toF :: Type ScalarB F
              , result :: LocalId
              } -> Cinst g;

  I_fptoui :: { srcF :: T (Type ScalarB F) (Value g)
              , toI :: Type ScalarB I
              , result :: LocalId
              } -> Cinst g;

  I_fptosi :: { srcF ::T (Type ScalarB F) (Value g)
              , toI :: Type ScalarB I
              , result :: LocalId
              } -> Cinst g;

  I_uitofp :: { srcI :: T (Type ScalarB I) (Value g)
              , toF :: Type ScalarB F
              , result :: LocalId
              } -> Cinst g;

  I_sitofp :: { srcI :: T (Type ScalarB I) (Value g)
              , toF :: Type ScalarB F
              , result :: LocalId
              } -> Cinst g;

  I_ptrtoint :: { srcP :: T (Type ScalarB P) (Value g)
                , toI :: Type ScalarB I
                , result :: LocalId
                } -> Cinst g;

  I_inttoptr :: { srcI :: T (Type ScalarB I) (Value g)
                , toP :: Type ScalarB P
                , result :: LocalId
                } -> Cinst g;

  I_addrspacecast :: { srcP :: T (Type ScalarB P) (Value g)
                     , toP :: Type ScalarB P
                     , result :: LocalId
                     } -> Cinst g;

  I_bitcast :: { srcP :: T (Type ScalarB P) (Value g)
               , toP :: Type ScalarB P
               , result :: LocalId
               } -> Cinst g;

  I_bitcast_D :: { srcD :: T Dtype (Value g)
                 , toD :: Dtype
                 , result :: LocalId
                 } -> Cinst g;

  {-- Vector conversion --}
  I_trunc_V :: { srcVI :: T (Type VectorB I) (Value g)
               , toVI :: Type VectorB I
               , result :: LocalId
               } -> Cinst g;

  I_zext_V :: { srcVI :: T (Type VectorB I) (Value g)
              , toVI :: Type VectorB I
              , result :: LocalId
              } -> Cinst g;

  I_sext_V :: { srcVI :: T (Type VectorB I) (Value g)
              , toVI :: Type VectorB I
              , result :: LocalId
              } -> Cinst g;

  I_fptrunc_V :: { srcVF :: T (Type VectorB F) (Value g)
                 , toVF :: Type VectorB F
                 , result :: LocalId
                 } -> Cinst g;

  I_fpext_V :: { srcVF :: T (Type VectorB F) (Value g)
               , toVF :: Type VectorB F
               , result :: LocalId
               } -> Cinst g;

  I_fptoui_V :: { srcVF :: T (Type VectorB F) (Value g)
                , toVI :: Type VectorB I
                , result :: LocalId
                } -> Cinst g;

  I_fptosi_V :: { srcVF :: T (Type VectorB F) (Value g)
                , toVI :: Type VectorB I
                , result :: LocalId
                } -> Cinst g;

  I_uitofp_V :: { srcVI :: T (Type VectorB I) (Value g)
                , toVF :: Type VectorB F
                , result :: LocalId
                } -> Cinst g;

  I_sitofp_V :: { srcVI :: T (Type VectorB I) (Value g)
                , toVF :: Type VectorB F
                , result :: LocalId
                } -> Cinst g;

  I_ptrtoint_V :: { srcVP :: T (Type VectorB P) (Value g)
                  , toVI :: Type VectorB I
                  , result :: LocalId } -> Cinst g;

  I_inttoptr_V :: { srcVI :: T (Type VectorB I) (Value g)
                  , toVP :: Type VectorB P
                  , result :: LocalId } -> Cinst g;

  I_addrspacecast_V :: { srcVP :: T (Type VectorB P) (Value g)
                       , toVP :: Type VectorB P
                       , result :: LocalId
                       } -> Cinst g;

  I_select_I :: { cond :: T (Type ScalarB I) (Value g)
                , trueI :: T (Type ScalarB I) (Value g)
                , falseI :: T (Type ScalarB I) (Value g)
                , result :: LocalId
                } -> Cinst g;

  I_select_F :: { cond :: T (Type ScalarB I) (Value g)
                , trueF :: T (Type ScalarB F) (Value g)
                , falseF :: T (Type ScalarB F) (Value g)
                , result :: LocalId
                } -> Cinst g;

  I_select_P :: { cond :: T (Type ScalarB I) (Value g)
                , trueP :: T (Type ScalarB P) (Value g)
                , falseP :: T (Type ScalarB P) (Value g)
                , result :: LocalId
                } -> Cinst g;

  I_select_VI :: { condVI :: Either (T (Type ScalarB I) (Value g)) (T (Type VectorB I) (Value g))
                 , trueVI :: T (Type VectorB I) (Value g)
                 , falseVI :: T (Type VectorB I) (Value g)
                 , result :: LocalId
                 } -> Cinst g;

  I_select_VF :: { condVF :: Either (T (Type ScalarB I) (Value g)) (T (Type VectorB I) (Value g))
                 , trueVF :: T (Type VectorB F) (Value g)
                 , falseVF :: T (Type VectorB F) (Value g)
                 , result :: LocalId
                 } -> Cinst g;

  I_select_VP :: { condV :: Either (T (Type ScalarB I) (Value g)) (T (Type VectorB I) (Value g))
                 , trueVP :: T (Type VectorB P) (Value g)
                 , falseVP :: T (Type VectorB P) (Value g)
                 , result :: LocalId
                 } -> Cinst g;

  I_select_First :: { cond :: T (Type ScalarB I) (Value g)
                    , trueFirst :: T (Type FirstClassB D) (Value g)
                    , falseFirst :: T (Type FirstClassB D) (Value g)
                    , result :: LocalId
                    } -> Cinst g;

  {-- llvm intrinsic function calls --}
  I_va_arg :: { dv :: T Dtype (Value g)
              , typeD :: Dtype
              , result :: LocalId
              } -> Cinst g;

  I_llvm_va_start :: { arglist :: (Value g) {- i8 * -} } -> Cinst g;
  I_llvm_va_end :: { arglist :: (Value g)  {- i8 * -} } -> Cinst g;
  I_llvm_va_copy :: { destarglist :: (Value g) {- i8 * -}
                    , srcarglist :: (Value g) {- i8 * -}
                    } -> Cinst g;

  I_llvm_gcroot :: { ptrloc :: (Value g)
                   , metadata :: (Value g) {- i8 * -}
                   } -> Cinst g;

  I_llvm_gcread :: { reference :: (Value g)
                   , readFrom :: (Value g)
                   } -> Cinst g;

  I_llvm_gcwrite :: { p1 :: (Value g)
                    , obj :: (Value g)
                    , p2 :: (Value g)
                    } -> Cinst g;

  I_llvm_returnaddress :: { level :: (Value g) {- i32 -} } -> Cinst g;
  I_llvm_frameaddress :: { level :: (Value g) {- i32 -} } -> Cinst g;
  I_llvm_frameescape :: [(Value g)] -> Cinst g;
  I_llvm_framerecover :: (Value g) -> (Value g) -> (Value g) -> Cinst g;

  I_llvm_read_register :: { memLen :: MemLen
                          , meta :: MetaKindedConst g
                          , result :: LocalId
                          } -> Cinst g;

  I_llvm_write_register :: { memLen :: MemLen
                           , meta :: MetaKindedConst g
                           , value :: (Value g)
                           } -> Cinst g;
  I_llvm_stacksave :: { result :: LocalId } -> Cinst g;
  I_llvm_stackrestore :: { pointer :: T (Type ScalarB P) (Value g) } -> Cinst g;
  I_llvm_prefetch :: (Value g) -> (Value g) -> (Value g) -> (Value g) -> Cinst g;
  I_llvm_pcmarker :: (Value g) -> Cinst g;
  I_llvm_readcyclecounter :: { result :: LocalId } -> Cinst g;
  I_llvm_clear_cache :: (Value g) -> (Value g) -> Cinst g;
  I_llvm_instprof_increment :: (Value g) -> (Value g) -> (Value g) -> (Value g) -> Cinst g;

  I_llvm_memcpy :: { memlen :: MemLen
                   , dest :: T (Type ScalarB P) (Value g)
                   , src :: T (Type ScalarB P) (Value g)
                   , len :: T (Type ScalarB I) (Value g)
                   , align :: T (Type ScalarB I) (Value g)
                   , isvolatile :: T (Type ScalarB I) (Value g)
                   } -> Cinst g;

  I_llvm_memmove :: { memlen :: MemLen
                    , dest :: T (Type ScalarB P) (Value g)
                    , src :: T (Type ScalarB P) (Value g)
                    , len :: T (Type ScalarB I) (Value g)
                    , align :: T (Type ScalarB I) (Value g)
                    , isvolatile :: T (Type ScalarB I) (Value g)
                    } -> Cinst g;

  I_llvm_memset :: { memlen :: MemLen
                   , dest :: T (Type ScalarB P) (Value g)
                   , setValue :: T (Type ScalarB I) (Value g)
                   , len :: T (Type ScalarB I) (Value g)
                   , align :: T (Type ScalarB I) (Value g)
                   , isvolatile :: T (Type ScalarB I) (Value g)
                   } -> Cinst g;

  I_llvm_libm_una :: { muop :: LibmUnaryExp g
                     , result :: LocalId
                     } -> Cinst g;

  I_llvm_libm_bin :: { mbop :: LibmBinaryExp g
                     , result :: LocalId
                     } -> Cinst g;

  I_llvm_powi :: { realOperand :: T (Type ScalarB F) (Value g)
                 , intOperand :: T (Type ScalarB I) (Value g)
                 , result :: LocalId
                 } -> Cinst g;

  I_llvm_bitset_test :: (Value g) -> (Value g) -> LocalId -> Cinst g;
  I_llvm_donothing :: Cinst g;
  I_llvm_ctpop :: { suffix :: String
                  , dv :: T Dtype (Value g)
                  , result :: LocalId
                  } -> Cinst g;
  I_llvm_lifetime_start :: { objsize :: T (Type ScalarB I) (Value g) 
                           , pointer :: T (Type ScalarB P) (Value g)
                           } -> Cinst g;
  I_llvm_lifetime_end :: { objsize :: T (Type ScalarB I) (Value g)
                         , pointer :: T (Type ScalarB P) (Value g)
                         } -> Cinst g;
  } deriving (Eq, Ord, Show)


data LibmUnaryExp g = Sqrt (Type ScalarB F) (Value g)
                  | Sin (Type ScalarB F)  (Value g)
                  | Cos (Type ScalarB F) (Value g)
                  | Exp (Type ScalarB F) (Value g)
                  | Exp2 (Type ScalarB F) (Value g)
                  | Log (Type ScalarB F) (Value g)
                  | Log10 (Type ScalarB F) (Value g)
                  | Log2 (Type ScalarB F) (Value g)
                  | Fabs (Type ScalarB F) (Value g)
                  | Floor (Type ScalarB F) (Value g)
                  | Ceil (Type ScalarB F) (Value g)
                  | Ftrunc (Type ScalarB F) (Value g)
                  | Rint (Type ScalarB F) (Value g)
                  | NearByInt (Type ScalarB F) (Value g)
                  | Round (Type ScalarB F) (Value g)
                  deriving (Eq, Ord, Show)

data LibmBinaryExp g = Pow (Type ScalarB F) (Value g) (Value g)
                     | Minnum (Type ScalarB F) (Value g) (Value g)
                     | Maxnum (Type ScalarB F) (Value g) (Value g)
                     | CopySign (Type ScalarB F) (Value g) (Value g)
                     deriving (Eq, Ord, Show)

data MemLen = MemLenI32
            | MemLenI64 deriving (Eq, Ord, Show)

-- | LLVM metadata instructions
data Minst g = Minst CallSiteType g [MetaOperand g]
             | M_llvm_dbg_declare (MetaOperand g) (MetaOperand g)
             | M_llvm_dbg_func_start (MetaOperand g)
             | M_llvm_dbg_stoppoint (MetaOperand g) (MetaOperand g) (MetaOperand g)
             | M_llvm_dbg_value (MetaOperand g) (MetaOperand g) (MetaOperand g)
             | M_llvm_dbg_region_end (MetaOperand g)
             deriving (Eq, Ord, Show)




data MetaOperand g = MetaOperandMeta (MetaKindedConst g)
                   | MetaOperandData Dtype [ParamAttr] (Maybe AlignInByte) (Value g)
                   deriving (Eq, Ord, Show)
{- -}
-- | Terminator instructions cause control flow transferring and
-- | side effects (which is unfortunately difficult to seperate out)
data Tinst g = T_unreachable
           | T_ret_void
           | T_return [T Dtype (Value g)]
           | T_br Label
           | T_cbr { condition :: (Value g)
                   , trueL :: Label
                   , falseL :: Label
                   }
           | T_indirectbr (T (Type ScalarB P) (Value g)) [Label]
           | T_switch { defaultcase :: (T (Type ScalarB I) (Value g), Label)
                      , othercases :: [(T (Type ScalarB I) (Value g), Label)]
                      }
           | T_invoke { invoke_ptr :: FunPtr g
                      , invoke_fun_interface :: InvokeFunInterface g
                      , invoke_normal_label :: Label
                      , invoke_exception_label :: Label
                      , invoke_return :: Maybe LocalId
                      }
           | T_invoke_asm { invoke_asmcode :: AsmCode
                          , invoke_asm_interface :: CallAsmInterface g
                          , invoke_normal_label :: Label
                          , invoke_exception_label :: Label
                          , invoke_return :: Maybe LocalId
                          }
           | T_resume (T Dtype (Value g))
           | T_unwind
           deriving (Eq, Ord, Show)

data Value g = Val_ssa LocalId
             | Val_const (Const g)
             deriving (Eq,Ord,Show)

data T t v = T t v deriving (Eq, Ord, Show)

data Aliasee g = Aliasee g
               | AliaseeTyped Dtype (Aliasee g)
               | AliaseeConversion (Conversion ScalarB (Aliasee g))
               | AliaseeGEP (GetElementPtr ScalarB (Aliasee g) (Const g))
               deriving (Eq, Ord, Show)

data Comdat g = Comdat (Maybe g) deriving (Eq, Ord, Show)

data FunctionInterface g = FunctionInterface { fi_linkage :: Maybe Linkage
                                             , fi_visibility :: Maybe Visibility
                                             , fi_dllstorage :: Maybe DllStorageClass
                                             , fi_signature :: FunSignature LocalId
                                             , fi_fun_name :: g
                                             , fi_addr_naming :: Maybe AddrNaming
                                             , fi_fun_attrs :: [FunAttr]
                                             , fi_section :: Maybe Section
                                             , fi_comdat :: Maybe (Comdat g)
                                             , fi_alignment :: Maybe AlignInByte
                                             , fi_gc :: Maybe Gc
                                             , fi_prefix :: Maybe (Prefix g)
                                             , fi_prologue :: Maybe (Prologue g)
                                             } deriving (Eq,Ord,Show)

data MetaFunParam = MetaFunParam MetaKind Fparam deriving (Eq, Ord, Show)

data Prefix g = Prefix (TypedConstOrNull g) deriving (Eq, Ord, Show)
data Prologue g = Prologue (TypedConstOrNull g) deriving (Eq, Ord, Show)

data TypedConstOrNull g = TypedConst (T Dtype (Const g))
                        | UntypedNull deriving (Eq, Ord, Show)

data FunSignature a = FunSignature { fs_callConv ::  CallConv
                                   , fs_type :: Type CodeFunB X
                                   , fs_params :: [FunOperand a]
                                   } deriving (Eq, Ord, Show)

data FunOperand a = FunOperandData Dtype [PAttr] (Maybe AlignInByte) a
                  | FunOperandExt Ext Dtype [PAttr] (Maybe AlignInByte) a
                  | FunOperandAsRet Dtype [PAttr] (Maybe AlignInByte) a
                  | FunOperandByVal Dtype [PAttr] (Maybe AlignInByte) a
                  | FunOperandLabel (Type CodeLabelB X) [PAttr] (Maybe AlignInByte) a
                  deriving (Eq,Ord,Show)
                           
data PAttr = PInReg
            | PInAlloca
            | PNoAlias
            | PNoCapture
            | PNest
            | PReturned
            | PNonNull
            | PDereferenceable Word32
            | PReadOnly
            | PReadNone
            deriving (Eq,Ord,Show)
                           