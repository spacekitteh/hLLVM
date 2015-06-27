{-# LANGUAGE CPP, TemplateHaskell #-}
module Llvm.Hir.Composer where

#define FLC  (FileLoc $(srcLoc))

import Llvm.Hir.Data.Type
import Llvm.Hir.Data.Inst
import Llvm.Hir.Data.Module
import Llvm.Hir.Cast
import Llvm.ErrorLoc

void :: Type NoB U
void = Tvoid

i1 :: Type ScalarB I
i1 = TpI 1

i8 :: Type ScalarB I
i8 = TpI 8

i16 :: Type ScalarB I
i16 = TpI 16

i32 :: Type ScalarB I
i32 = TpI 32

i64 :: Type ScalarB I
i64 = TpI 64

i128 :: Type ScalarB I
i128 = TpI 128

ptr0 :: Type s r -> Type ScalarB P
ptr0 t = Tpointer (dcast FLC t) 0

half :: Type ScalarB F
half = TpHalf

float :: Type ScalarB F
float = TpFloat

double :: Type ScalarB F
double = TpDouble

fp128 :: Type ScalarB F
fp128 = TpFp128

x86_fp80 :: Type ScalarB F
x86_fp80 = TpX86Fp80


isInbounds :: IsOrIsNot InBounds
isInbounds = Is InBounds

iload :: T (Type ScalarB P) Value -> LocalId -> Cinst
iload p r = I_load (IsNot Volatile) p Nothing Nothing Nothing Nothing r

istore :: T Dtype Value -> T (Type ScalarB P) Value -> Cinst
istore v p = I_store (IsNot Volatile) v p Nothing Nothing


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

icallcmd :: GlobalId -> [(Dtype, Value)] -> Cinst
icallcmd fname params = I_call_fun (FunId fname) 
                        CallFunInterface { cfi_tail = TcNon 
                                         , cfi_castType = Nothing
                                         , cfi_signature = FunSignature { fs_callConv = Ccc 
                                                                        , fs_type = Tfunction (RtypeVoidU Tvoid,[]) 
                                                                                    (fmap (\x ->  (MtypeData $ fst x, Nothing)) params) Nothing
                                                                        , fs_params = fmap (\(dt,v) -> FunOperandData dt [] Nothing v) params 
                                                                        } 
                                         , cfi_funAttrs = [] 
                                         } Nothing

icallfun :: GlobalId -> [(Dtype, Value)] -> Dtype -> LocalId -> Cinst
icallfun fname params retType rid = 
  let funType = Tfunction (ucast retType, []) (fmap (\x -> (MtypeData $ fst x, Nothing)) params) Nothing
  in 
  I_call_fun (FunId fname) 
  CallFunInterface { cfi_tail = TcNon 
                   , cfi_castType = Nothing
                   , cfi_signature = FunSignature { fs_callConv = Ccc 
                                                  , fs_type = funType
                                                  , fs_params = fmap (\(dt,v) -> FunOperandData dt [] Nothing v) params 
                                                  } 
                   , cfi_funAttrs = []
                   } (Just rid)

llvm_sizeof :: Dtype -> Type ScalarB I -> Const
llvm_sizeof s intType = let start = C_getelementptr (Is InBounds) (T (Tpointer (ucast s) 0) C_null) [toTC (0::Word32)]
                            end = C_getelementptr (Is InBounds) (T (Tpointer (ucast s) 0) C_null) [toTC (1::Word32)]
                        in C_sub Nothing intType (toInt end) (toInt start)
  where toInt ptr = C_ptrtoint (T (Tpointer (ucast s) 0) ptr) intType