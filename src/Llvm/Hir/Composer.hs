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


icallcmd :: GlobalId -> [(Dtype, Value)] -> Cinst
icallcmd fname params = I_call_fun TcNon Nothing [] (CallSiteRet $ RtypeVoidU Tvoid) (FunId fname)
                        (fmap (\(dt,v) -> ActualParamData dt [] Nothing v []) params) [] Nothing