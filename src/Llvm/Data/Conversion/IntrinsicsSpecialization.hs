{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Llvm.Data.Conversion.IntrinsicsSpecialization where

import Llvm.Data.CoreIr
import Data.Maybe

#define FLC  (FileLoc $(srcLoc))

specializeCallSite :: Maybe LocalId -> CallSite -> Maybe Cinst
specializeCallSite lhs csi = case csi of
  CsFun Nothing [] _ (FunId (GlobalIdAlphaNum "llvm.va_start"))
    [ActualParamData t1 [] Nothing v []] [] | isNothing lhs -> Just $ I_llvm_va_start v
  CsFun Nothing [] _ (FunId (GlobalIdAlphaNum "llvm.va_end"))
    [ActualParamData t1 [] Nothing v []] [] | isNothing lhs -> Just $ I_llvm_va_end v
  CsFun Nothing [] _ (FunId (GlobalIdAlphaNum nm))
    [ActualParamData t1 [] Nothing v1 [] -- dest
    ,ActualParamData t2 [] Nothing v2 [] -- src
    ,ActualParamData t3 [] Nothing v3 [] -- len
    ,ActualParamData t4 [] Nothing v4 [] -- align
    ,ActualParamData t5 [] Nothing v5 [] -- volatile
    ] [] | isNothing lhs && (nm == "llvm.memcpy.p0i8.p0i8.i32" || nm == "llvm.memcpy.p0i8.p0i8.i64") 
           -> 
    let mod = case nm of
          "llvm.memcpy.p0i8.p0i8.i32" -> MemLenI32
          "llvm.memcpy.p0i8.p0i8.i64" -> MemLenI64                         
    in Just $ I_llvm_memcpy mod (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
       (T (dcast FLC t4) v4)
       (T (dcast FLC t5) v5)  
  _ -> Nothing


unspecializeIntrinsics :: Cinst -> Maybe Cinst
unspecializeIntrinsics inst = case inst of
  I_llvm_va_start v -> 
    Just $ I_call_fun TcNon Nothing [] (CallSiteRet (RtypeVoidU Tvoid)) (FunId (GlobalIdAlphaNum "llvm.va_start"))
    [tvToAp (T (ptr0 i8) v)] [] Nothing
  I_llvm_va_end v -> 
    Just $ I_call_fun TcNon Nothing [] (CallSiteRet (RtypeVoidU Tvoid)) (FunId (GlobalIdAlphaNum "llvm.va_end"))
    [tvToAp (T (ptr0 i8) v)] [] Nothing
  I_llvm_memcpy memLen tv1 tv2 tv3 tv4 tv5 -> 
    let nm = case memLen of
          MemLenI32 -> "llvm.memcpy.p0i8.p0i8.i32"
          MemLenI64 -> "llvm.memcpy.p0i8.p0i8.i64"
    in Just $ I_call_fun TcNon Nothing [] (CallSiteRet (RtypeVoidU Tvoid)) (FunId (GlobalIdAlphaNum nm))
       ([tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5]) [] Nothing
  _ -> Nothing
    
tvToAp :: Ucast t Dtype => T t Value -> ActualParam
tvToAp (T t v) = ActualParamData (ucast t) [] Nothing v []
  