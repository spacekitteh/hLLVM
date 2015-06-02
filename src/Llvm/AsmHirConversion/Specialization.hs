{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RecordWildCards #-}
module Llvm.AsmHirConversion.Specialization where

import qualified Llvm.Asm.Data as A
import Llvm.Hir
import Llvm.ErrorLoc
import Data.Maybe


#define FLC  (FileLoc $(srcLoc))


specializeRetAttr :: ParamAttr -> RetAttr    
specializeRetAttr x = case x of
  PaZeroExt -> RetAttrZeroExt
  PaSignExt -> RetAttrSignExt
  PaInReg -> RetAttrInReg
  PaNoAlias -> RetAttrNoAlias
  PaDereferenceable n -> RetAttrDereferenceable n
  _ -> errorLoc FLC $ show x

unspecializeRetAttr :: RetAttr -> ParamAttr  
unspecializeRetAttr x = case x of
  RetAttrZeroExt -> PaZeroExt
  RetAttrSignExt -> PaSignExt
  RetAttrInReg -> PaInReg
  RetAttrNoAlias -> PaNoAlias
  RetAttrDereferenceable n -> PaDereferenceable n


specializeParamAsRetAttr :: ParamAttr -> Maybe ParamAsRetAttr
specializeParamAsRetAttr x = case x of
  PaSRet -> Just ParamAsRetAttr
  _ -> Nothing


unspecializeParamAsRetAttr :: ParamAsRetAttr -> ParamAttr
unspecializeParamAsRetAttr x = case x of
  ParamAsRetAttr -> PaSRet


specializeRegisterIntrinsic :: Maybe LocalId -> A.CallSite -> Maybe (Maybe LocalId, MemLen, [A.ActualParam])
specializeRegisterIntrinsic lhs cs = case (lhs, cs) of
  (Just r, A.CallSiteFun Nothing [] _ (A.FunNameGlobal (GolG (GlobalIdAlphaNum nm))) [m] _) 
    | (nm == "llvm.read_register.i32" || nm == "llvm.read_register.i64") -> 
      let ml = case nm of
            "llvm.read_register.i32" -> MemLenI32
            "llvm.read_register.i64" -> MemLenI64
      in Just (Just r, ml, [m])
  (Nothing, A.CallSiteFun Nothing [] _ (A.FunNameGlobal (GolG (GlobalIdAlphaNum nm))) [m,v] _) 
    | (nm == "llvm.write_register.i32" || nm == "llvm.write_register.i64") -> 
      let ml = case nm of
            "llvm.write_register.i32" -> MemLenI32
            "llvm.write_register.i64" -> MemLenI64
      in Just (Nothing, ml, [m,v])
  (_, _) -> Nothing

unspecializeRegisterIntrinsic :: Cinst -> Maybe (GlobalId, Type ScalarB I, [MetaOperand], Maybe LocalId)
unspecializeRegisterIntrinsic cinst = case cinst of
  I_llvm_read_register mLen mc r ->
    let (nm, typ) = case mLen of
          MemLenI32 -> (GlobalIdAlphaNum "llvm.read_register.i32", i32)
          MemLenI64 -> (GlobalIdAlphaNum "llvm.read_register.i64", i64)
    in Just (nm, typ, [MetaOperandMeta mc], Just r)
  I_llvm_write_register mLen mc val ->
    let (nm, typ) = case mLen of
          MemLenI32 -> (GlobalIdAlphaNum "llvm.write_register.i32", i32)
          MemLenI64 -> (GlobalIdAlphaNum "llvm.write_register.i64", i64)
    in Just (nm, typ, [MetaOperandMeta mc, MetaOperandData (ucast typ) [] Nothing val], Nothing)
  _ -> Nothing


specializeCallSite :: Maybe LocalId -> FunPtr -> CallFunInterface -> Maybe Cinst
specializeCallSite lhs fptr csi = case (fptr, csi) of
  (FunId (GlobalIdAlphaNum "llvm.va_start"),
   CallFunInterface TcNon Ccc [] _ Nothing [CallOperandData t1 [] Nothing v] []) | isNothing lhs -> 
    Just $ I_llvm_va_start v
  (FunId (GlobalIdAlphaNum "llvm.va_end"),
   CallFunInterface TcNon Ccc [] _ Nothing [CallOperandData t1 [] Nothing v] []) | isNothing lhs -> 
    Just $ I_llvm_va_end v
  (FunId (GlobalIdAlphaNum "llvm.va_copy"),
   CallFunInterface TcNon Ccc [] _ Nothing [CallOperandData t1 [] Nothing v1
                                   ,CallOperandData t2 [] Nothing v2] []) | isNothing lhs -> Just $ I_llvm_va_copy v1 v2
  (FunId (GlobalIdAlphaNum nm), 
   CallFunInterface TcNon Ccc [] _ Nothing
   [CallOperandData t1 [] Nothing v1 -- dest
   ,CallOperandData t2 [] Nothing v2 -- src or setValue
   ,CallOperandData t3 [] Nothing v3 -- len
   ,CallOperandData t4 [] Nothing v4 -- align
   ,CallOperandData t5 [] Nothing v5 -- volatile
   ] []) | isNothing lhs && (nm == "llvm.memcpy.p0i8.p0i8.i32" 
                             || nm == "llvm.memcpy.p0i8.p0i8.i64"
                             || nm == "llvm.memmove.p0i8.p0i8.i32"
                             || nm == "llvm.memmove.p0i8.p0i8.i64"
                             || nm == "llvm.memset.p0i8.i32" 
                             || nm == "llvm.memset.p0i8.i64") -> 
    let mod = case nm of
          "llvm.memcpy.p0i8.p0i8.i32" -> I_llvm_memcpy MemLenI32
                                         (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
                                         (T (dcast FLC t4) v4) (T (dcast FLC t5) v5)  
          "llvm.memcpy.p0i8.p0i8.i64" -> I_llvm_memcpy MemLenI64
                                         (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
                                         (T (dcast FLC t4) v4) (T (dcast FLC t5) v5)  
          "llvm.memmove.p0i8.p0i8.i32" -> I_llvm_memmove MemLenI32
                                          (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
                                          (T (dcast FLC t4) v4) (T (dcast FLC t5) v5)  
          "llvm.memmove.p0i8.p0i8.i64" -> I_llvm_memmove MemLenI64
                                          (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
                                          (T (dcast FLC t4) v4) (T (dcast FLC t5) v5)  
          "llvm.memset.p0i8.i32" -> I_llvm_memset MemLenI32
                                    (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
                                    (T (dcast FLC t4) v4) (T (dcast FLC t5) v5)  
          "llvm.memset.p0i8.i64" -> I_llvm_memset MemLenI64
                                    (T (dcast FLC t1) v1) (T (dcast FLC t2) v2) (T (dcast FLC t3) v3)
                                    (T (dcast FLC t4) v4) (T (dcast FLC t5) v5)  
    in Just $ mod
  (FunId (GlobalIdAlphaNum "llvm.stacksave"),
   CallFunInterface TcNon Ccc [] _ Nothing [] []) | isJust lhs -> 
    Just $ I_llvm_stacksave $ fromJust lhs
  (FunId (GlobalIdAlphaNum "llvm.stackrestore"),
   CallFunInterface TcNon Ccc [] _ Nothing [CallOperandData t1 [] Nothing v] []) | isNothing lhs -> 
    Just $ I_llvm_stackrestore (T (dcast FLC t1) v)
  _ -> Nothing


unspecializeIntrinsics :: Cinst -> Maybe Cinst
unspecializeIntrinsics inst = case inst of
  I_llvm_va_start v -> 
    Just $ I_call_fun (FunId (GlobalIdAlphaNum "llvm.va_start")) 
    (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) Nothing [tvToAp (T (ptr0 i8) v)] []) Nothing
  I_llvm_va_end v -> 
    Just $ I_call_fun (FunId (GlobalIdAlphaNum "llvm.va_end"))
    (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) Nothing [tvToAp (T (ptr0 i8) v)] []) Nothing
  I_llvm_va_copy v1 v2 ->
    Just $ I_call_fun (FunId (GlobalIdAlphaNum "llvm.va_copy"))
    (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) 
     Nothing [tvToAp (T (ptr0 i8) v1), tvToAp (T (ptr0 i8) v2)] []) Nothing
  I_llvm_memcpy memLen tv1 tv2 tv3 tv4 tv5 -> 
    let nm = case memLen of
          MemLenI32 -> "llvm.memcpy.p0i8.p0i8.i32"
          MemLenI64 -> "llvm.memcpy.p0i8.p0i8.i64"
    in Just $ I_call_fun (FunId (GlobalIdAlphaNum nm))
       (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) 
        Nothing [tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5] []) Nothing
  I_llvm_memmove memLen tv1 tv2 tv3 tv4 tv5 -> 
    let nm = case memLen of
          MemLenI32 -> "llvm.memmove.p0i8.p0i8.i32"
          MemLenI64 -> "llvm.memmove.p0i8.p0i8.i64"
    in Just $ I_call_fun (FunId (GlobalIdAlphaNum nm))
       (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) 
        Nothing [tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5] []) Nothing
  I_llvm_memset memLen tv1 tv2 tv3 tv4 tv5 -> 
    let nm = case memLen of
          MemLenI32 -> "llvm.memset.p0i8.i32"
          MemLenI64 -> "llvm.memset.p0i8.i64"
    in Just $ I_call_fun (FunId (GlobalIdAlphaNum nm))
       (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) 
        Nothing [tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5] []) Nothing
  I_llvm_stacksave v -> 
    Just $ I_call_fun (FunId (GlobalIdAlphaNum "llvm.stacksave")) 
    (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeScalarP $ ptr0 i8)) Nothing [] []) (Just v)
  I_llvm_stackrestore tv -> 
    Just $ I_call_fun (FunId (GlobalIdAlphaNum "llvm.stackrestore")) 
    (CallFunInterface TcNon Ccc [] (CallSiteTypeRet (RtypeVoidU Tvoid)) Nothing [tvToAp tv] []) Nothing
  _ -> Nothing
    
tvToAp :: Ucast t Dtype => T t Value -> CallOperand
tvToAp (T t v) = CallOperandData (ucast t) [] Nothing v
                 
specializeTlGlobal :: TlGlobal -> Maybe TlIntrinsic
specializeTlGlobal tl = case tl of
  TlGlobalDtype {..} -> case tlg_lhs of
    GlobalIdAlphaNum nm | (nm == "llvm.used" 
                           || nm == "llvm.compiler.used" 
                           || nm == "llvm.global_ctors" 
                           || nm == "llvm.global_dtors") && tlg_linkage == Just LinkageAppending -> 
      
      let cnf = case nm of
            "llvm.used" -> TlIntrinsic_llvm_used
            "llvm.compiler.used" -> TlIntrinsic_llvm_compiler_used 
            "llvm.global_ctors" -> TlIntrinsic_llvm_global_ctors
            "llvm.global_dtors" -> TlIntrinsic_llvm_global_dtors
      in Just $ cnf (dcast FLC tlg_dtype) (fromJust tlg_const) tlg_section
    _ -> Nothing
  _ -> Nothing
  
  
unspecializeTlIntrinsics :: TlIntrinsic -> TlGlobal  
unspecializeTlIntrinsics tl = case tl of
  TlIntrinsic_llvm_used ty cnst sec -> mkGlobal "llvm.used" ty cnst sec
  TlIntrinsic_llvm_compiler_used ty cnst sec -> mkGlobal "llvm.compiler.used" ty cnst sec  
  TlIntrinsic_llvm_global_ctors ty cnst sec -> mkGlobal "llvm.global_ctors" ty cnst sec  
  TlIntrinsic_llvm_global_dtors ty cnst sec -> mkGlobal "llvm.global_dtors" ty cnst sec
  where mkGlobal str t c s = TlGlobalDtype { tlg_lhs = GlobalIdAlphaNum str
                                           , tlg_linkage = Just LinkageAppending
                                           , tlg_visibility = Nothing
                                           , tlg_dllstorage = Nothing
                                           , tlg_tls = Nothing
                                           , tlg_addrnaming = NamedAddr
                                           , tlg_addrspace = Nothing
                                           , tlg_externallyInitialized = IsNot ExternallyInitialized
                                           , tlg_globalType = GlobalType "global"
                                           , tlg_dtype = ucast t
                                           , tlg_const = Just c
                                           , tlg_section = s
                                           , tlg_comdat = Nothing
                                           , tlg_alignment = Nothing
                                           }
      
    
    
specializeMinst :: Minst -> Minst    
specializeMinst mi = case mi of
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (GlobalIdAlphaNum "llvm.dbg.declare") [m1,m2] ->  M_llvm_dbg_declare m1 m2
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (GlobalIdAlphaNum "llvm.dbg.value") [m1,m2,m3] -> M_llvm_dbg_value m1 m2 m3
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (GlobalIdAlphaNum "llvm.dbg.func.start") [m1] -> M_llvm_dbg_func_start m1
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (GlobalIdAlphaNum "llvm.dbg.region.end") [m1] -> M_llvm_dbg_region_end m1  
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (GlobalIdAlphaNum "llvm.dbg.stopppoint") [m1,m2,m3] -> M_llvm_dbg_stoppoint m1 m2 m3
  _ -> mi
    
    
unspecializeMinst :: Minst -> Minst
unspecializeMinst mi = case mi of
  M_llvm_dbg_declare m1 m2 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (GlobalIdAlphaNum "llvm.dbg.declare") [m1,m2]
  M_llvm_dbg_value m1 m2 m3 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (GlobalIdAlphaNum "llvm.dbg.value") [m1,m2,m3]
  M_llvm_dbg_func_start m1 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (GlobalIdAlphaNum "llvm.dbg.func.start") [m1]
  M_llvm_dbg_region_end m1 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (GlobalIdAlphaNum "llvm.dbg.region.end") [m1]
  M_llvm_dbg_stoppoint m1 m2 m3 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (GlobalIdAlphaNum "llvm.dbg.stoppoint") [m1,m2,m3]  
  _ -> mi
                            
    
       
specializeUnamedMd :: TlUnamedMd -> TlUnamedMd
specializeUnamedMd x = case x of
  (TlUnamedMd n (MetaKindedConst Mmetadata (McStruct [MetaKindedConst _ (McSimple (C_int "786473")), mc]))) -> TlUnamedMd_DW_file_type n mc
  _ -> x
  
  
unspecializeUnamedMd :: TlUnamedMd -> TlUnamedMd  
unspecializeUnamedMd x = case x of
  TlUnamedMd_DW_file_type n mc -> (TlUnamedMd n (MetaKindedConst Mmetadata 
                                                 (McStruct [MetaKindedConst (Mtype $ ucast i32) (McSimple (C_int "786473")), mc])))
  _ -> x
