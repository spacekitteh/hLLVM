{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RecordWildCards #-}
module Llvm.AsmHirConversion.Specialization where

import qualified Llvm.Asm.Data as A
import Llvm.Hir
import Llvm.ErrorLoc
import Data.Maybe
import Llvm.Hir.DataLayoutMetrics
import Llvm.Hir.Target.Linux_Gnu
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Bits

#define FLC  (FileLoc $(srcLoc))

sLid :: A.LocalId -> Lname
sLid x = case x of
  A.LocalIdNum x -> errorLoc FLC $ show x
  A.LocalIdAlphaNum x -> Lname x
  A.LocalIdDqString x -> Lname x
  
unLid :: Lname -> A.LocalId  
unLid (Lname s) = A.LocalIdDqString s

specializeGlobalId :: A.GlobalId -> Gname
specializeGlobalId x = case x of
  A.GlobalIdNum v -> errorLoc FLC $ show x
  A.GlobalIdAlphaNum v -> Gname v
  A.GlobalIdDqString v -> Gname v

unspecializeGlobalId :: Gname -> A.GlobalId
unspecializeGlobalId x = case x of
  Gname v -> A.GlobalIdDqString v

specializeDollarId :: A.DollarId -> Gname
specializeDollarId x = case x of
  A.DollarIdNum v -> errorLoc FLC $ show x
  A.DollarIdAlphaNum v -> Gname v
  A.DollarIdDqString v -> Gname v


unspecializeDollarId :: Gname -> A.DollarId
unspecializeDollarId x = case x of
  Gname v -> A.DollarIdDqString v


specializePAttr :: ParamAttr -> PAttr
specializePAttr x = case x of
  PaInReg -> PInReg
  PaInAlloca -> PInAlloca
  PaNoAlias -> PNoAlias
  PaNoCapture -> PNoCapture
  PaNest -> PNest
  PaReturned -> PReturned
  PaNonNull -> PNonNull
  PaDereferenceable n -> PDereferenceable n
  PaReadOnly -> PReadOnly
  PaReadNone -> PReadNone
  _ -> errorLoc FLC $ show x


unspecializePAttr :: PAttr -> ParamAttr
unspecializePAttr x = case x of
  PInReg -> PaInReg
  PInAlloca -> PaInAlloca
  PNoAlias -> PaNoAlias
  PNoCapture -> PaNoCapture
  PNest -> PaNest
  PReturned -> PaReturned
  PNonNull -> PaNonNull
  PDereferenceable n -> PaDereferenceable n
  PReadOnly -> PaReadOnly
  PReadNone -> PaReadNone


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


specializeRegisterIntrinsic :: Maybe A.LocalId -> A.CallSite -> Maybe (Maybe A.LocalId, MemLen, [A.ActualParam])
specializeRegisterIntrinsic lhs cs = case (lhs, cs) of
  (Just r, A.CallSiteFun Nothing [] _ (A.FunNameGlobal (GolG (A.GlobalIdAlphaNum nm))) [m] _) 
    | (nm == "llvm.read_register.i32" || nm == "llvm.read_register.i64") -> 
      let ml = case nm of
            "llvm.read_register.i32" -> MemLenI32
            "llvm.read_register.i64" -> MemLenI64
      in Just (Just r, ml, [m])
  (Nothing, A.CallSiteFun Nothing [] _ (A.FunNameGlobal (GolG (A.GlobalIdAlphaNum nm))) [m,v] _) 
    | (nm == "llvm.write_register.i32" || nm == "llvm.write_register.i64") -> 
      let ml = case nm of
            "llvm.write_register.i32" -> MemLenI32
            "llvm.write_register.i64" -> MemLenI64
      in Just (Nothing, ml, [m,v])
  (_, _) -> Nothing

unspecializeRegisterIntrinsic :: Cinst Gname -> Maybe (Gname, Type ScalarB I, [MetaOperand Gname], Maybe Lname)
unspecializeRegisterIntrinsic cinst = case cinst of
  I_llvm_read_register mLen mc r ->
    let (nm, typ) = case mLen of
          MemLenI32 -> (Gname "llvm.read_register.i32", i32)
          MemLenI64 -> (Gname "llvm.read_register.i64", i64)
    in Just (nm, typ, [MetaOperandMeta mc], Just r)
  I_llvm_write_register mLen mc val ->
    let (nm, typ) = case mLen of
          MemLenI32 -> (Gname "llvm.write_register.i32", i32)
          MemLenI64 -> (Gname "llvm.write_register.i64", i64)
    in Just (nm, typ, [MetaOperandMeta mc, MetaOperandData (ucast typ) [] Nothing val], Nothing)
  _ -> Nothing


specializeCallSite :: Maybe Lname -> FunPtr Gname -> CallFunInterface Gname -> Maybe (Cinst Gname)
specializeCallSite lhs fptr csi = case (fptr, cfi_signature csi) of
  (FunId (Gname "llvm.va_start"),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v]) | isNothing lhs -> 
    Just $ I_llvm_va_start v
  (FunId (Gname "llvm.va_end"),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v]) | isNothing lhs -> 
    Just $ I_llvm_va_end v
  (FunId (Gname "llvm.va_copy"),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v1
                         ,FunOperandData t2 [] Nothing v2]) | isNothing lhs -> Just $ I_llvm_va_copy v1 v2
  (FunId (Gname nm), 
   FunSignature Ccc _ 
   [FunOperandData t1 [] Nothing v1 -- dest
   ,FunOperandData t2 [] Nothing v2 -- src or setValue
   ,FunOperandData t3 [] Nothing v3 -- len
   ,FunOperandData t4 [] Nothing v4 -- align
   ,FunOperandData t5 [] Nothing v5 -- volatile
   ]) | isNothing lhs && (nm == "llvm.memcpy.p0i8.p0i8.i32" 
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
  (FunId (Gname "llvm.stacksave"),
   FunSignature Ccc _ []) | isJust lhs -> 
    Just $ I_llvm_stacksave $ fromJust lhs
  (FunId (Gname "llvm.stackrestore"),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v]) | isNothing lhs -> 
    Just $ I_llvm_stackrestore (T (dcast FLC t1) v)
  (FunId (Gname gname),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v]) | ((isJust $ stripPrefix "llvm.ctpop." gname) && isJust lhs) -> 
    Just $ I_llvm_ctpop { suffix = fromJust $ stripPrefix "llvm.ctpop." gname, dv = T t1 v, result = fromJust lhs } 
  (FunId (Gname "llvm.lifetime.start"),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v1
                      ,FunOperandData t2 [] Nothing v2]) | isNothing lhs -> 
    Just $ I_llvm_lifetime_start (T (dcast FLC t1) v1) (T (dcast FLC t2) v2)
  (FunId (Gname "llvm.lifetime.end"),
   FunSignature Ccc _ [FunOperandData t1 [] Nothing v1
                      ,FunOperandData t2 [] Nothing v2]) | isNothing lhs -> 
    Just $ I_llvm_lifetime_end (T (dcast FLC t1) v1) (T (dcast FLC t2) v2)
  _ -> Nothing


unspecializeIntrinsics :: Cinst Gname -> Maybe (Cinst Gname)
unspecializeIntrinsics inst = case inst of
  I_llvm_va_start v -> 
    Just $ I_call_fun (FunId (Gname "llvm.va_start")) 
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid, []) 
                                                         [(MtypeData (ucast $ ptr0 i8), Nothing)] Nothing) [tvToAp (T (ptr0 i8) v)] 
                     , cfi_funAttrs = [] 
                     } Nothing
  I_llvm_va_end v -> 
    Just $ I_call_fun (FunId (Gname "llvm.va_end"))
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid, []) 
                                                         [(MtypeData (ucast $ ptr0 i8), Nothing)] Nothing) [tvToAp (T (ptr0 i8) v)] 
                     , cfi_funAttrs = [] 
                     } Nothing
  I_llvm_va_copy v1 v2 ->
    Just $ I_call_fun (FunId (Gname "llvm.va_copy"))
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid, []) 
                                                         [(MtypeData (ucast $ ptr0 i8), Nothing)
                                                         ,(MtypeData (ucast $ ptr0 i8), Nothing)] Nothing)
                                       [tvToAp (T (ptr0 i8) v1), tvToAp (T (ptr0 i8) v2)] 
                     , cfi_funAttrs = [] 
                     } Nothing
  I_llvm_memcpy memLen tv1 tv2 tv3 tv4 tv5 -> 
    let (nm, ltype) = case memLen of
          MemLenI32 -> ("llvm.memcpy.p0i8.p0i8.i32", i32)
          MemLenI64 -> ("llvm.memcpy.p0i8.p0i8.i64", i64)
    in Just $ I_call_fun (FunId (Gname nm))
       CallFunInterface { cfi_tail = TcNon 
                        , cfi_castType = Nothing
                        , cfi_signature = FunSignature Ccc  (Tfunction (RtypeVoidU Tvoid,[]) 
                                                             [(MtypeData (ucast $ ptr0 i8), Nothing)
                                                             ,(MtypeData (ucast $ ptr0 i8), Nothing)
                                                             ,(MtypeData (ucast ltype), Nothing)
                                                             ,(MtypeData (ucast i32), Nothing)
                                                             ,(MtypeData (ucast i1), Nothing)
                                                             ] Nothing)
                                          [tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5] 
                        , cfi_funAttrs = [] 
                        } Nothing
  I_llvm_memmove memLen tv1 tv2 tv3 tv4 tv5 -> 
    let (nm, ltype) = case memLen of
          MemLenI32 -> ("llvm.memmove.p0i8.p0i8.i32", i32)
          MemLenI64 -> ("llvm.memmove.p0i8.p0i8.i64", i64)
    in Just $ I_call_fun (FunId (Gname nm))
       CallFunInterface { cfi_tail = TcNon 
                        , cfi_castType = Nothing
                        , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid,[]) 
                                                            [(MtypeData (ucast $ ptr0 i8), Nothing)
                                                            ,(MtypeData (ucast $ ptr0 i8), Nothing)
                                                            ,(MtypeData (ucast ltype), Nothing)
                                                            ,(MtypeData (ucast i32), Nothing)
                                                            ,(MtypeData (ucast i1), Nothing)
                                                            ] Nothing)
                                          [tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5] 
                        , cfi_funAttrs = [] 
                        } Nothing
  I_llvm_memset memLen tv1 tv2 tv3 tv4 tv5 -> 
    let (nm, ltype) = case memLen of
          MemLenI32 -> ("llvm.memset.p0i8.i32", i32)
          MemLenI64 -> ("llvm.memset.p0i8.i64", i64)
    in Just $ I_call_fun (FunId (Gname nm))
       CallFunInterface { cfi_tail = TcNon 
                        , cfi_castType = Nothing
                        , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid,[]) 
                                                            [(MtypeData (ucast $ ptr0 i8), Nothing)
                                                            ,(MtypeData (ucast i8), Nothing)
                                                            ,(MtypeData (ucast ltype), Nothing)
                                                            ,(MtypeData (ucast i32), Nothing)
                                                            ,(MtypeData (ucast i1), Nothing)
                                                            ] Nothing)
                                                            [tvToAp tv1, tvToAp tv2, tvToAp tv3, tvToAp tv4, tvToAp tv5] 
                        , cfi_funAttrs = []
                        } Nothing
  I_llvm_stacksave v -> 
    Just $ I_call_fun (FunId (Gname "llvm.stacksave")) 
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeScalarP $ ptr0 i8,[]) [] Nothing) [] 
                     , cfi_funAttrs = [] 
                     } (Just v)
  I_llvm_stackrestore tv -> 
    Just $ I_call_fun (FunId (Gname "llvm.stackrestore")) 
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid,[]) 
                                                         [(MtypeData (ucast $ ptr0 i8),Nothing)] Nothing) [tvToAp tv] 
                     , cfi_funAttrs = [] 
                     } Nothing
  I_llvm_ctpop { suffix = s, dv = tv@(T t v), result = r } ->
    Just $ I_call_fun (FunId (Gname $ "llvm.ctpop." ++ s)) 
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (ucast t,[]) 
                                                         [(MtypeData t,Nothing)] Nothing) [tvToAp tv] 
                     , cfi_funAttrs = []
                     } (Just r)
  I_llvm_lifetime_start { objsize = tv1@(T t1 _), pointer = tv2@(T t2 _) } ->
    Just $ I_call_fun (FunId (Gname $ "llvm.lifetime.start")) 
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid,[]) 
                                                         [(MtypeData $ ucast t1,Nothing)
                                                         ,(MtypeData $ ucast t2,Nothing)] Nothing) [tvToAp tv1, tvToAp tv2] 
                     , cfi_funAttrs = []
                     } Nothing
  I_llvm_lifetime_end { objsize = tv1@(T t1 _), pointer = tv2@(T t2 _) } ->
    Just $ I_call_fun (FunId (Gname $ "llvm.lifetime.end")) 
    CallFunInterface { cfi_tail = TcNon 
                     , cfi_castType = Nothing
                     , cfi_signature = FunSignature Ccc (Tfunction (RtypeVoidU Tvoid,[]) 
                                                         [(MtypeData $ ucast t1,Nothing)
                                                         ,(MtypeData $ ucast t2,Nothing)] Nothing) [tvToAp tv1, tvToAp tv2] 
                     , cfi_funAttrs = []
                     } Nothing
  _ -> Nothing
    
tvToAp :: Ucast t Dtype => T t (Value g) -> FunOperand (Value g)
tvToAp (T t v) = FunOperandData (ucast t) [] Nothing v
                 
specializeTlGlobal :: TlGlobal Gname -> Maybe (TlIntrinsic Gname)
specializeTlGlobal tl = case tl of
  TlGlobalDtype {..} -> case tlg_lhs of
    Gname nm | (nm == "llvm.used" 
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
  
  
unspecializeTlIntrinsics :: TlIntrinsic Gname -> TlGlobal Gname
unspecializeTlIntrinsics tl = case tl of
  TlIntrinsic_llvm_used ty cnst sec -> mkGlobal "llvm.used" ty cnst sec
  TlIntrinsic_llvm_compiler_used ty cnst sec -> mkGlobal "llvm.compiler.used" ty cnst sec  
  TlIntrinsic_llvm_global_ctors ty cnst sec -> mkGlobal "llvm.global_ctors" ty cnst sec  
  TlIntrinsic_llvm_global_dtors ty cnst sec -> mkGlobal "llvm.global_dtors" ty cnst sec
  where mkGlobal str t c s = TlGlobalDtype { tlg_lhs = Gname str
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
      
    
    
specializeMinst :: Minst Gname -> Minst Gname
specializeMinst mi = case mi of
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (Gname "llvm.dbg.declare") [m1,m2] ->  M_llvm_dbg_declare m1 m2
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (Gname "llvm.dbg.value") [m1,m2,m3] -> M_llvm_dbg_value m1 m2 m3
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (Gname "llvm.dbg.func.start") [m1] -> M_llvm_dbg_func_start m1
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (Gname "llvm.dbg.region.end") [m1] -> M_llvm_dbg_region_end m1  
  Minst (CallSiteTypeRet (RtypeVoidU Tvoid)) (Gname "llvm.dbg.stopppoint") [m1,m2,m3] -> M_llvm_dbg_stoppoint m1 m2 m3
  _ -> mi
    
    
unspecializeMinst :: Minst Gname -> Minst Gname
unspecializeMinst mi = case mi of
  M_llvm_dbg_declare m1 m2 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (Gname "llvm.dbg.declare") [m1,m2]
  M_llvm_dbg_value m1 m2 m3 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (Gname "llvm.dbg.value") [m1,m2,m3]
  M_llvm_dbg_func_start m1 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (Gname "llvm.dbg.func.start") [m1]
  M_llvm_dbg_region_end m1 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (Gname "llvm.dbg.region.end") [m1]
  M_llvm_dbg_stoppoint m1 m2 m3 -> Minst (CallSiteTypeRet $ RtypeVoidU Tvoid) (Gname "llvm.dbg.stoppoint") [m1,m2,m3]  
  _ -> mi


specializeUnamedMd :: TlUnamedMd Gname -> TlUnamedMd Gname
specializeUnamedMd x = case x of
  TlUnamedMd n mc@(MetaKindedConst MKmetadata (McStruct ((MetaKindedConst _ (McSimple (C_int num))):t))) -> 
    let intNum = read num
    in 
    case M.lookup intNum intToTagMap of 
      Just tag -> TlUnamedMd_Tagged n tag t
      Nothing -> TlUnamedMd n mc
  _ -> x

unspecializeUnamedMd :: TlUnamedMd Gname -> TlUnamedMd Gname
unspecializeUnamedMd x = case x of
  TlUnamedMd_Tagged n tag lst -> 
    let h = case M.lookup tag tagToIntMap of
          Just num -> MetaKindedConst (MKtype $ ucast i32) (McSimple (C_int $ show num))
          Nothing -> errorLoc FLC $ show tag
    in TlUnamedMd n (MetaKindedConst MKmetadata (McStruct (h:lst)))
  _ -> x


tagToIntMap :: M.Map DW_TAG Word32
tagToIntMap = M.fromList enumTag

intToTagMap :: M.Map Word32 DW_TAG
intToTagMap = 
  M.fromList (fmap (\(x,y) -> (y,x)) enumTag)

           
enumTag :: [(DW_TAG, Word32)]
enumTag = fmap (\(k,v)-> (k, 786432 + v))
          [(DW_TAG_array_type,0x01),
           (DW_TAG_class_type,0x02),
           (DW_TAG_entry_point,0x03),
           (DW_TAG_enumeration_type,0x04),
           (DW_TAG_formal_parameter,0x05),
           (DW_TAG_imported_declaration,0x08),
           (DW_TAG_label,0x0a),
           (DW_TAG_lexical_block,0x0b),
           (DW_TAG_member,0x0d),
           (DW_TAG_pointer_type,0x0f),
           (DW_TAG_reference_type,0x10),
           (DW_TAG_compile_unit,0x11),
           (DW_TAG_string_type,0x12),
           (DW_TAG_structure_type,0x13),
           (DW_TAG_subroutine_type,0x15),
           (DW_TAG_typedef,0x16),
           (DW_TAG_union_type,0x17),
           (DW_TAG_unspecified_parameters,0x18),
           (DW_TAG_variant,0x19),
           (DW_TAG_common_block,0x1a),
           (DW_TAG_common_inclusion,0x1b),
           (DW_TAG_inheritance,0x1c),
           (DW_TAG_inlined_subroutine,0x1d),
           (DW_TAG_module,0x1e),
           (DW_TAG_ptr_to_member_type,0x1f),
           (DW_TAG_set_type,0x20),
           (DW_TAG_subrange_type,0x21),
           (DW_TAG_with_stmt,0x22),
           (DW_TAG_access_declaration,0x23),
           (DW_TAG_base_type,0x24),
           (DW_TAG_catch_block,0x25),
           (DW_TAG_const_type,0x26),
           (DW_TAG_constant,0x27),
           (DW_TAG_enumerator,0x28),
           (DW_TAG_file_type,0x29),
           (DW_TAG_friend,0x2a),
           (DW_TAG_namelist,0x2b),
           (DW_TAG_namelist_item,0x2c),
           (DW_TAG_packed_type,0x2d),
           (DW_TAG_subprogram,0x2e),
           (DW_TAG_template_type_parameter,0x2f),
           (DW_TAG_template_value_parameter,0x30),
           (DW_TAG_thrown_type,0x31),
           (DW_TAG_try_block,0x32),
           (DW_TAG_variant_part,0x33),
           (DW_TAG_variable,0x34),
           (DW_TAG_volatile_type,0x35),
           (DW_TAG_dwarf_procedure,0x36),
           (DW_TAG_restrict_type,0x37),
           (DW_TAG_interface_type,0x38),
           (DW_TAG_namespace,0x39),
           (DW_TAG_imported_module,0x3a),
           (DW_TAG_unspecified_type,0x3b),
           (DW_TAG_partial_unit,0x3c),
           (DW_TAG_imported_unit,0x3d),
           (DW_TAG_condition,0x3f),
           (DW_TAG_shared_type,0x40),
           (DW_TAG_type_unit,0x41),
           (DW_TAG_rvalue_reference_type,0x42),
           (DW_TAG_template_alias,0x43),
           
           (DW_TAG_coarray_type,0x44),
           (DW_TAG_generic_subrange,0x45),
           (DW_TAG_dynamic_type,0x46),
           
           (DW_TAG_auto_variable, 0x100),
           (DW_TAG_arg_variable, 0x101),
           (DW_TAG_user_base, 0x1000),
           
           (DW_TAG_MIPS_loop,0x4081),
           (DW_TAG_format_label,0x4101),
           (DW_TAG_function_template,0x4102),
           (DW_TAG_class_template,0x4103),
           (DW_TAG_GNU_template_template_param,0x4106),
           (DW_TAG_GNU_template_parameter_pack,0x4107),
           (DW_TAG_GNU_formal_parameter_pack,0x4108),
           (DW_TAG_lo_user,0x4080),
           (DW_TAG_APPLE_property,0x4200),
           (DW_TAG_hi_user,0xffff)
          ]


specializeDeclareIntrinsic :: TlDeclare Gname -> Maybe TlDeclareIntrinsic
specializeDeclareIntrinsic (TlDeclare fd) = case fd of
  FunctionDeclareData { fd_fun_name = fname 
                      , fd_signature = sig
                      , fd_fun_attrs = attrs
                      } -> 
    if isIntrinsic fname then
      Just $ TlDeclareIntrinsic $ IntrinsicDeclareData sig fname attrs
    else
      Nothing
  FunctionDeclareMeta {..} -> Nothing
  where isIntrinsic (Gname g) = isJust $ stripPrefix "llvm." g
          

unspecializeDeclareIntrinsic :: TlDeclareIntrinsic -> TlDeclare Gname
unspecializeDeclareIntrinsic (TlDeclareIntrinsic fd) = case fd of
  IntrinsicDeclareData { id_fun_name = fname 
                       , id_signature = sig
                       , id_fun_attrs = attrs
                       } -> 
    TlDeclare $ FunctionDeclareData { fd_linkage = Nothing
                                    , fd_visibility = Nothing
                                    , fd_dllstorage = Nothing
                                    , fd_signature = sig 
                                    , fd_fun_name = fname 
                                    , fd_addr_naming = Nothing
                                    , fd_fun_attrs = attrs 
                                    , fd_section = Nothing
                                    , fd_alignment = Nothing
                                    , fd_gc = Nothing
                                    , fd_prefix = Nothing
                                    , fd_prologue = Nothing
                                    , fd_comdat = Nothing
                                    }
  
          