{-# LANGUAGE CPP, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Llvm.Query.Type where

import Llvm.ErrorLoc
#define FLC   (FileLoc $(srcLoc))

import Llvm.Hir.Data.Type
import Llvm.Hir.Composer (i1,i8,ptr0)
import qualified Data.Map as M
import qualified Data.Bits as B 
import Llvm.Hir.Data.Inst
import Llvm.Hir.Cast
import Llvm.Query.HirCxt
import Llvm.Query.Conversion
import Debug.Trace
import Data.Word (Word8, Word64)
import Data.DoubleWord (Word96)
import Data.Bits(Bits(..), (.&.), (.|.), shiftR)
import Llvm.Hir.DataLayoutMetrics 


selectAlign :: AlignType -> AlignMetrics -> AlignInBit
selectAlign at AlignMetrics { abi = abix, pref = prefx } = case at of
  AlignAbi -> abix
  AlignPref -> maybe abix id prefx


normalizeAddrSpace :: Maybe AddrSpace -> (LayoutAddrSpace, LayoutAddrSpace)
normalizeAddrSpace x = case x of
  Just n | n == 0 -> (LayoutAddrSpaceUnspecified, LayoutAddrSpace n)
  Just n -> (LayoutAddrSpace n, LayoutAddrSpace n)  
  Nothing -> (LayoutAddrSpaceUnspecified, LayoutAddrSpace 0)


eightBits :: Word8
eightBits = 8

data SizeInByte = SizeInByte Word64 deriving (Eq, Ord, Show)
data OffsetInByte = OffsetInByte Word64 deriving (Eq, Ord, Show)

fromSizeInBit :: SizeInBit -> SizeInByte
fromSizeInBit (SizeInBit n) = SizeInByte $ fromIntegral (n `div` (fromIntegral eightBits))

toSizeInBit :: SizeInByte -> SizeInBit
toSizeInBit (SizeInByte n) = SizeInBit ((fromIntegral n) * (fromIntegral eightBits))

fromAlignInBit :: AlignInBit -> AlignInByte
fromAlignInBit (AlignInBit n) = AlignInByte $ fromIntegral (n `div` (fromIntegral eightBits))

fromAlignInByte :: AlignInByte -> AlignInBit
fromAlignInByte (AlignInByte n) = AlignInBit $ fromIntegral (n * (fromIntegral eightBits))

getTypeAlignment :: DataLayoutMetrics dlm => dlm -> TypeEnv -> Dtype -> AlignType -> AlignInByte
getTypeAlignment dlm te@TypeEnv{..} t at = case getTypeDef te t of
  DtypeScalarI st -> fromAlignInBit $ getTpAlignment st
  DtypeScalarF st -> fromAlignInBit $ getTpAlignment st  
  DtypeRecordD st -> case st of
    Tarray i et -> getTypeAlignment dlm te et at
    Tstruct p tys -> case (p, at) of
      (Packed, AlignAbi)  -> AlignInByte 1
      _ -> let aa = selectAlign at $ alignOfAggregate dlm
               sl = getStructLayout dlm te (p,tys)
           in (max (fromAlignInBit aa) (structAlignment sl))
  DtypeScalarP (Tpointer t a) -> fromAlignInBit $ selectAlign at (alignOfPtr dlm a)
  DtypeVectorI x -> fromAlignInBit $ getVectorAlignment x
  DtypeVectorF x -> fromAlignInBit $ getVectorAlignment x
  DtypeVectorP x -> fromAlignInBit $ getVectorAlignment x  
  _ -> errorLoc FLC $ show t
  where
    getVectorAlignment :: Type VectorB x -> AlignInBit
    getVectorAlignment vt = case getVXAlignMetrics dlm (getTypeSizeInBits dlm te (dcast FLC vt)) of
      Just am -> selectAlign at am
      Nothing -> case vt of
        TvectorI n e -> bestMatchVectorAlignment (fromIntegral n) e
        TvectorF n e -> bestMatchVectorAlignment (fromIntegral n) e
        TvectorP n e -> bestMatchVectorAlignment (fromIntegral n) e
        _ -> errorLoc FLC $ show vt
    getTpAlignment :: Type ScalarB x -> AlignInBit
    getTpAlignment tp = case tp of
      TpI n -> selectAlign at $ getIXAlignMetrics dlm $ bestMatchSizeInBit dlm (fromIntegral n) 
      TpF n -> selectAlign at (getFXAlignMetrics dlm (SizeInBit $ fromIntegral n)) 
      TpV n -> errorLoc FLC $ show tp -- selectAlign at (getVXAlignMetrics dlm (SizeInBit $ fromIntegral n))
      TpHalf -> selectAlign at $ getFXAlignMetrics dlm (SizeInBit 16)
      TpFloat -> selectAlign at $ getFXAlignMetrics dlm (SizeInBit 32)
      TpDouble -> selectAlign at $ getFXAlignMetrics dlm (SizeInBit 64)
      TpFp128 -> selectAlign at $ getFXAlignMetrics dlm (SizeInBit 128)
      TpX86Fp80 -> selectAlign at $ getFXAlignMetrics dlm (SizeInBit 80)
      TpPpcFp128 -> selectAlign at $ getFXAlignMetrics dlm (SizeInBit 128)
      TpX86Mmx -> errorLoc FLC $ show tp
    bestMatchVectorAlignment :: Word96 -> Type ScalarB x -> AlignInBit
    bestMatchVectorAlignment n e = 
      let (SizeInByte s) = getTypeAllocSize dlm te (dcast FLC e)
          align = (fromIntegral s) * n
      in if align .&. (align -1) == 0 
         then fromAlignInByte $ AlignInByte (fromIntegral align)
         else fromAlignInByte $ AlignInByte (fromIntegral (nextPowerOf2 align))


nextPowerOf2 :: (Num a, Bits a) => a -> a
nextPowerOf2 a = let a0 = foldl (\p x -> p .|. (p `shiftR` x)) a [1, 2, 4, 8, 16, 32] 
                 in a0 + 1


getTypeSizeInBits :: DataLayoutMetrics dlm => dlm -> TypeEnv -> Dtype -> SizeInBit
getTypeSizeInBits dlm te@TypeEnv{..} dt = case getTypeDef te dt of
  DtypeRecordD t -> case t of
    Tarray i et -> let SizeInBit n = getTypeAllocSizeInBits dlm te et
                   in SizeInBit ((fromIntegral i) * n)
    Tstruct pk ts -> let sl = getStructLayout dlm te (pk, ts)
                     in toSizeInBit $ structSize sl
    TnameRecordD n -> case getTypeByTname n typedefs of
      Just d -> getTypeSizeInBits dlm te d
      Nothing -> errorLoc FLC ("undefined " ++ show n)
    _ -> errorLoc FLC (show t)
  DtypeScalarP (Tpointer t a) -> sizeOfPtr dlm a 
  DtypeScalarI t -> case t of
    TpI n -> SizeInBit $ fromIntegral n
    TpV n -> SizeInBit $ fromIntegral n
  DtypeScalarF t -> case t of  
    TpF n -> SizeInBit $ fromIntegral n
    TpHalf -> SizeInBit 16
    TpFloat -> SizeInBit 32
    TpDouble -> SizeInBit 64
    TpFp128 -> SizeInBit 128
    TpX86Fp80 -> SizeInBit 80
    TpPpcFp128 -> SizeInBit 128
  DtypeVectorI t -> case t of  
    TvectorI n et -> let (SizeInBit s) = getTypeSizeInBits dlm te (ucast et)
                     in (SizeInBit (s * (fromIntegral n)))
  DtypeVectorF t -> case t of  
    TvectorF n et -> let (SizeInBit s) = getTypeSizeInBits dlm te (ucast et)
                     in (SizeInBit (s * (fromIntegral n)))
  DtypeVectorP t -> case t of  
    TvectorP n et -> let (SizeInBit s) = getTypeSizeInBits dlm te (ucast et)
                     in (SizeInBit (s * (fromIntegral n)))
  _ -> errorLoc FLC (show dt)
      

getTypeStoreSize :: DataLayoutMetrics dlm => dlm -> TypeEnv ->  Dtype -> SizeInByte
getTypeStoreSize dlm te ty = let (SizeInBit tyBits) = getTypeSizeInBits dlm te ty
                             in fromSizeInBit (SizeInBit (tyBits + 7))

getTypeAllocSize :: DataLayoutMetrics dlm => dlm -> TypeEnv -> Dtype -> SizeInByte
getTypeAllocSize dlm te ty = let SizeInByte tys = getTypeStoreSize dlm te ty
                             in SizeInByte $ roundUpAlignment tys (getTypeAlignment dlm te ty AlignAbi)

getTypeAllocSizeInBits :: DataLayoutMetrics dlm => dlm -> TypeEnv -> Dtype -> SizeInBit
getTypeAllocSizeInBits dlm te ty = toSizeInBit (getTypeAllocSize dlm te ty)

data StructLayout = StructLayout { structSize :: SizeInByte
                                 , structAlignment :: AlignInByte
                                 , numElements :: Integer
                                 , memberOffsets :: [OffsetInByte]
                                 } deriving (Eq, Ord, Show)

roundUpAlignment :: Word64 -> AlignInByte -> Word64
roundUpAlignment val (AlignInByte align) = 
  (val + ((fromIntegral align) -1)) B..&. (B.complement ((fromIntegral align) - 1))

nextDataFieldOffset :: SizeInByte -> AlignInByte -> OffsetInByte
nextDataFieldOffset (SizeInByte curSizeByte) tyAlign@(AlignInByte tyAlignByte) = 
  if curSizeByte B..&. ((fromIntegral tyAlignByte) -1) /= 0 then
    OffsetInByte $ roundUpAlignment curSizeByte tyAlign
  else 
    OffsetInByte curSizeByte

getStructLayout :: DataLayoutMetrics dlm => dlm -> TypeEnv -> (Packing, [Dtype]) -> StructLayout
getStructLayout dlm te@TypeEnv{..} (pk, tys) = 
  let (totalSize@(SizeInByte totalSizeByte), offsets, alignment@(AlignInByte alignmentByte)) =
        foldl (\(curSize@(SizeInByte curSizeByte), offsets, AlignInByte structAlignment0) ty ->
                let tyAlign@(AlignInByte tyAlignByte) = case pk of
                      Packed -> AlignInByte 1
                      Unpacked -> getTypeAlignment dlm te ty AlignAbi
                    (OffsetInByte nextOffsetByte) = nextDataFieldOffset curSize tyAlign
                    (SizeInByte tySize) = getTypeAllocSize dlm te ty
                in (SizeInByte $ nextOffsetByte + tySize, (OffsetInByte nextOffsetByte):offsets
                   , AlignInByte $ max tyAlignByte structAlignment0)
              ) (SizeInByte 0, [], AlignInByte 1) tys
  in let OffsetInByte finalOffset = nextDataFieldOffset totalSize alignment
     in StructLayout { structSize = SizeInByte finalOffset
                     , structAlignment = alignment
                     , numElements = toInteger $ length tys
                     , memberOffsets = reverse offsets
                     }

getScalarTypeSizeInBits :: TypeEnv -> ScalarType -> SizeInBit
getScalarTypeSizeInBits te x = 
  case x of
    ScalarTypeI e -> case e of
      TpI n -> SizeInBit $ fromIntegral n
      TpV n -> SizeInBit $ fromIntegral n
      TpX86Mmx -> SizeInBit 64
      TnameScalarI _ -> let td = getTypeDef te (ucast x)
                        in getScalarTypeSizeInBits te (dcast FLC td)
      TquoteNameScalarI _ -> let td = getTypeDef te (ucast x)
                             in getScalarTypeSizeInBits te (dcast FLC td)
      TnoScalarI _ -> let td = getTypeDef te (ucast x)
                      in getScalarTypeSizeInBits te (dcast FLC td)
    ScalarTypeF e -> case e of
      TpF n -> SizeInBit $ fromIntegral n
      TpHalf -> SizeInBit 16
      TpFloat -> SizeInBit 32
      TpDouble -> SizeInBit 64
      TpFp128 -> SizeInBit 128
      TpX86Fp80 -> SizeInBit 80
      TpPpcFp128 -> SizeInBit 128
      TnameScalarF _ -> let td = getTypeDef te (ucast x)
                        in getScalarTypeSizeInBits te (dcast FLC td)
      TquoteNameScalarF _ -> let td = getTypeDef te (ucast x)
                             in getScalarTypeSizeInBits te (dcast FLC td)
      TnoScalarF _ -> let td = getTypeDef te (ucast x)
                      in getScalarTypeSizeInBits te (dcast FLC td)
    ScalarTypeP _ -> SizeInBit 32



getGetElemtPtrIndexedType :: (Eq g, Show g) => TypeEnv -> Dtype -> [T (Type ScalarB I) (Value g)] -> Dtype
getGetElemtPtrIndexedType te x is = case is of 
  [] -> x
  hd:tl -> case x of
    DtypeScalarP (Tpointer et _) -> if tl == [] then dcast FLC et
                                    else getGetElemtPtrIndexedType te (dcast FLC et) tl
    DtypeScalarP (TnameScalarP _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is
    DtypeRecordD (TnameRecordD _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is
    DtypeRecordD (TquoteNameRecordD _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is    
    DtypeRecordD (TnoRecordD _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is
    _ -> let ct = getTypeAtIndex te x (castTypedValueToTypedConst hd)
         in if tl == [] then ct
            else getGetElemtPtrIndexedType te ct tl

getTypeAtIndex :: (Show t, Show g) => TypeEnv -> Dtype -> T t (Const g) -> Dtype
getTypeAtIndex _ x@(DtypeScalarP (Tpointer _ _)) _ = error $ "does not expect a pointer type: " ++ show x
getTypeAtIndex te t idx = 
  case (getTypeDef te t) of
    DtypeRecordD (Tstruct _ ts) -> let (ii::Word32) = fromIntegral $ getUniqueInteger idx
                                   in if ii < fromIntegral (length ts) then ts !! (fromIntegral ii)
                                      else error ("Invalid structure index! " ++ show ii)
    DtypeRecordD (Tarray n et) -> et 
    DtypeVectorI (TvectorI n et) -> ucast et
    DtypeVectorF (TvectorF n et) -> ucast et 
    DtypeVectorP (TvectorP n et) -> ucast et
    x -> errorLoc FLC $ "Invalid indexing of " ++ show x ++ ", idx: " ++ show idx

getTypeAtIndices :: (Show t, Show g) => TypeEnv -> Dtype -> [T t (Const g)] -> Dtype
getTypeAtIndices te t indices = case indices of
  [] -> t
  h:tl -> getTypeAtIndices te (getTypeAtIndex te t h) tl
  
getTypeByTname :: String -> M.Map LocalId Dtype -> Maybe Dtype
getTypeByTname tn mp = M.lookup (LocalIdAlphaNum tn) mp

getTypeByTquoteName :: String -> M.Map LocalId Dtype -> Maybe Dtype
getTypeByTquoteName tn mp = M.lookup (LocalIdDqString tn) mp

getTypeByTno :: Word32 -> M.Map LocalId Dtype -> Maybe Dtype
getTypeByTno n mp = M.lookup (LocalIdNum n) mp


getTypeDef :: TypeEnv -> Dtype -> Dtype
getTypeDef TypeEnv{..} t = case t of
  DtypeRecordD (TnameRecordD n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs)
  DtypeRecordD (TquoteNameRecordD n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeRecordD (TnoRecordD n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeRecordD _ -> t
  DtypeScalarI (TnameScalarI n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeScalarI (TquoteNameScalarI n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeScalarI (TnoScalarI n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeScalarI _ -> t
  DtypeScalarP (TnameScalarP n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeScalarP (TquoteNameScalarP n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)   
  DtypeScalarP (TnoScalarP n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeScalarP _ -> t   
  DtypeScalarF (TnameScalarF n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeScalarF (TquoteNameScalarF n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeScalarF (TnoScalarF n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeScalarF _ -> t
  DtypeVectorI (TnameVectorI n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeVectorI (TquoteNameVectorI n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeVectorI (TnoVectorI n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeVectorI _ -> t  
  DtypeVectorP (TnameVectorP n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeVectorP (TquoteNameVectorP n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeVectorP (TnoVectorP n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeVectorP _ -> t  
  DtypeVectorF (TnameVectorF n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeVectorF (TquoteNameVectorF n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeVectorF (TnoVectorF n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeVectorF _ -> t  
  DtypeFirstClassD (Tfirst_class_name n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTname n typedefs)
  DtypeFirstClassD (Tfirst_class_quoteName n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeFirstClassD (Tfirst_class_no n) -> maybe (errorLoc FLC $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeFirstClassD _ -> t

getElementType :: TypeEnv -> Dtype -> Dtype
getElementType te t = case getTypeDef te t of
  DtypeRecordD (Tarray _ t1) -> t1
  DtypeScalarP (Tpointer t1 _) -> dcast FLC t1
  _ -> errorLoc FLC $ (show t) ++ " has no element type"

getPointedType :: TypeEnv -> Type ScalarB P -> Dtype
getPointedType te t = case getTypeDef te (ucast t) of
  DtypeScalarP (Tpointer t1 _) -> dcast FLC t1
  _ -> errorLoc FLC $ show t ++ " has no element type"


getConstArray :: Dtype -> [T Dtype (Const g)] -> T Dtype (Const g)
getConstArray t@(DtypeRecordD (Tarray n el)) [] = T t (C_array $ fmap (\x -> TypedConst $ T el C_zeroinitializer) [1..n])
getConstArray t@(DtypeRecordD (Tarray n el)) l = if or $ fmap (\(T vt _) -> vt /= el) l then error "type mismatch"
                                                 else T t (C_array (fmap TypedConst l))
getConstArray t _ = error "type mismatch"


getTypedConst :: TypeEnv -> (Type ScalarB x -> Const g) -> Dtype -> T Dtype (Const g)
getTypedConst te f t = case (getTypeDef te t) of
  (DtypeRecordD (Tarray n et)) -> let ev = getTypedConst te f et
                                  in T t (C_arrayN n $ TypedConst ev)
  (DtypeRecordD (Tstruct pk ts)) -> let evs = fmap (getTypedConst te f) ts
                                    in T t (C_struct pk (fmap TypedConst evs))

getNullValue :: Type ScalarB x -> Const g
getNullValue t = case t of
  _ -> C_zeroinitializer

getUndefValue :: Type ScalarB x -> Const g
getUndefValue t = case t of
  _ -> C_undef

getGetElementPtr :: T (Type ScalarB P) (Const g) -> [T (Type ScalarB I) (Const g)] -> IsOrIsNot InBounds -> GetElementPtr ScalarB (Const g) (Const g)
getGetElementPtr (T t cv) indices isB = GetElementPtr isB (T t cv) indices


class TypeOf a t | a -> t where  
  typeof :: TypeEnv -> a -> Maybe t
  
elementTypeOfVector :: TypeEnv -> Type VectorB x -> Type ScalarB x
elementTypeOfVector te vt = case vt of
  TvectorI _ e -> e
  TvectorF _ e -> e
  TvectorP _ e -> e
  

addrSpaceOf :: TypeEnv -> Dtype -> AddrSpace  
addrSpaceOf te (DtypeScalarP t) = addrSpaceOf_ te t
  where
    addrSpaceOf_ :: TypeEnv -> Type ScalarB P -> AddrSpace  
    addrSpaceOf_ te (Tpointer _ as) = as
    addrSpaceOf_ te n@(TnameScalarP _) = addrSpaceOf te (getTypeDef te (ucast n))


instance TypeOf (T (Type ScalarB I) x) Dtype where
  typeof te (T t _) = Just (ucast t)

instance TypeOf (T (Type ScalarB F) x) Dtype where
  typeof te (T t _) = Just (ucast t)

instance TypeOf (T (Type ScalarB P) x) Dtype where
  typeof te (T t _) = Just (ucast t)

instance TypeOf (T Dtype x) Dtype where
  typeof te (T t _) = Just t

instance TypeOf (FunSignature (Value g)) Dtype where
  typeof te (FunSignature { fs_type = typ}) = typeof te typ
    
instance (Eq g, Show g) => TypeOf (Cinst g) Dtype where
  typeof te x = case x of
    I_alloca{..} -> Just $ ucast $ Tpointer (ucast dtype) 0
    I_load{..} -> let (T (Tpointer et _) _) = pointer
                  in Just $ dcast FLC et
    I_loadatomic{..} -> let (T (Tpointer et _) _) = pointer
                        in Just $ dcast FLC et
    I_store{..} -> Nothing
    I_storeatomic{..} -> Nothing
    I_fence{..} -> Nothing
    
    I_cmpxchg_I{..} -> let (T t _) = cmpi
                       in Just $ ucast t
    
    I_cmpxchg_F{..} -> let (T t _) = cmpf
                       in Just $ ucast t
    
    I_cmpxchg_P{..} -> let (T t _) = cmpp
                       in Just $ ucast t
    
    I_atomicrmw{..} -> let (T t _) = val
                       in Just $ ucast t

    I_call_fun{..} -> typeof te (cfi_signature call_fun_interface)
    
    I_call_asm{..} -> typeof te (cai_type call_asm_interface)
    
    I_extractelement_I{..} -> let (T vt _) = vectorI
                              in Just $ ucast $ elementTypeOfVector te vt
    I_extractelement_F{..} -> let (T vt _) = vectorF
                              in Just $ ucast $ elementTypeOfVector te vt
    I_extractelement_P{..} -> let (T vt _) = vectorP
                              in Just $ ucast $ elementTypeOfVector te vt                            
    I_insertelement_I{..} -> let (T vt _) = vectorI
                             in Just $ ucast vt
    I_insertelement_F{..} -> let (T vt _) = vectorF
                             in Just $ ucast vt
    I_insertelement_P{..} -> let (T vt _) = vectorP
                             in Just $ ucast vt
    I_shufflevector_I{..} -> let (T vt _) = vector1I
                             in Just $ ucast vt
    I_shufflevector_F{..} -> let (T vt _) = vector1F
                             in Just $ ucast vt
    I_shufflevector_P{..} -> let (T vt _) = vector1P
                             in Just $ ucast vt
                                
    I_extractvalue{..} -> let (T t _) = record
                          in Just $ getTypeAtIndices te (ucast t) ((u32sToTcs windices)::[T (Type ScalarB I) (Const g)])
                             
    I_insertvalue{..} -> let (T t _) = record
                         in Just $ ucast t
                             
    I_landingpad{..} -> Just resultType
    
    I_getelementptr{..} -> 
      let (T bt _) = pointer
      in if indices == [] 
         then Just (ucast bt)
         else let et = getGetElemtPtrIndexedType te (ucast bt) indices
              in Just (ucast $ Tpointer (ucast et) (addrSpaceOf te (ucast bt)))
                 
    I_getelementptr_V{..} -> errorLoc FLC $ show x 
    
    I_icmp{..} -> Just $ ucast (TpI 1) 
    I_icmp_V{..} -> Just $ ucast (TpI 1)
    I_fcmp{..} -> Just $ ucast (TpI 1) 
    I_fcmp_V{..} -> Just $ ucast (TpI 1)
                 
    I_add{..} -> Just $ ucast typeI
    I_sub{..} -> Just $ ucast typeI
    I_mul{..} -> Just $ ucast typeI
    I_udiv{..} -> Just $ ucast typeI
    I_sdiv{..} -> Just $ ucast typeI
    I_urem{..} -> Just $ ucast typeI
    I_srem{..} -> Just $ ucast typeI
    I_shl{..} -> Just $ ucast typeI
    I_lshr{..} -> Just $ ucast typeI
    I_ashr{..} -> Just $ ucast typeI
    I_and{..} -> Just $ ucast typeI
    I_or{..} -> Just $ ucast typeI
    I_xor{..} -> Just $ ucast typeI
    
    I_add_V{..} -> Just $ ucast typeVI
    I_sub_V{..} -> Just $ ucast typeVI
    I_mul_V{..} -> Just $ ucast typeVI
    I_udiv_V{..} -> Just $ ucast typeVI
    I_sdiv_V{..} -> Just $ ucast typeVI
    I_urem_V{..} -> Just $ ucast typeVI
    I_srem_V{..} -> Just $ ucast typeVI
    I_shl_V{..} -> Just $ ucast typeVI
    I_lshr_V{..} -> Just $ ucast typeVI
    I_ashr_V{..} -> Just $ ucast typeVI
    I_and_V{..} -> Just $ ucast typeVI
    I_or_V{..} -> Just $ ucast typeVI
    I_xor_V{..} -> Just $ ucast typeVI
                 
    I_fadd{..} -> Just $ ucast typeF
    I_fsub{..} -> Just $ ucast typeF
    I_fmul{..} -> Just $ ucast typeF
    I_fdiv{..} -> Just $ ucast typeF
    I_frem{..} -> Just $ ucast typeF
    
    I_fadd_V{..} -> Just $ ucast typeVF
    I_fsub_V{..} -> Just $ ucast typeVF
    I_fmul_V{..} -> Just $ ucast typeVF
    I_fdiv_V{..} -> Just $ ucast typeVF
    I_frem_V{..} -> Just $ ucast typeVF
    
    I_trunc{..} -> Just $ ucast toI
    I_zext{..} -> Just $ ucast toI
    I_sext{..} -> Just $ ucast toI
    I_fptrunc{..} -> Just $ ucast toF
    I_fpext{..} -> Just $ ucast toF    
    I_fptoui{..} -> Just $ ucast toI
    I_fptosi{..} -> Just $ ucast toI    
    I_uitofp{..} -> Just $ ucast toF
    I_sitofp{..} -> Just $ ucast toF
    I_ptrtoint{..} -> Just $ ucast toI
    I_inttoptr{..} -> Just $ ucast toP    
    I_addrspacecast{..} -> Just $ ucast toP
    I_bitcast{..} -> Just $ ucast toP
    I_bitcast_D{..} -> Just toD
    
    I_trunc_V{..} -> Just $ ucast toVI
    I_zext_V{..} -> Just $ ucast toVI
    I_sext_V{..} -> Just $ ucast toVI
    I_fptrunc_V{..} -> Just $ ucast toVF
    I_fpext_V{..} -> Just $ ucast toVF    
    I_fptoui_V{..} -> Just $ ucast toVI
    I_fptosi_V{..} -> Just $ ucast toVI    
    I_uitofp_V{..} -> Just $ ucast toVF
    I_sitofp_V{..} -> Just $ ucast toVF
    I_ptrtoint_V{..} -> Just $ ucast toVI
    I_inttoptr_V{..} -> Just $ ucast toVP    
    I_addrspacecast_V{..} -> Just $ ucast toVP

    I_select_I{..} -> let (T t _) = trueI
                      in Just $ ucast t
    
    I_select_F{..} -> let (T t _) = trueF
                      in Just $ ucast t
    
    I_select_P{..} -> let (T t _) = trueP
                      in Just $ ucast t
                         
    I_select_VI{..} -> let (T t _) = trueVI
                       in Just $ ucast t
    
    I_select_VF{..} -> let (T t _) = trueVF
                       in Just $ ucast t
    
    I_select_VP{..} -> let (T t _) = trueVP
                       in Just $ ucast t
    
    I_select_First{..} -> let (T t _) = trueFirst
                          in Just $ ucast t
                             
    I_va_arg{..} -> Just typeD
    I_llvm_va_start{..} -> Nothing
    I_llvm_va_end{..} -> Nothing
    I_llvm_va_copy{..} -> Nothing
    I_llvm_gcroot{..} -> Nothing
    I_llvm_gcread{..} -> Nothing
    I_llvm_gcwrite{..} -> Nothing
    I_llvm_memcpy{..}->Nothing
    I_llvm_memmove{..}->Nothing
    I_llvm_memset{..}->Nothing
    I_llvm_returnaddress{..} -> Nothing
    I_llvm_frameaddress{..} -> Nothing
    I_llvm_stacksave{..} -> Just $ ucast $ ptr0 i8
    I_llvm_stackrestore{..} -> Nothing
    I_llvm_libm_una{..} -> case muop of
      Sqrt t _ -> Just $ ucast t
      Sin t _ -> Just $ ucast t
      Cos t _ -> Just $ ucast t
      Exp t _ -> Just $ ucast t
      Exp2 t _ -> Just $ ucast t
      Log t _ -> Just $ ucast t
      Log2 t _ -> Just $ ucast t
      Log10 t _ -> Just $ ucast t
      Fabs t _ -> Just $ ucast t
      Floor t _ -> Just $ ucast t
      Ceil t _ -> Just $ ucast t
      Ftrunc t _ -> Just $ ucast t
      Rint t _ -> Just $ ucast t
      NearByInt t _ -> Just $ ucast t
      Round t _ -> Just $ ucast t
    _ -> errorLoc FLC $ "unsupported " ++ show x 
    
    
instance TypeOf Rtype Dtype where
  typeof te x = case x of
    RtypeScalarI t -> Just $ ucast t
    RtypeScalarF t -> Just $ ucast t
    RtypeScalarP t -> Just $ ucast t
    RtypeVectorI t -> Just $ ucast t
    RtypeVectorF t -> Just $ ucast t
    RtypeVectorP t -> Just $ ucast t
    RtypeFirstClassD t -> Just $ ucast t
    RtypeRecordD t -> Just $ ucast t
    RtypeVoidU _ -> Nothing
    
instance TypeOf (Type CodeFunB X) Dtype where
  typeof te t = case t of
    Tfunction (rt,_) _ _ -> typeof te rt
    TnameCodeFunX n -> errorLoc FLC $ "unsupported " ++ show t

instance TypeOf CallSiteType Dtype where
  typeof te x = case x of
    CallSiteTypeRet rt -> typeof te rt
    CallSiteTypeFun t as -> case t of
      Tfunction (rt,_) _ _ -> typeof te rt
      TnameCodeFunX n -> errorLoc FLC $ "unsupported " ++ show x

instance (Eq g, Show g) => TypeOf (Const g) Dtype where    
  typeof te x = case x of
    C_getelementptr b (T bt _) indices -> 
      let et = getGetElemtPtrIndexedType te (ucast bt) ((fmap ucast indices)::[T (Type ScalarB I) (Value g)])
      in Just (ucast $ Tpointer (ucast et) 0)
    C_bitcast _ dt -> Just dt
    C_inttoptr _ dt -> Just $ ucast dt
    C_ptrtoint _ dt -> Just $ ucast dt
    _ -> errorLoc FLC $ show x

class SizeOf a where
  sizeof :: DataLayoutMetrics dlm => dlm -> TypeEnv -> a -> Word64

instance SizeOf Dtype where
  sizeof dlm te dt = let (SizeInByte s) = getTypeAllocSize dlm te dt
                     in fromIntegral s

class DataSizeOf a where
  dataSizeOf :: DataLayoutMetrics dlm => dlm -> TypeEnv -> a -> Word64
  
instance DataSizeOf Dtype where
  dataSizeOf dlm te dt = let (SizeInByte s) = getTypeStoreSize dlm te dt
                         in fromIntegral s
