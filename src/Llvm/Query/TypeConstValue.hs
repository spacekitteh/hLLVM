{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Llvm.Query.TypeConstValue where

#define FLC   (FileLoc $(srcLoc))


import Llvm.Data.Shared
import Llvm.Data.IrType
import qualified Data.Map as M
import qualified Data.Bits as B
import Llvm.Data.CoreIr
import Llvm.Query.IrCxt 
import Llvm.Query.Conversion
import Debug.Trace
import Llvm.Query.TypeDef
import Data.Word

eightBits :: Word32
eightBits = 8

data SizeInByte = SizeInByte Word32 deriving (Eq, Ord, Show)
data OffsetInByte = OffsetInByte Word32 deriving (Eq, Ord, Show)
data AlignInByte = AlignInByte Word32 deriving (Eq, Ord, Show)

fromSizeInBit :: SizeInBit -> SizeInByte
fromSizeInBit (SizeInBit n) = SizeInByte (n `div` eightBits)

toSizeInBit :: SizeInByte -> SizeInBit
toSizeInBit (SizeInByte n) = SizeInBit (n * eightBits)

fromAlignInBit :: AlignInBit -> AlignInByte
fromAlignInBit (AlignInBit n) = AlignInByte (n `div` eightBits)


getTypeAlignment :: TypeEnv -> Dtype -> AlignType -> AlignInByte
getTypeAlignment te@TypeEnv{..} t at = case t of
  DtypeScalarI st -> fromAlignInBit $ getTpAlignment dataLayout st
  DtypeScalarF st -> fromAlignInBit $ getTpAlignment dataLayout st  
  DtypeRecordD st -> case st of
    Tarray i et -> getTypeAlignment te et at
    Tstruct p tys -> case (p, at) of
      (Packed, AlignAbi)  -> AlignInByte 1
      _ -> let aa = case M.lookup (Just $ SizeInBit 0) (aggregates dataLayout) of
                 Just (aa, pa) -> selectAlignment at aa pa
                 Nothing -> errorX
               sl = getStructLayout te (p,tys)
           in (max (fromAlignInBit aa) (structAlignment sl))
  DtypeScalarP st -> case st of  
    Tpointer t a -> case (uncurry lookupOr) (ta $ Just a) (pointers dataLayout) of
      Just (s, aa, pa) -> (fromAlignInBit $ selectAlignment at aa pa)
      Nothing -> errorX
  where
    errorX :: a
    errorX = error $ "getTypedAlignment:unsupported " ++ show t
    getTpAlignment :: DataLayoutInfo -> Type ScalarB x -> AlignInBit
    getTpAlignment dl tp = case tp of
      TpI n -> case M.lookup (SizeInBit n) (ints dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> AlignInBit n
      TpF n -> case M.lookup (SizeInBit n) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> AlignInBit n
      TpV n -> case M.lookup (SizeInBit n) (vectors dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing ->AlignInBit n
      TpHalf -> AlignInBit 16 
      TpFloat -> let b = getTypeSizeInBits te (ucast tp)
                 in case M.lookup b (floats dl) of
                   Just (aa, pa) -> selectAlignment at aa pa
                   Nothing -> AlignInBit 32
      TpDouble -> let b = getTypeSizeInBits te (ucast tp)
                  in case M.lookup b (floats dl) of
                    Just (aa, pa) -> selectAlignment at aa pa
                    Nothing -> AlignInBit 64
      TpFp128 -> let b = getTypeSizeInBits te (ucast tp)
                 in case M.lookup b (floats dl) of
                   Just (aa, pa) -> selectAlignment at aa pa
                   Nothing -> errorX
      TpX86Fp80 -> let b = getTypeSizeInBits te (ucast tp)
                   in case M.lookup b (floats dl) of
                     Just (aa, pa) -> selectAlignment at aa pa
                     Nothing -> errorX
      TpPpcFp128 -> let b = getTypeSizeInBits te (ucast tp)
                    in case M.lookup b (floats dl) of
                      Just (aa, pa) -> selectAlignment at aa pa
                      Nothing -> errorX
      TpX86Mmx -> errorX

ta :: Maybe AddrSpace -> (LayoutAddrSpace, LayoutAddrSpace)
ta x = case x of
  Just n | n == 0 -> (LayoutAddrSpaceUnspecified, LayoutAddrSpace n)
  Just n -> (LayoutAddrSpace n, LayoutAddrSpace n)  
  Nothing -> (LayoutAddrSpaceUnspecified, LayoutAddrSpace 0)


getCallFrameTypeAlignment :: TypeEnv -> Dtype -> AlignInByte
getCallFrameTypeAlignment te@TypeEnv{..} ty = case stackAlign dataLayout of
  StackAlign n -> fromAlignInBit n
  StackAlignUnspecified -> getTypeAlignment te ty AlignAbi

lookupOr :: Ord a => a -> a -> M.Map a r -> Maybe r
lookupOr a1 a2 m = maybe (M.lookup a2 m) Just (M.lookup a1 m)

getTypeSizeInBits :: TypeEnv -> Dtype -> SizeInBit
getTypeSizeInBits te@TypeEnv{..} dt = case dt of
  (DtypeRecordD t) -> case t of
    Tarray i et -> let SizeInBit n = getTypeAllocSizeInBits te et
                   in SizeInBit (i * n)
    Tstruct pk ts -> let sl = getStructLayout te (pk, ts)
                     in toSizeInBit $ structSize sl
    TnameRecordD n -> case getTypeByTname n typedefs of
      Just d -> getTypeSizeInBits te d
      Nothing -> errorLoc FLC ("undefined " ++ show n)
    _ -> errorLoc FLC (show t)
  (DtypeScalarP (Tpointer t a)) -> case (uncurry lookupOr) (ta $ Just a) (pointers dataLayout) of
    Just (n, _, _) -> n
    Nothing -> error $ "getTypeSizeInBits:unsupported type " ++ show t
  (DtypeScalarI t) -> case t of  
    TpI n -> SizeInBit n
    TpV n -> SizeInBit n
  (DtypeScalarF t) -> case t of  
    TpF n -> SizeInBit n
    TpHalf -> SizeInBit 16
    TpFloat -> SizeInBit 32
    TpDouble -> SizeInBit 64
    TpFp128 -> SizeInBit 128
    TpX86Fp80 -> SizeInBit 80
    TpPpcFp128 -> SizeInBit 128
  _ -> errorLoc FLC (show dt)
      

getTypeStoreSize :: TypeEnv ->  Dtype -> SizeInByte
getTypeStoreSize te ty = let (SizeInBit tyBits) = getTypeSizeInBits te ty
                         in fromSizeInBit (SizeInBit (tyBits + 7))

getTypeStoreSizeInBits :: TypeEnv -> Dtype -> SizeInBit
getTypeStoreSizeInBits te ty = toSizeInBit (getTypeStoreSize te ty)

getTypeAllocSize :: TypeEnv -> Dtype -> SizeInByte
getTypeAllocSize te ty = roundUpAlignment (getTypeStoreSize te ty) (getTypeAlignment te ty AlignAbi)

getTypeAllocSizeInBits :: TypeEnv -> Dtype -> SizeInBit
getTypeAllocSizeInBits te ty = toSizeInBit (getTypeAllocSize te ty)

data StructLayout = StructLayout { structSize :: SizeInByte
                                 , structAlignment :: AlignInByte
                                 , numElements :: Integer
                                 , memberOffsets :: [OffsetInByte]
                                 } deriving (Eq, Ord, Show)

roundUpAlignment :: SizeInByte -> AlignInByte -> SizeInByte
roundUpAlignment (SizeInByte val) (AlignInByte align) = SizeInByte $ (val + (align -1)) B..&. (B.complement (align - 1))

getStructLayout :: TypeEnv -> (Packing, [Dtype]) -> StructLayout
getStructLayout te@TypeEnv{..} (pk, tys) = 
  let (totalSize@(SizeInByte totalSizeByte), offsets, alignment@(AlignInByte alignmentByte)) =
        foldl (\(curSize@(SizeInByte curSizeByte), offsets, AlignInByte structAlignment0) ty ->
                let tyAlign@(AlignInByte tyAlignByte) = case pk of
                      Packed -> AlignInByte 1
                      Unpacked -> getTypeAlignment te ty AlignAbi
                    (SizeInByte nextOffsetByte) = if curSizeByte B..&. (tyAlignByte - 1) /= 0 then roundUpAlignment curSize tyAlign
                                                  else curSize
                    (SizeInByte tySize) = getTypeAllocSize te ty
                in (SizeInByte $ nextOffsetByte + tySize, (OffsetInByte nextOffsetByte):offsets, AlignInByte $ max tyAlignByte structAlignment0)
              ) (SizeInByte 0, [], AlignInByte 1) tys
  in StructLayout { structSize = if (totalSizeByte B..&. (alignmentByte - 1)) /= 0 then roundUpAlignment totalSize alignment
                                 else totalSize
                  , structAlignment = alignment
                  , numElements = toInteger $ length tys
                  , memberOffsets = reverse offsets
                  }

getPointerSizeInBits :: DataLayoutInfo -> Maybe AddrSpace -> SizeInBit
getPointerSizeInBits dl mas = case (uncurry lookupOr) (ta mas) (pointers dl) of
  Just (s, aa, pa) -> s
  Nothing -> error $ "getPointerSizeInBits:unsupported " ++ show dl

getPointerSize :: DataLayoutInfo -> Maybe AddrSpace -> SizeInByte
getPointerSize dl mas = fromSizeInBit (getPointerSizeInBits dl mas)

getPointerAlignment :: DataLayoutInfo -> Maybe AddrSpace -> AlignType -> AlignInByte
getPointerAlignment dl mas at = case (uncurry lookupOr) (ta mas) (pointers dl) of
  Just (s, aa, pa) -> fromAlignInBit $ selectAlignment at aa pa
  Nothing -> error $ "getPointerAlignment:unsupported " ++ show dl



getScalarTypeSizeInBits :: DataLayoutInfo -> ScalarType -> SizeInBit
getScalarTypeSizeInBits dl x = 
  SizeInBit (case x of
                ScalarTypeI e -> case e of
                  (TpI n) -> n
                  (TpV n) -> n
                  TpX86Mmx -> 64
                ScalarTypeF e -> case e of
                  (TpF n) -> n
                  TpHalf -> 16
                  TpFloat -> 32
                  TpDouble -> 64
                  TpFp128 -> 128
                  TpX86Fp80 -> 80
                  TpPpcFp128 -> 128
                ScalarTypeP _ -> 32
            )


getGetElemtPtrIndexedType :: TypeEnv -> Dtype -> [T (Type ScalarB I) Value] -> Dtype
getGetElemtPtrIndexedType te x is | trace ("getGetElemtPtrIndexedType : type:" ++ show x ++ ", " ++ show is) False = undefined
getGetElemtPtrIndexedType te x is = case is of 
  hd:tl -> case x of
    DtypeScalarP (Tpointer et _) -> if tl == [] then dcast FLC et
                                    else getGetElemtPtrIndexedType te (dcast FLC et) tl
    DtypeRecordD (TnameRecordD _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is
    DtypeRecordD (TquoteNameRecordD _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is    
    DtypeRecordD (TnoRecordD _) -> getGetElemtPtrIndexedType te (getTypeDef te x) is
    _ -> let ct = getTypeAtIndex te x (castTypedValueToTypedConst hd)
         in if tl == [] then ct
            else getGetElemtPtrIndexedType te ct tl

getTypeAtIndex :: Show t => TypeEnv -> Dtype -> T t Const -> Dtype
getTypeAtIndex _ x@(DtypeScalarP (Tpointer _ _)) _ = error $ "does not expect a pointer type: " ++ show x
getTypeAtIndex te t idx = 
  case (getTypeDef te t) of
    DtypeRecordD (Tstruct _ ts) -> let (ii::Word32) = fromIntegral $ getUniqueInteger idx
                                   in if ii < fromIntegral (length ts) then ts !! (fromIntegral ii)
                                      else error ("Invalid structure index! " ++ show ii)
    DtypeRecordD (Tarray n et) -> et 
    x -> error $ "Invalid indexing of " ++ show x

getTypeDef :: TypeEnv -> Dtype -> Dtype
getTypeDef TypeEnv{..} t = case t of
  DtypeRecordD (TnameRecordD n) -> maybe (error $ show t ++ " is not defined.") id (getTypeByTname n typedefs)
  DtypeRecordD (TquoteNameRecordD n) -> maybe (error $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)
  DtypeRecordD (TnoRecordD n) -> maybe (error $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeRecordD _ -> t
  DtypeScalarI _ -> t
  DtypeScalarP (TnameScalarP n) -> maybe (error $ show t ++ " is not defined.") id (getTypeByTname n typedefs) 
  DtypeScalarP (TquoteNameScalarP n) -> maybe (error $ show t ++ " is not defined.") id (getTypeByTquoteName n typedefs)   
  DtypeScalarP (TnoScalarP n) -> maybe (error $ show t ++ " is not defined.") id (getTypeByTno n typedefs)
  DtypeScalarP _ -> t   
  DtypeScalarF _ -> t
  DtypeVectorI _ -> t
  DtypeVectorP _ -> t
  DtypeVectorF _ -> t
  DtypeFirstClassD _ -> t

getElementType :: TypeEnv -> Dtype -> Dtype
getElementType te t = case getTypeDef te t of
  DtypeRecordD (Tarray _ t1) -> t1
  DtypeScalarP (Tpointer t1 _) -> dcast FLC t1
  _ -> error $ (show t) ++ " has no element type"

getPointsToType :: TypeEnv -> Type ScalarB P -> Dtype
getPointsToType te t = case getTypeDef te (ucast t) of
  DtypeScalarP (Tpointer t1 _) -> dcast FLC t1
  _ -> errorLoc FLC $ show t ++ " has no element type"

{-
getScalarType :: TypeEnv -> Dtype -> Dtype
getScalarType te (DtypeAgg (Tvector _ t)) = t
getScalarType te x = x
-}

castIsValid :: DataLayoutInfo -> Conversion ScalarB v -> Bool -- ConvertOp -> Dtype -> Dtype -> Bool
-- castIsValid op src dest | vsize src == vsize dest = case op of
castIsValid dl op = True 
                    {-case op of  
  Trunc (T src _) dest -> getScalarTypeSizeInBits dl src > getScalarTypeSizeInBits dl
  Zext (T src _) dest -> getScalarTypeSizeInBits dl src < getScalarTypeSizeInBits dl
  Sext (T src _) dest -> getScalarTypeSizeInBits dl src < getScalarTypeSizeInBits dl
  FpTrunc (T src _) dest -> getScalarTypeSizeInBits dl src > getScalarTypeSizeInBits dl dest
  FpExt (T src _) dest -> getScalarTypeSizeInBits dl src < getScalarTypeSizeInBits dl dest
  UiToFp (T src _) dest -> vsize src == vsize dest && vmap isInt src && vmap isFp dest
  SiToFp (T src _) dest -> vsize src == vsize dest && vmap isInt src && vmap isFp dest
  FpToUi (T src _) dest -> vsize src == vsize dest && vmap isFp src && vmap isInt dest
  FpToSi (T src _) dest -> vsize src == vsize dest && vmap isFp src && vmap isInt dest
  PtrToInt (T src _) dest -> vsize src == vsize dest && vmap isInt src && vmap isPtr dest
  IntToPtr (T src _) dest -> vsize src == vsize dest && vmap isPtr src && vmap isInt dest
  Bitcast (T src _) dest -> vsize src == vsize dest &&  (vmap isPtr src && vmap isPtr dest && vmap addrSpace src == vmap addrSpace dest)  {- ptr to ptr -}
                            || (vmap (not . isPtr) src) && (vmap (getScalarTypeSizeInBits dl) src == vmap (getScalarTypeSizeInBits dl) dest) {- non ptr to ptr -}
  AddrSpaceCast (T src _) dest -> vsize src == vsize dest && vmap isPtr src && vmap isPtr dest && vmap addrSpace src == vmap addrSpace dest
  where isValid f s d = vmap f s && vmap f d
-}


castable :: Show v => DataLayoutInfo -> Conversion ScalarB v -> Conversion ScalarB v
castable dl op = if castIsValid dl op then op
                 else error $ "Invalid cast:" ++ show op 



getConstArray :: Dtype -> [T Dtype Const] -> T Dtype Const
getConstArray t@(DtypeRecordD (Tarray n el)) [] = T t (C_array $ fmap (\x -> TypedConst $ T el C_zeroinitializer) [1..n])
getConstArray t@(DtypeRecordD (Tarray n el)) l = if or $ fmap (\(T vt _) -> vt /= el) l then error "type mismatch"
                                                 else T t (C_array (fmap TypedConst l))
getConstArray t _ = error "type mismatch"


getTypedConst :: TypeEnv -> (Type ScalarB x -> Const) -> Dtype -> T Dtype Const
getTypedConst te f t = case (getTypeDef te t) of
  (DtypeRecordD (Tarray n et)) -> let ev = getTypedConst te f et
                                  in T t (C_arrayN n $ TypedConst ev)
  (DtypeRecordD (Tstruct pk ts)) -> let evs = fmap (getTypedConst te f) ts
                                    in T t (C_struct pk (fmap TypedConst evs))

getNullValue :: Type ScalarB x -> Const
getNullValue t = case t of
--  TpVoid -> error $ show t ++ " has no null const value"
--  TpNull -> error $ show t ++ " has no null const value"
--  TpLabel -> error $ show t ++ " has no null const value"
  _ -> C_zeroinitializer

getUndefValue :: Type ScalarB x -> Const
getUndefValue t = case t of
--  TpVoid -> error $ show t ++ " has no undef const value"
--  TpNull -> error $ show t ++ " has no undef const value"
--  TpLabel -> error $ show t ++ " has no undef const value"
  _ -> C_undef

getPointerCast :: Show v => DataLayoutInfo -> T (Type ScalarB P) v -> Type ScalarB I -> Conversion ScalarB v
getPointerCast dl (T ts v) td = castable dl (PtrToInt (T ts v) td)

getBitCast :: Show v => DataLayoutInfo -> T Dtype v -> Dtype -> Conversion ScalarB v
getBitCast dl (T t c) dt = castable dl (Bitcast (T t c) dt)

getGetElementPtr :: T (Type ScalarB P) Const -> [T (Type ScalarB I) Const] -> IsOrIsNot InBounds -> GetElementPtr ScalarB Const
getGetElementPtr (T t cv) indices isB = GetElementPtr isB (T t cv) indices


getIntStoreTypeForPointer :: DataLayoutInfo -> Dtype
getIntStoreTypeForPointer dl =  let (SizeInBit sizeInBits) = getPointerSizeInBits dl Nothing
                                in DtypeScalarI $ TpI sizeInBits

typeOfCallSite :: IrCxt -> CallSite -> Maybe Rtype
typeOfCallSite = error "typeOfCallSite is not defined yet"

typeOfExtractElem = undefined
typeOfInsertElem _ = Nothing
typeOfShuffleVector = undefined
typeOfExtractValue = undefined
typeOfInsertValue _ = Nothing

{-
castTcToTv :: T t Const -> T t Value
castTcToTv (T t c) = (T t (Val_const c))
-}


class TypeOf a t | a -> t where  
  typeof :: TypeEnv -> a -> t
  
  
instance TypeOf Cinst Dtype where
  typeof te x = case x of
    I_getelementptr{..} -> let (T bt _) = pointer
                               et = getGetElemtPtrIndexedType te (ucast bt) indices
                           in (ucast $ Tpointer (ucast et) 0)
    I_add{..} -> ucast typeI
    I_bitcast{..} -> ucast toP
    I_ptrtoint{..} -> ucast toI
    I_inttoptr{..} -> ucast toP
    I_load{..} -> let (T (Tpointer et _) _) = pointer
                  in dcast FLC et
--    I_llvm_dbg_declare{..} -> error $ show x ++ " has no type"
    _ -> errorLoc FLC $ "unsupported " ++ show x 