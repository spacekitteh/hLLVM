{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Llvm.Query.TypeConstValue where


import Llvm.Data.Shared (Type (..), TypePrimitive (..), AddrSpace(..), Packing(..))
import Llvm.Data.Shared.DataLayout (DataLayoutInfo (..),LayoutAddrSpace(..), AbiAlign (..), PrefAlign(..), SizeInBit(..), AlignType (..), AlignInBit(..), selectAlignment, StackAlign(..))
import qualified Data.Map as M
import qualified Data.Bits as B
import Llvm.Data.CoreIr
import Llvm.Query.Qerror
import Llvm.Query.Conversion
import Llvm.Query.Translucent
import Data.List (or)

eightBits = 8

data SizeInByte = SizeInByte Integer deriving (Eq, Ord, Show)
data OffsetInByte = OffsetInByte Integer deriving (Eq, Ord, Show)
data AlignInByte = AlignInByte Integer deriving (Eq, Ord, Show)

fromSizeInBit :: SizeInBit -> SizeInByte
fromSizeInBit (SizeInBit n) = SizeInByte (n `div` eightBits)

toSizeInBit :: SizeInByte -> SizeInBit
toSizeInBit (SizeInByte n) = SizeInBit (n * eightBits)

fromAlignInBit :: AlignInBit -> AlignInByte
fromAlignInBit (AlignInBit n) = AlignInByte (n `div` eightBits)

getTypeAlignment :: MonadError Qerror m => DataLayoutInfo -> Type -> AlignType -> m AlignInByte
getTypeAlignment dl t at = 
  case t of
    Tprimitive tp -> liftM fromAlignInBit (getTpAlignment dl tp)
    Tmetadata -> errorX
    Topaque -> errorX
    Tname s -> errorX
    TquoteName s -> errorX
    Tno s -> errorX
    TupRef s -> errorX
    Tarray i et -> getTypeAlignment dl et at
    Tvector i s -> errorX
    Tstruct p tys -> case (p, at) of
      (Packed, AlignAbi)  -> return $ AlignInByte 1
      _ -> do { aa <- case M.lookup (Just $ SizeInBit 0) (aggregates dl) of
                   Just (aa, pa) -> return $ selectAlignment at aa pa
                   Nothing -> errorX
              ; sl <- getStructLayout dl (p,tys)
              ; return (max (fromAlignInBit aa) (structAlignment sl))
              }
    Tpointer t a -> case (uncurry lookupOr) (ta a) (pointers dl) of
      Just (s, aa, pa) -> return (fromAlignInBit $ selectAlignment at aa pa)
      Nothing -> errorX
    Tfunction t tpl fas -> errorX
    Tderef t -> errorX
  where
    errorX :: MonadError Qerror m => m a
    errorX = throwError $ QerrWithInfo $ "getTypedAlignment:unsupported " ++ show t
    getTpAlignment dl tp = case tp of
      TpI n -> case M.lookup (SizeInBit n) (ints dl) of 
        Just (aa, pa) -> return $ selectAlignment at aa pa
        Nothing -> errorX
      TpF n -> case M.lookup (SizeInBit n) (floats dl) of
        Just (aa, pa) -> return $ selectAlignment at aa pa
        Nothing -> errorX
      TpV n -> case M.lookup (SizeInBit n) (vectors dl) of
        Just (aa, pa) -> return $ selectAlignment at aa pa
        Nothing -> errorX
      TpVoid -> do { b <- getPrimitiveTypeSizeInBits dl tp
                   ; case M.lookup b (ints dl) of 
                        Just (aa, pa) -> return $ selectAlignment at aa pa
                        Nothing -> errorX
                   }
      TpHalf -> undefined 
      TpFloat -> do { b <- getPrimitiveTypeSizeInBits dl tp
                    ; case M.lookup b (floats dl) of
                      Just (aa, pa) -> return $ selectAlignment at aa pa
                      Nothing -> errorX
                    }
      TpDouble -> do { b <- getPrimitiveTypeSizeInBits dl tp
                     ; case M.lookup b (floats dl) of
                          Just (aa, pa) -> return $ selectAlignment at aa pa
                          Nothing -> errorX
                     }
      TpFp128 -> do { b <- getPrimitiveTypeSizeInBits dl tp
                    ; case M.lookup b (floats dl) of
                         Just (aa, pa) -> return $ selectAlignment at aa pa
                         Nothing -> errorX
                    }
      TpX86Fp80 -> do { b <- getPrimitiveTypeSizeInBits dl tp
                      ; case M.lookup b (floats dl) of
                           Just (aa, pa) -> return $ selectAlignment at aa pa
                           Nothing -> errorX
                      }
      TpPpcFp128 -> do { b <- getPrimitiveTypeSizeInBits dl tp
                       ; case M.lookup b (floats dl) of
                            Just (aa, pa) -> return $ selectAlignment at aa pa
                            Nothing -> errorX
                       }
      TpX86Mmx -> errorX
      TpNull -> errorX
      TpLabel -> errorX
  
  
ta x = case x of 
  AddrSpace n -> (LayoutAddrSpace n, LayoutAddrSpace n)
  AddrSpaceUnspecified -> (LayoutAddrSpaceUnspecified, LayoutAddrSpace 0)

 
getCallFrameTypeAlignment :: MonadError Qerror m => DataLayoutInfo -> Type -> m AlignInByte  
getCallFrameTypeAlignment dl ty = case stackAlign dl of
  StackAlign n -> return $ fromAlignInBit n
  StackAlignUnspecified -> getTypeAlignment dl ty AlignAbi

lookupOr :: Ord a => a -> a -> M.Map a r -> Maybe r          
lookupOr a1 a2 m = maybe (M.lookup a2 m) Just (M.lookup a1 m) 
        
getPrimitiveTypeSizeInBits :: MonadError Qerror m => DataLayoutInfo -> TypePrimitive -> m SizeInBit
getPrimitiveTypeSizeInBits dl t = case t of
  TpI n -> return $ SizeInBit n
  TpF n -> return $ SizeInBit n
  TpV n -> return $ SizeInBit n
  TpVoid -> return $ SizeInBit eightBits
  TpFloat -> return $ SizeInBit 32
  TpDouble -> return $ SizeInBit 64
  TpFp128 -> return $ SizeInBit 128
  TpX86Fp80 -> return $ SizeInBit 80
  TpPpcFp128 -> return $ SizeInBit 128
  TpLabel -> case lookupOr (LayoutAddrSpaceUnspecified) (LayoutAddrSpace 0) (pointers dl) of
    Just (SizeInBit n, _, _) -> return $ SizeInBit n
    Nothing -> throwError $ QerrWithInfo $ "getPrimitiveTypeSizeInBits:unsupported type " ++ show t
    
getTypeSizeInBits :: MonadError Qerror m => DataLayoutInfo -> Type -> m SizeInBit
getTypeSizeInBits dl t = case t of
  Tprimitive tp -> getPrimitiveTypeSizeInBits dl tp
  Tarray i t -> do { SizeInBit n <- getTypeAllocSizeInBits dl t
                   ; return $ SizeInBit (i * n)
                   }
  Tvector n t -> undefined
  Tstruct pk ts -> do { sl <- getStructLayout dl (pk, ts)
                      ; return $ toSizeInBit $ structSize sl
                      }
  Tpointer t a -> case (uncurry lookupOr) (ta a) (pointers dl) of
    Just (n, _, _) -> return n
    Nothing -> throwError $ QerrWithInfo "aaaa"
  _ -> throwError $ QerrWithInfo $ "getTypeSizeInBits:unsupported type " ++ show t


getTypeStoreSize :: MonadError Qerror m => DataLayoutInfo -> Type -> m SizeInByte
getTypeStoreSize dl ty = do { (SizeInBit tyBits) <- getTypeSizeInBits dl ty 
                            ; return $ fromSizeInBit (SizeInBit (tyBits + 7))
                            }

getTypeStoreSizeInBits :: MonadError Qerror m => DataLayoutInfo -> Type -> m SizeInBit
getTypeStoreSizeInBits dl ty = liftM toSizeInBit (getTypeStoreSize dl ty)

getTypeAllocSize :: MonadError Qerror m => DataLayoutInfo -> Type -> m SizeInByte
getTypeAllocSize dl ty = liftM2 roundUpAlignment (getTypeStoreSize dl ty) (getTypeAlignment dl ty AlignAbi)

getTypeAllocSizeInBits :: MonadError Qerror m => DataLayoutInfo -> Type -> m SizeInBit
getTypeAllocSizeInBits dl ty = liftM toSizeInBit (getTypeAllocSize dl ty)

data StructLayout = StructLayout { structSize :: SizeInByte
                                 , structAlignment :: AlignInByte
                                 , numElements :: Integer
                                 , memberOffsets :: [OffsetInByte]
                                 } deriving (Eq, Ord, Show)

roundUpAlignment :: SizeInByte -> AlignInByte -> SizeInByte
roundUpAlignment (SizeInByte val) (AlignInByte align) = SizeInByte $ (val + (align -1)) B..&. (B.complement (align - 1)) 

getStructLayout :: MonadError Qerror m => DataLayoutInfo -> (Packing, [Type]) -> m StructLayout
getStructLayout dl (pk, tys) = do { (totalSize@(SizeInByte totalSizeByte), offsets, alignment@(AlignInByte alignmentByte)) <-
                                       foldM (\(curSize@(SizeInByte curSizeByte), offsets, AlignInByte structAlignment0) ty -> 
                                               do { tyAlign@(AlignInByte tyAlignByte) <- case pk of 
                                                       Packed -> return $ AlignInByte 1 
                                                       Unpacked -> getTypeAlignment dl ty AlignAbi
                                                  ; let (SizeInByte nextOffsetByte) = if curSizeByte B..&. (tyAlignByte - 1) /= 0 then roundUpAlignment curSize tyAlign
                                                                                      else curSize
                                                  ; (SizeInByte tySize) <- getTypeAllocSize dl ty
                                                  ; return (SizeInByte $ nextOffsetByte + tySize, (OffsetInByte nextOffsetByte):offsets, AlignInByte $ max tyAlignByte structAlignment0)
                                                  }
                                           ) (SizeInByte 0, [], AlignInByte 1) tys
                                  ; return $ StructLayout { structSize = if (totalSizeByte B..&. (alignmentByte - 1)) /= 0 then roundUpAlignment totalSize alignment
                                                                         else totalSize
                                                          , structAlignment = alignment
                                                          , numElements = toInteger $ length tys
                                                          , memberOffsets = reverse offsets
                                                          }
                                  }

getPointerSizeInBits :: MonadError Qerror m => DataLayoutInfo -> m SizeInBit                                  
getPointerSizeInBits dl = case (uncurry lookupOr) (ta $ AddrSpace 0) (pointers dl) of
  Just (s, aa, pa) -> return s
  Nothing -> throwError $ QerrWithInfo "getPointerSizeInBits:unsupported "
    
getPointerSize :: MonadError Qerror m => DataLayoutInfo -> m SizeInByte  
getPointerSize dl = liftM fromSizeInBit (getPointerSizeInBits dl)

getPointerAlignment :: MonadError Qerror m => DataLayoutInfo -> AlignType -> m AlignInByte
getPointerAlignment dl at = case (uncurry lookupOr) (ta $ AddrSpace 0) (pointers dl) of
  Just (s, aa, pa) -> return $ fromAlignInBit $ selectAlignment at aa pa
  Nothing -> throwError $ QerrWithInfo "getPointerAlignment:unsupported"
  
  

isSized :: Type -> Bool
isSized t = case t of
  Tprimitive tp -> case tp of
    TpVoid -> False
    TpNull -> False
    TpLabel -> False
    _ -> True
  Tpointer _ _ -> True
  Tstruct _ el -> all isSized el
  Tvector _ et -> isSized et
  Tarray _ et -> isSized et
  _ -> False
  
  
isInt :: Type -> Bool  
isInt t = case t of
  Tprimitive (TpI _) -> True
  _ -> False
  
isFp :: Type -> Bool  
isFp t = case t of
  Tprimitive (TpF _) -> True
  _ -> False
  
isPtr :: Type -> Bool  
isPtr t = case t of
  Tpointer t _ -> True
  _ -> False
  
addrSpace :: Type -> Maybe AddrSpace
addrSpace (Tpointer _ a) = Just a
addrSpace _ = Nothing
  
getPrimitiveSizeInBits :: Type -> Maybe Integer  
getPrimitiveSizeInBits x = case x of
  Tprimitive tx -> case tx of
    (TpI n) -> Just n
    (TpF n) -> Just n
    (TpV n) -> Just n
    (TpVoid) -> Nothing
    TpHalf -> Just 16
    TpFloat -> Just 32
    TpDouble -> Just 64
    TpFp128 -> Just 128
    TpX86Fp80 -> Just 80
    TpPpcFp128 -> Just 128
    TpX86Mmx -> Just 64
    _ -> Nothing
  _ -> Nothing 
  
  

getGetElemtPtrIndexedType :: MonadError Qerror m => Type -> [Typed Value] -> m Type
getGetElemtPtrIndexedType x is = case getScalarType x of
  Tpointer et _ -> if length is == 0 then
                     return et
                   else if not $ isSized et then
                          throwError (QerrWithInfo $ "Invalid based type " ++ show et)
                        else 
                          do { ic <- typedValueToTypedConst (head is)
                             ; ct <- getTypeAtIndex et ic
                             ; getGetElemtPtrIndexedType ct (tail is)
                             }


getTypeAtIndex :: MonadError Qerror m => Type -> Typed Const -> m Type
getTypeAtIndex x@(Tpointer _ _) _ = throwError (QerrWithInfo $ "Invalid base type " ++ show x)
getTypeAtIndex t idx = 
  do { ii <- getUniqueInteger idx
     ; case t of
       Tstruct p ts -> if ii < toInteger (length ts) then return (ts !! (fromInteger ii))
                       else throwError (QerrWithInfo $ "Invalid structure index! " ++ show ii)
       Tvector n t -> if ii < n then return t
                      else throwError (QerrWithInfo $ "Invalid structure index! " ++ show ii)
       Tarray n t -> if ii < n then return t
                     else throwError (QerrWithInfo $ "Invalid structure index! " ++ show ii)
     }


getElementType :: MonadError Qerror m => Type -> m Type  
getElementType t = case t of
  Tarray _ t1 -> return t1
  Tvector _ t1 -> return t1
  Tpointer t1 _ -> return t1
  _ -> throwError (QerrWithInfo $ (show t) ++ " has not element type")
  
getScalarType :: Type -> Type  
getScalarType (Tvector _ t) = t 
getScalarType t = t


castIsValid :: ConvertOp -> Type -> Type -> Bool
castIsValid op src dest | vsize src == vsize dest = case op of 
  Trunc -> isValid isInt && vmap getPrimitiveSizeInBits src > vmap getPrimitiveSizeInBits dest
  Zext -> isValid isInt && vmap getPrimitiveSizeInBits src < vmap getPrimitiveSizeInBits dest
  Sext -> isValid isInt && vmap getPrimitiveSizeInBits src < vmap getPrimitiveSizeInBits dest
  FpTrunc -> isValid isFp && vmap getPrimitiveSizeInBits src > vmap getPrimitiveSizeInBits dest
  FpExt -> isValid isFp && vmap getPrimitiveSizeInBits src < vmap getPrimitiveSizeInBits dest
  UiToFp -> vmap isInt src && vmap isFp dest
  SiToFp -> vmap isInt src && vmap isFp dest
  FpToUi -> vmap isFp src && vmap isInt dest
  FpToSi -> vmap isFp src && vmap isInt dest
  PtrToInt -> vmap isInt src && vmap isPtr dest
  IntToPtr -> vmap isPtr src && vmap isInt dest
  Bitcast -> (vmap isPtr src && vmap isPtr dest && vmap addrSpace src == vmap addrSpace dest)  {- ptr to ptr -}
             || (vmap (not . isPtr) src) && (vmap getPrimitiveSizeInBits src == vmap getPrimitiveSizeInBits dest) {- non ptr to ptr -}
  AddrSpaceCast -> vmap isPtr src && vmap isPtr dest && vmap addrSpace src == vmap addrSpace dest
  where isValid f = vmap f src && vmap f dest
castIsValid _ _ _ = False
        
        
assertCast :: MonadError Qerror m => ConvertOp -> Type -> Type -> m ()        
assertCast op src dest = if castIsValid op src dest then return ()
                         else throwError (QerrWithInfo $ "Invalid cast:" ++ show op ++ " " ++ show src ++ " to " ++ show dest)

{- Const -}
getGetElementPtr :: Typed Const -> [Typed Const] -> IsOrIsNot InBounds -> Const
getGetElementPtr c indices isB = CgEp (GetElemPtr isB c indices)

getBitCast :: MonadError Qerror m => Typed Const -> Type -> m (Typed Const)
getBitCast x@(TypedData t c) dt = if dt == t then return x
                                  else do { _ <- assertCast Bitcast t dt 
                                          ; return (TypedData dt (Cconv $ Conversion Bitcast x dt))
                                          }

getSplatValue :: Typed Const -> Maybe (Typed Const)
getSplatValue (TypedData _ (Cca (Cvector l))) = if all (\x -> x == head l) l then Just $ head l
                                                else Nothing                              

isNullValue :: Typed Const -> Bool                                                      
isNullValue (TypedData _ (Ccp CpNull)) = True 
isNullValue UntypedNull = True
isNullValue _ = False

isUndef :: Typed Const -> Bool
isUndef (TypedData _ (Ccp CpUndef)) = True
isUndef _ = False


getConstArray :: MonadError Qerror m => Type -> [Typed Const] -> m (Typed Const)
getConstArray t@(Tarray n el) [] = return (TypedData t (Cca (Carray $ fmap (\x -> TypedData el (Ccp CpZero)) [1..n])))
getConstArray t@(Tarray n el) l = if or $ fmap (\(TypedData vt _) -> vt /= el) l then
                                    throwError $ QerrWithInfo "type mismatch"
                                  else
                                    return (TypedData t (Cca (Carray l)))
getConstArray t _ = throwError $ QerrWithInfo "type mismatch"


getTypedConst :: MonadError Qerror m => (TypePrimitive -> m SimpleConstant) -> Type -> m (Typed Const)
getTypedConst f t = case t of
  Tprimitive tp -> f tp >>= \x -> return (TypedData t (Ccp x)) 
  Tvector n et -> do { ev <- getTypedConst f et
                     ; return $ TypedData t (Cca (CvectorN n ev))
                     }
  Tarray n et -> do { ev <- getTypedConst f et
                    ; return $ TypedData t (Cca (CarrayN n ev))
                    }
  Tstruct pk ts -> do { evs <- mapM (getTypedConst f) ts
                      ; return $ TypedData t (Cca (Cstruct pk evs))
                      }
  Tfunction _ _ _ -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  Tmetadata -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  Topaque -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  Tname _ -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  TquoteName _ -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  Tno _ -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  TupRef _ -> throwError $ QerrWithInfo $ show t ++ " has no const value"
  Tderef _ -> throwError $ QerrWithInfo $ show t ++ " has no const value"

getNullValue :: MonadError Qerror m => TypePrimitive -> m SimpleConstant
getNullValue t = case t of
  TpVoid -> throwError $ QerrWithInfo $ show t ++ " has no null const value"
  TpNull -> throwError $ QerrWithInfo $ show t ++ " has no null const value"
  TpLabel -> throwError $ QerrWithInfo $ show t ++ " has no null const value"
  _ -> return CpZero

getUndefValue :: MonadError Qerror m => TypePrimitive -> m SimpleConstant  
getUndefValue t = case t of
  TpVoid -> throwError $ QerrWithInfo $ show t ++ " has no undef const value"
  TpNull -> throwError $ QerrWithInfo $ show t ++ " has no undef const value"
  TpLabel -> throwError $ QerrWithInfo $ show t ++ " has no undef const value"
  _ -> return CpUndef 

createPointerCast :: MonadError Qerror m => Typed Value -> Type -> m (Typed Value)
createPointerCast tv@(TypedData ts v) td = do { assertCast PtrToInt ts td
                                              ; return (TypedData td (Ve $ Ec $ Conversion PtrToInt tv td))
                                              }