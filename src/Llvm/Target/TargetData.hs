module Llvm.Target.TargetData where

import Llvm.VmCore.AtomicEntity (Type (..), TypePrimitive (..), AddrSpace(..), Packing(..))
import Llvm.VmCore.DataLayout (DataLayoutInfo (..),LayoutAddrSpace(..), AbiAlign (..), PrefAlign(..), SizeInBit(..), AlignType (..), AlignInBit(..), selectAlignment)
import qualified Data.Map as M
import qualified Data.Bits as B

eightBits = 8

data SizeInByte = SizeInByte Integer deriving (Eq, Ord, Show)
data OffsetInByte = OffsetInByte Integer deriving (Eq, Ord, Show)
data AlignInByte = AlignInByte Integer deriving (Eq, Ord, Show)


fromSizeInBit (SizeInBit n) = SizeInByte (n `div` eightBits)
toSizeInBit (SizeInByte n) = SizeInBit (n * eightBits)
fromAlignInBit (AlignInBit n) = AlignInByte (n `div` eightBits)

getTypeAlignment :: DataLayoutInfo -> Type -> AlignType -> AlignInByte
getTypeAlignment dl t at = 
  case t of
    Tprimitive tp -> fromAlignInBit $ getTpAlignment dl tp
    Tmetadata -> undefined
    Topaque -> undefined
    Tname s -> undefined
    TquoteName s -> undefined
    Tno s -> undefined
    TupRef s -> undefined
    Tarray i t -> undefined
    Tvector i s -> undefined
    Tstruct p tys -> case (p, at) of
      (Packed, AlignAbi)  -> AlignInByte 1
      _ -> let aa = case M.lookup (SizeInBit 0) (aggregates dl) of
                 Just (aa, pa) -> selectAlignment at aa pa
                 Nothing -> error ""
           in max (fromAlignInBit aa) (structAlignment $ getStructLayout dl (p,tys))
    Tpointer t a -> case (uncurry lookupOr) (ta a) (pointers dl) of
      Just (s, aa, pa) -> fromAlignInBit $ selectAlignment at aa pa 
      Nothing -> error ""
    Tfunction t tpl fas -> undefined
    Tderef t -> undefined
  where 
    getTpAlignment dl tp = case tp of
      TpI n -> case M.lookup (SizeInBit n) (ints dl) of 
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpF n -> case M.lookup (SizeInBit n) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpV n -> case M.lookup (SizeInBit n) (vectors dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpVoid -> case M.lookup (getPrimitiveTypeSizeInBits dl tp) (ints dl) of 
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpHalf -> undefined 
      TpFloat -> case M.lookup (getPrimitiveTypeSizeInBits dl tp) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpDouble -> case M.lookup (getPrimitiveTypeSizeInBits dl tp) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpFp128 -> case M.lookup (getPrimitiveTypeSizeInBits dl tp) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error ""
      TpX86Fp80 -> case M.lookup (getPrimitiveTypeSizeInBits dl tp) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error "unsupported"
      TpPpcFp128 -> case M.lookup (getPrimitiveTypeSizeInBits dl tp) (floats dl) of
        Just (aa, pa) -> selectAlignment at aa pa
        Nothing -> error "unsupported"
      TpX86Mmx -> error "unsupported" 
      TpNull -> error "unsupported" 
      TpLabel -> error "unsupported"
  
  
ta x = case x of 
  AddrSpace n -> (LayoutAddrSpace n, LayoutAddrSpace n)
  AddrSpaceUnspecified -> (LayoutAddrSpaceUnspecified, LayoutAddrSpace 0)


lookupOr :: Ord a => a -> a -> M.Map a r -> Maybe r          
lookupOr a1 a2 m = maybe (M.lookup a2 m) Just (M.lookup a1 m) 
        
getPrimitiveTypeSizeInBits :: DataLayoutInfo -> TypePrimitive -> SizeInBit
getPrimitiveTypeSizeInBits dl t = case t of
  TpI n -> SizeInBit n
  TpF n -> SizeInBit n
  TpV n -> SizeInBit n
  TpVoid -> SizeInBit eightBits
  TpFloat -> SizeInBit 32
  TpDouble -> SizeInBit 64
  TpFp128 -> SizeInBit 128
  TpX86Fp80 -> SizeInBit 80
  TpPpcFp128 -> SizeInBit 128
  TpLabel -> case lookupOr (LayoutAddrSpaceUnspecified) (LayoutAddrSpace 0) (pointers dl) of
    Just (SizeInBit n, _, _) -> SizeInBit n
    Nothing -> error ""
    
getTypeSizeInBits :: DataLayoutInfo -> Type -> SizeInBit
getTypeSizeInBits dl t = case t of
  Tprimitive tp -> getPrimitiveTypeSizeInBits dl tp
  Tarray i t -> let SizeInBit n = getTypeAllocSizeInBits dl t
                in SizeInBit (i * n)
  Tvector n t -> undefined
  Tstruct pk ts -> toSizeInBit $ structSize (getStructLayout dl (pk, ts))
  Tpointer t a -> case (uncurry lookupOr) (ta a) (pointers dl) of
    Just (n, _, _) -> n
    Nothing -> error ""
  _ -> error "unsupported type"


getTypeStoreSize :: DataLayoutInfo -> Type -> SizeInByte
getTypeStoreSize dl ty = let (SizeInBit tyBits) = getTypeSizeInBits dl ty 
                         in fromSizeInBit (SizeInBit (tyBits + 7))

getTypeStoreSizeInBits :: DataLayoutInfo -> Type -> SizeInBit
getTypeStoreSizeInBits dl ty = toSizeInBit $ getTypeStoreSize dl ty

getTypeAllocSize :: DataLayoutInfo -> Type -> SizeInByte
getTypeAllocSize dl ty = roundUpAlignment (getTypeStoreSize dl ty) (getTypeAlignment dl ty AlignAbi)

getTypeAllocSizeInBits :: DataLayoutInfo -> Type -> SizeInBit
getTypeAllocSizeInBits dl ty = toSizeInBit $ getTypeAllocSize dl ty

data StructLayout = StructLayout { structSize :: SizeInByte
                                 , structAlignment :: AlignInByte
                                 , numElements :: Integer
                                 , memberOffsets :: [OffsetInByte]
                                 } deriving (Eq, Ord, Show)

roundUpAlignment :: SizeInByte -> AlignInByte -> SizeInByte
roundUpAlignment (SizeInByte val) (AlignInByte align) = SizeInByte $ (val + (align -1)) B..&. (B.complement (align - 1)) 

getStructLayout :: DataLayoutInfo -> (Packing, [Type]) -> StructLayout
getStructLayout dl (pk, tys) = let (totalSize@(SizeInByte totalSizeByte), offsets, alignment@(AlignInByte alignmentByte)) = 
                                     foldl (\(curSize@(SizeInByte curSizeByte), offsets, AlignInByte structAlignment0) ty -> 
                                             let tyAlign@(AlignInByte tyAlignByte) = case pk of 
                                                   Packed -> AlignInByte 1 
                                                   Unpacked -> getTypeAlignment dl ty AlignAbi
                                                 (SizeInByte nextOffsetByte) = if curSizeByte B..&. (tyAlignByte - 1) /= 0 then roundUpAlignment curSize tyAlign
                                                                               else curSize
                                                 (SizeInByte tySize) = getTypeAllocSize dl ty
                                             in (SizeInByte $ nextOffsetByte + tySize, (OffsetInByte nextOffsetByte):offsets, AlignInByte $ max tyAlignByte structAlignment0) 
                                           ) (SizeInByte 0, [], AlignInByte 1) tys
                               in StructLayout { structSize = if (totalSizeByte B..&. (alignmentByte - 1)) /= 0 then roundUpAlignment totalSize alignment
                                                              else totalSize
                                               , structAlignment = alignment
                                               , numElements = toInteger $ length tys
                                               , memberOffsets = reverse offsets
                                               }