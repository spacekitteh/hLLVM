{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Llvm.Query.Conversion where
import Llvm.Data.CoreIr
import Llvm.Query.Qerror
import Llvm.Data.IrType
import Debug.Trace
import Data.Int


strToApInt :: String -> Integer
-- strToApInt s | trace ("parsing " ++ show s ++ " as int") False = undefined
strToApInt s = case ((reads s)::[(Integer,String)]) of
  [(n,"")] -> n
  _ -> error $ "is not integer " ++ s

castTypedValueToTypedConst :: Show t => T t Value -> T t Const
castTypedValueToTypedConst (T t (Val_const c)) = T t c
castTypedValueToTypedConst x = error $ "is not const " ++ show x

castTypedConstToTypedValue :: T t Const -> T t Value  
castTypedConstToTypedValue (T t c) = T t (Val_const c) 


zExtTo32or64 :: MonadError Qerror m => Integer -> m Integer
zExtTo32or64 n = return n -- FIXME n should be less then or equal to uint64_t

castToStructType :: MonadError Qerror m => Type x r -> m (Packing, [Dtype])
castToStructType (Tstruct p ts) = return (p, ts)
castToStructType x = throwError (QerrWithInfo $ (show x) ++ " is not a struct type")

getUniqueInteger :: Show t => T t Const -> Integer
getUniqueInteger (T _ (C_int s))  = strToApInt s
getUniqueInteger (T _ (C_u8 s)) = fromIntegral s
getUniqueInteger (T _ (C_u16 s)) = fromIntegral s
getUniqueInteger (T _ (C_u32 s)) = fromIntegral s
getUniqueInteger (T _ (C_s8 s)) = fromIntegral s
getUniqueInteger (T _ (C_s16 s)) = fromIntegral s
getUniqueInteger (T _ (C_s32 s)) = fromIntegral s
getUniqueInteger x = error $ "is not integer " ++ show x


type ExponentType = Int 
data FloatingSemantics = FloatingSemantics { maxExponent :: ExponentType  
                                           , minExponent :: ExponentType
                                           , precision :: Integer
                                           } deriving (Eq, Ord, Show)

ieeeHalf = FloatingSemantics 15 (-14) 11
ieeeSingle = FloatingSemantics 127 (-126) 24
ieeeDouble = FloatingSemantics 1023 (-1022) 53
ieeeQuad = FloatingSemantics 16383 (-16382) 113
x87DoubleExtended = FloatingSemantics 16383 (-16382) 64
pPcDoubleDouble = FloatingSemantics 1023 (-1022+53) (53+53)
bogus = FloatingSemantics 0 0 0

data InMemRep  = InMemRep (Type ScalarB I) SimpleConstant deriving (Eq, Ord, Show)
{-
castPointerToValue :: Typed Pointer -> Typed Value
castPointerToValue (Typed (Tpointer _) (Pointer v)) = Ec (Conversion 
-}


class Cvalue x where
  toConst :: x -> Const
  
instance Cvalue Int32 where
  toConst = C_s32
  
instance Cvalue Int64 where
  toConst = C_s64

instance Cvalue Word32 where  
  toConst = C_u32


class Rvalue x where
  toRvalue :: x -> Value

instance Rvalue LocalId where
  toRvalue = Val_ssa

instance Rvalue GlobalId where  
  toRvalue = Val_const . C_globalAddr

instance Rvalue Const where
  toRvalue = Val_const 

instance Rvalue Int32 where
  toRvalue = Val_const . C_s32 
  
instance Rvalue Word32 where
  toRvalue = Val_const . C_u32


class TC x where
  toTC :: x -> T (Type ScalarB I) Const

instance TC Word32 where
  toTC x = T i32 (toConst x)

instance TC Int32 where
  toTC x = T i32 (toConst x)


class TV x where
  toTV :: x -> T (Type ScalarB I) Value
  

instance TV Word32 where
  toTV x = T i32 (toRvalue x)

instance TV Int32 where
  toTV x = T i32 (toRvalue x)
  
toC :: FileLoc -> Value -> Const
toC flc v = case v of
  Val_const c -> c
  Val_ssa _ -> errorLoc flc $ "cast " ++ show v ++ " to Const"

toTVs :: TV x => [x] -> [T (Type ScalarB I) Value]   
toTVs l = fmap toTV l


toTCs :: TC x => [x] -> [T (Type ScalarB I) Const]   
toTCs l = fmap toTC l

i32sToTvs :: [Int32] -> [T (Type ScalarB I) Value]
i32sToTvs = toTVs
  
i32sToTcs :: [Int32] -> [T (Type ScalarB I) Const]
i32sToTcs = toTCs

u32sToTvs :: [Word32] -> [T (Type ScalarB I) Value]
u32sToTvs = toTVs

u32sToTcs :: [Word32] -> [T (Type ScalarB I) Const]
u32sToTcs = toTCs

i32ToTv :: Int32 -> T (Type ScalarB I) Value
i32ToTv = toTV
            
u32ToTv :: Word32 -> T (Type ScalarB I) Value            
u32ToTv = toTV
