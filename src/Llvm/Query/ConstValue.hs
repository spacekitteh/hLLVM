{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Llvm.Query.ConstValue where

import Llvm.Query.TypeConstValue
-- import Llvm.Native.Support
import Llvm.Data.Shared (Packing(..))
import Llvm.Data.IrType
import Llvm.Data.CoreIr
import Data.Int
import Data.Word
import Data.DoubleWord
import Llvm.Query.Qerror
import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import Prelude (($), undefined, String, (*), fromIntegral, error, Integer,(.))

constI8 :: Word8
constI8  = 0x01

constI16 :: Word16
constI16 = 0x8002

constI32 :: Word32
constI32 = 0x80808003

constI64 :: Word64
constI64 = 0x8080808080808004

constF32 :: Word32
constF32 = 0x80808005 

constF64 :: Word64
constF64 = 0x8080808080808006

constF80 :: Word96
constF80 = 0x8080808080808006

constF128 :: Word128
constF128 = undefined

constB8 :: Word8
constB8  =  0x00

constB16 :: Word16
constB16 =  0x0000

constB32 :: Word32
constB32 = 0x00000000

constB64 :: Word64
constB64 = 0x0000000000000000

constB96 :: Word96
constB96 = 0x0

constB128 :: Word128
constB128 = 0x0

-- __i386__
constGA :: Word32
constGA  =           0x80808010  -- global address 

constSA :: Word32
constSA  =           0x80808020  -- stack address

constLA :: Word32
constLA  =           0x80808030  -- label address

constFA :: Word32
constFA  =           0x80808040  -- function address

constHA :: Word32
constHA  =           0x80808050  -- heap address

constNEP :: Word32
constNEP =           0x7f7f7f00

constIXStr :: [String]
constIXStr = 
  ["0",
   "1",                "3",                "7",                "F",                -- 1-4   bits
   "1F",               "3F",               "7F",               "01",               -- 5-8   bits
   "1FF",              "3FF",              "7FF",              "FFF",              -- 9-12  bits
   "1FFF",             "3FFF",             "7FFF",             "8002",             -- 13-16 bits
   "1FFFF",            "3FFFF",            "7FFFF",            "FFFFF",            -- 17-20 bits
   "1FFFFF",           "3FFFFF",           "7FFFFF",           "FFFFFF",           -- 21-24 bits
   "1FFFFFF",          "3FFFFFF",          "7FFFFFF",          "FFFFFFF",          -- 25-28 bits
   "1FFFFFFF",         "3FFFFFFF",         "7FFFFFFF",         "80808003",         -- 29-32 bits
   "1FFFFFFFF",        "3FFFFFFFF",        "7FFFFFFFF",        "FFFFFFFFF",        -- 33-36 bits
   "1FFFFFFFFF",       "3FFFFFFFFF",       "7FFFFFFFFF",       "FFFFFFFFFF",       -- 33-40 bits
   "1FFFFFFFFFF",      "3FFFFFFFFFF",      "7FFFFFFFFFF",      "FFFFFFFFFFF",      -- 41-44 bits
   "1FFFFFFFFFFF",     "3FFFFFFFFFFF",     "7FFFFFFFFFFF",     "FFFFFFFFFFFF",     -- 45-48 bits
   "1FFFFFFFFFFFF",    "3FFFFFFFFFFFF",    "7FFFFFFFFFFFF",    "FFFFFFFFFFFFF",    -- 49-52 bits
   "1FFFFFFFFFFFFF",   "3FFFFFFFFFFFFF",   "7FFFFFFFFFFFFF",   "FFFFFFFFFFFFFF",   -- 53-56 bits
   "1FFFFFFFFFFFFFF",  "3FFFFFFFFFFFFFF",  "7FFFFFFFFFFFFFF",  "FFFFFFFFFFFFFFF",  -- 57-60 bits
   "1FFFFFFFFFFFFFFF", "3FFFFFFFFFFFFFFF", "7FFFFFFFFFFFFFFF", "8080808080808004"  -- 61-64 bits
  ]
             
constI8Str :: String  
constI8Str  = "01"

constI16Str :: String  
constI16Str = "8002"

constI32Str :: String  
constI32Str = "80808003"

constI64Str :: String  
constI64Str = "8080808080808004"

  -- floating point types
              
constF32Str :: String
constF32Str = "80808005"

constF64Str :: String
constF64Str = "8080808080808006" -- 8

constF80Str :: String
constF80Str = "808080808080808080808007" -- 12

  -- uninitialized types
              
constB8Str :: String
constB8Str =  "00"

constB16Str :: String
constB16Str = "0000"

constB32Str :: String
constB32Str = "00000000"

constB64Str :: String
constB64Str = "0000000000000000"

constNEPStr :: String
constNEPStr = "7F7F7F00"


constINT_BITS = 32
constPTR_BITS = 32
constHSECT_LENGTH = 4 * 2

constHSECT_SIZE_OFFSET :: Int32
constHSECT_SIZE_OFFSET = -4

constHSECT_SYMB_OFFSET :: Int32
constHSECT_SYMB_OFFSET = -8

constGPStr :: String
constGPStr = "80808010"

constGP :: Word32
constGP = 0x80808010

constSPStr :: String
constSPStr = "80808020"

constSP :: Word32
constSP = 0x80808020

constLPStr :: String
constLPStr = "80808030" -- label pointer

constLP :: Word32
constLP = 80808030

constFPStr :: String
constFPStr = "80808040" -- function pointer

constFP :: Word32
constFP = 80808040

{-
const char * memcpyFunName = "llvm.memcpy.p0i8.p0i8.i32";
  static const char * memmoveFunName = "llvm.memmove.p0i8.p0i8.i32";
  static const char * memsetFunName = "llvm.memset.p0i8.p0i8.i32";
-}

uint1Type :: Type ScalarB I
uint1Type = (TpI 1)

uint8Type :: Type ScalarB I
uint8Type = (TpI 8)

uint16Type :: Type ScalarB I
uint16Type = (TpI 16)

uint32Type :: Type ScalarB I
uint32Type = (TpI 32)

uint64Type :: Type ScalarB I
uint64Type = (TpI 64)

uint96Type :: Type ScalarB I
uint96Type = (TpI 96)

uint128Type :: Type ScalarB I
uint128Type = (TpI 128)

uintType :: Type ScalarB I
uintType = (TpI 32)

{--- ---}

getUint1 :: Word8 -> T (Type ScalarB I) Const
getUint1 w = T (TpI 1) (C_u8 w)

getUint8 :: Word8 -> T (Type ScalarB I) Const
getUint8 w = T uint8Type (C_u8 w)

getUint16 :: Word16 -> T (Type ScalarB I) Const
getUint16 w = T uint16Type (C_u16 w)

getUint32 :: Word32 -> T (Type ScalarB I) Const
getUint32 w = T uint32Type (C_u32 w)

getUint64 :: Word64 -> T (Type ScalarB I) Const
getUint64 w = T uint64Type (C_u64 w)

getUint96 :: Word96 -> T (Type ScalarB I) Const
getUint96 w = T uint96Type (C_u96 w)

getUint128 :: Word128 -> T (Type ScalarB I) Const
getUint128 w = T uint128Type (C_u128 w)

{- this one should be generated -}
getUint :: Word32 -> T (Type ScalarB I) Const
getUint w = T uintType (C_u32 w)


getInt8 :: Int8 -> T (Type ScalarB I) Const
getInt8 w = T uint8Type (C_s8 w)

getInt16 :: Int16 -> T (Type ScalarB I) Const
getInt16 w = T uint16Type (C_s16 w)

getInt32 :: Int32 -> T (Type ScalarB I) Const
getInt32 w = T uint32Type (C_s32 w)

getInt64 :: Int64 -> T (Type ScalarB I) Const
getInt64 w = T uint64Type (C_s64 w)

getInt96 :: Int96 -> T (Type ScalarB I) Const
getInt96 w = T uint96Type (C_s96 w)

getInt128 :: Int128 -> T (Type ScalarB I) Const
getInt128 w = T uint128Type (C_s128 w)


constInt8Zero :: T (Type ScalarB I) Const
constInt8Zero = getInt8 0

constInt16Zero :: T (Type ScalarB I) Const
constInt16Zero = getInt16 0

constInt32Zero :: T (Type ScalarB I) Const
constInt32Zero = getInt32 0

constInt64Zero :: T (Type ScalarB I) Const
constInt64Zero = getInt64 0

constPtrZero :: T (Type ScalarB I) Const
constPtrZero = getInt32 0

constSizeZero :: T (Type ScalarB I) Const
constSizeZero = getInt32 0

constInt96Zero :: T (Type ScalarB I) Const
constInt96Zero = getInt96 0

const1False :: T (Type ScalarB I) Const
const1False = getUint1 0

const1True :: T (Type ScalarB I) Const
const1True = getUint1 1


constIntZero :: T (Type ScalarB I) Const
constIntZero = getUint 0

constIntOne :: T (Type ScalarB I) Const
constIntOne = getUint 1

constIntTwo :: T (Type ScalarB I) Const
constIntTwo = getUint 2

constIntThree :: T (Type ScalarB I) Const
constIntThree = getUint 3

constIntFour :: T (Type ScalarB I) Const
constIntFour = getUint 4

constInt32One :: T (Type ScalarB I) Const
constInt32One = getUint32 1

constInt32Two :: T (Type ScalarB I) Const
constInt32Two = getUint32 2

constInt32Four :: T (Type ScalarB I) Const
constInt32Four = getUint32 4

constHsectSizeOffset :: T (Type ScalarB I) Const
constHsectSizeOffset = getInt32 constHSECT_SIZE_OFFSET

constHsectSymbOffset :: T (Type ScalarB I) Const
constHsectSymbOffset = getInt32 constHSECT_SYMB_OFFSET


dtsTypeCodeI8 :: T (Type ScalarB I) Const
dtsTypeCodeI8 = getUint8 constI8

dtsTypeCodeI16 :: T (Type ScalarB I) Const
dtsTypeCodeI16 = getUint16 constI16

dtsTypeCodeI32 :: T (Type ScalarB I) Const
dtsTypeCodeI32 = getUint32 constI32

dtsTypeCodeI64 :: T (Type ScalarB I) Const
dtsTypeCodeI64 = getUint64 constI64

dtsTypeCodeF32 :: T (Type ScalarB I) Const
dtsTypeCodeF32 = getUint32 constF32

dtsTypeCodeF64 :: T (Type ScalarB I) Const
dtsTypeCodeF64 = getUint64 constF64

dtsTypeCodeF80 :: T (Type ScalarB I) Const
dtsTypeCodeF80 = getUint96 constF80

dtsTypeCodeF128 :: T (Type ScalarB I) Const
dtsTypeCodeF128 = getUint128 constF128



dtsTypeCodeGP :: T (Type ScalarB I) Const
dtsTypeCodeGP = getUint constGP

dtsTypeCodeFP :: T (Type ScalarB I) Const
dtsTypeCodeFP = getUint constFP

dtsTypeCodeLP :: T (Type ScalarB I) Const
dtsTypeCodeLP = getUint constLP

dtsTypeCodeB8 :: T (Type ScalarB I) Const
dtsTypeCodeB8 = getUint8 constB8

dtsTypeCodeB16 :: T (Type ScalarB I) Const
dtsTypeCodeB16 = getUint16 constB16

dtsTypeCodeB32 :: T (Type ScalarB I) Const
dtsTypeCodeB32 = getUint32 constB32

dtsTypeCodeB64 :: T (Type ScalarB I) Const
dtsTypeCodeB64 = getUint64 constB64

dtsTypeCodeB80 :: T (Type ScalarB I) Const
dtsTypeCodeB80 = getUint96 constB96

dtsTypeCodeB128 :: T (Type ScalarB I) Const
dtsTypeCodeB128 = getUint128 constB128

getTypeCode :: Dtype -> T Dtype Const
getTypeCode ty = case ty of
  {-
  DtypeRef (Tname _) -> undefined
  DtypeRef (TquoteName _) -> undefined
  DtypeRef (Tno _) -> undefined
  -}
  DtypeRecordD (Tarray n et) -> T ty (C_arrayN n (TypedConst $ getTypeCode et))
--  DtypeVector (Tvector n et) -> TypedData ty (Cca (CvectorN n (getTypeCode et)))
  DtypeRecordD (Tstruct p ts) -> T ty (C_struct p (fmap (TypedConst . getTypeCode) ts))
  -- DtypePtr (Tpointer t as) -> ucast dtsTypeCodeGP
  DtypeScalarI (TpI n) -> ucast (case n of
                                    8 -> dtsTypeCodeI8
                                    16 -> dtsTypeCodeI16
                                    32 -> dtsTypeCodeI32
                                    64 -> dtsTypeCodeI64)
  DtypeScalarF (TpF n) -> ucast (case n of
                                    32 -> dtsTypeCodeF32
                                    64 -> dtsTypeCodeF64
                                    80 -> dtsTypeCodeF80
                                    128 -> dtsTypeCodeF128)
  DtypeScalarP _ -> ucast dtsTypeCodeI32


getBaseAddr :: Map (T Dtype Value) (T Dtype Value) -> T Dtype Value -> T Dtype Value
getBaseAddr mp tv@(T ty v) = case (ty, v) of
  (DtypeRecordD (Tarray n et), av) -> undefined 
  (DtypeRecordD (Tstruct p ts), sv) -> undefined
  (DtypeScalarI (TpI n),_) -> let x = case n of
                                    8 -> dtsTypeCodeB8
                                    16 -> dtsTypeCodeB16
                                    32 -> dtsTypeCodeB32
                                    64 -> dtsTypeCodeB64
                              in ucast x
  (DtypeScalarF (TpF n),_) -> let x = case n of
                                    32 -> dtsTypeCodeB32
                                    64 -> dtsTypeCodeB64
                                    80 -> dtsTypeCodeB80
                                    128 -> dtsTypeCodeB128
                              in ucast x
  (DtypeScalarP (Tpointer t as), _) -> fromJust $ lookup tv mp
            
            


getIntConst :: Type ScalarB I -> Integer -> T (Type ScalarB I) Const
getIntConst ((TpI n)) it = case n of
  8 -> getInt8 $ fromIntegral it 
getIntConst x _ = error "getIntConst's first parameter is not an integer type"  