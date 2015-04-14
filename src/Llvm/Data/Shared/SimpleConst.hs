module Llvm.Data.Shared.SimpleConst 
       (module Llvm.Data.Shared.SimpleConst
       ,module Llvm.Data.Shared.AtomicEntity
       )
       where

import Llvm.Data.Shared.AtomicEntity (GlobalId)
import Data.Int
import Data.Word
import Data.DoubleWord

-- | Simple Constants <http://llvm.org/releases/3.0/docs/LangRef.html#simpleconstants>  
data SimpleConstant = CpInt String
                    | CpUhexInt String
                    | CpShexInt String
                    | CpFloat String
                    -- | null pointer constant
                    | CpNull
                    | CpUndef
                    -- | true::i1 
                    | CpTrue
                    -- | false::i1
                    | CpFalse
                    | CpZeroInitializer
                    | CpGlobalAddr GlobalId
                    | CpStr String
                    | CpBconst BinaryConstant
                    deriving (Eq, Ord, Show)

data BinaryConstant = BconstUint8 Word8
                    | BconstUint16 Word16
                    | BconstUint32 Word32
                    | BconstUint64 Word64
                    | BconstUint96 Word96
                    | BconstUint128 Word128
                    | BconstInt8 Int8
                    | BconstInt16 Int16
                    | BconstInt32 Int32
                    | BconstInt64 Int64
                    | BconstInt96 Int96
                    | BconstInt128 Int128
                    deriving (Eq, Ord, Show)