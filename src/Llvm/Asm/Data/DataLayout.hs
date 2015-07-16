module Llvm.Asm.Data.DataLayout where

import qualified Data.Map as M
import Data.DoubleWord
import Data.Word (Word16, Word32)

data Endianness = LittleEndian
                | BigEndian deriving (Eq, Ord, Show)

data LayoutAddrSpace = LayoutAddrSpace Word32
                     | LayoutAddrSpaceUnspecified deriving (Eq, Ord, Show)
                                                           
data StackAlign = StackAlign AlignInBit
                | StackAlignUnspecified deriving (Eq, Ord, Show)



data SizeInBit = SizeInBit Word96 deriving (Eq, Ord, Show)
data AlignInBit = AlignInBit Word32 deriving (Eq, Ord, Show)

data AlignMetrics = AlignMetrics { abi :: AlignInBit
                                 , pref :: Maybe AlignInBit
                                 } deriving (Eq, Ord, Show)
                    
{-
data AbiAlign = AbiAlign AlignInBit deriving (Eq, Ord, Show)

data PrefAlign = PrefAlign AlignInBit deriving (Eq, Ord, Show)
-}

data Mangling = ManglingE
              | ManglingM
              | ManglingO
              | ManglingW deriving (Eq, Ord, Show)
                
data LayoutSpec = DlE Endianness
                | DlS StackAlign
                | DlLittleS (Maybe Word32) (Maybe Word32) (Maybe Word32)
                | DlP LayoutAddrSpace SizeInBit AlignMetrics 
                | DlI SizeInBit AlignMetrics 
                | DlF SizeInBit AlignMetrics
                | DlV SizeInBit AlignMetrics
                | DlA AlignMetrics
                | DlM Mangling
                | DlN [SizeInBit] deriving (Eq, Ord, Show)
                
data DataLayout = DataLayout [LayoutSpec] deriving (Eq, Ord, Show)


