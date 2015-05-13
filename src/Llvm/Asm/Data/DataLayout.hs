module Llvm.Asm.Data.DataLayout where

import qualified Data.Map as M
import Data.Word

data Endianness = LittleEndian
                | BigEndian deriving (Eq, Ord, Show)

data LayoutAddrSpace = LayoutAddrSpace Word32
                     | LayoutAddrSpaceUnspecified deriving (Eq, Ord, Show)
                                                           
data StackAlign = StackAlign AlignInBit
                | StackAlignUnspecified deriving (Eq, Ord, Show)

data SizeInBit = SizeInBit Word32 deriving (Eq, Ord, Show)
data AlignInBit = AlignInBit Word32 deriving (Eq, Ord, Show)

data AbiAlign = AbiAlign AlignInBit deriving (Eq, Ord, Show)

data PrefAlign = PrefAlign AlignInBit deriving (Eq, Ord, Show)

data Mangling = ManglingE
              | ManglingM
              | ManglingO
              | ManglingW deriving (Eq, Ord, Show)
                
data LayoutSpec = DlE Endianness
                | DlS StackAlign
                | DlLittleS (Maybe Word32) (Maybe Word32) (Maybe Word32)
                | DlP LayoutAddrSpace SizeInBit AbiAlign (Maybe PrefAlign)
                | DlI SizeInBit AbiAlign (Maybe PrefAlign)
                | DlF SizeInBit AbiAlign (Maybe PrefAlign)
                | DlV SizeInBit AbiAlign (Maybe PrefAlign) 
                | DlA (Maybe SizeInBit) AbiAlign (Maybe PrefAlign)
                | DlM Mangling
                | DlN [SizeInBit] deriving (Eq, Ord, Show)
                
data DataLayout = DataLayout [LayoutSpec] deriving (Eq, Ord, Show)


