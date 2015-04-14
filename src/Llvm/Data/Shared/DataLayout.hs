module Llvm.Data.Shared.DataLayout where

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


data DataLayoutInfo = DataLayoutInfo 
                      { endianess :: Endianness
                      , stackAlign :: StackAlign
                      , pointers :: M.Map LayoutAddrSpace (SizeInBit, AbiAlign, Maybe PrefAlign)
                      , ints :: M.Map SizeInBit (AbiAlign, Maybe PrefAlign) 
                      , floats :: M.Map SizeInBit (AbiAlign, Maybe PrefAlign) 
                      , vectors :: M.Map SizeInBit (AbiAlign, Maybe PrefAlign)
                      , aggregates :: M.Map (Maybe SizeInBit) (AbiAlign, Maybe PrefAlign)
                      , nativeInt :: [SizeInBit]
                      , mangling :: Maybe Mangling
                      } deriving (Eq, Ord, Show)
                                 
                                 
data AlignType = AlignAbi | AlignPref deriving (Eq, Ord, Show)

selectAlignment :: AlignType -> AbiAlign -> Maybe PrefAlign -> AlignInBit
selectAlignment at (AbiAlign aba) pa = case at of
  AlignAbi -> aba
  AlignPref -> maybe aba (\(PrefAlign n) -> n) pa
                                 

emptyDataLayoutInfo :: DataLayoutInfo
emptyDataLayoutInfo = DataLayoutInfo { endianess = LittleEndian
                                     , stackAlign = StackAlignUnspecified
                                     , pointers = M.empty -- (LayoutAddrSpaceUnspecified, SizeInBit 0, AbiAlign 0, Nothing)
                                     , ints = M.empty
                                     , floats = M.empty
                                     , vectors = M.empty
                                     , aggregates = M.empty
                                     , nativeInt = []
                                     , mangling = Nothing
                                     }
  
getDataLayoutInfo :: DataLayout -> DataLayoutInfo                                 
getDataLayoutInfo (DataLayout ls) = foldl (\p v -> case v of
                                                 DlE x -> p { endianess = x}
                                                 DlS x -> p { stackAlign = x}
                                                 DlP la s aa pa -> p { pointers = M.insert la (s, aa, pa) (pointers p) }
                                                 DlI s aa pa -> p { ints = M.insert s (aa, pa) (ints p) }
                                                 DlF s aa pa -> p { floats = M.insert s (aa, pa) (floats p) }
                                                 DlV s aa pa -> p { vectors = M.insert s (aa, pa) (vectors p) }
                                                 DlA s aa pa -> p { aggregates = M.insert s (aa, pa) (aggregates p) }
                                                 DlN l -> p { nativeInt = l }
                                                 DlM m -> p { mangling = Just m }
                                                 _ -> error $ "unexpected case " ++ show v
                                          ) emptyDataLayoutInfo ls
