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


data DataLayoutInfo = DataLayoutInfo 
                      { endianess :: Endianness
                      , stackAlign :: StackAlign
                      , pointers :: M.Map LayoutAddrSpace (SizeInBit, AbiAlign, Maybe PrefAlign)
                      , ints :: M.Map SizeInBit (AbiAlign, Maybe PrefAlign) 
                      , floats :: M.Map SizeInBit (AbiAlign, Maybe PrefAlign) 
                      , vectors :: M.Map SizeInBit (AbiAlign, Maybe PrefAlign)
                      , aggregate :: (AbiAlign, Maybe PrefAlign)
                      , nativeInt :: [SizeInBit]
                      , mangling :: Maybe Mangling
                      } deriving (Eq, Ord, Show)
                                 
defaultDataLayoutInfo :: DataLayoutInfo
defaultDataLayoutInfo = DataLayoutInfo { endianess = error $ show "endian is not specified in Datalayout.hs"
                                       , stackAlign = StackAlignUnspecified
                                       , pointers = M.empty 
                                       , ints = M.fromList [(SizeInBit 1, (AbiAlign $ AlignInBit $ 1*8, Just $ PrefAlign $ AlignInBit $ 1*8))
                                                           ,(SizeInBit 8, (AbiAlign $ AlignInBit $ 1*8, Just $ PrefAlign $ AlignInBit $ 1*8))
                                                           ,(SizeInBit 16, (AbiAlign $ AlignInBit $ 2*8, Just $ PrefAlign $ AlignInBit $ 2*8))
                                                           ,(SizeInBit 32, (AbiAlign $ AlignInBit $ 4*8, Just $ PrefAlign $ AlignInBit $ 4*8))
                                                           ,(SizeInBit 64, (AbiAlign $ AlignInBit $ 4*8, Just $ PrefAlign $ AlignInBit $ 8*8))
                                                           ]
                                       , floats = M.fromList [(SizeInBit 16, (AbiAlign $ AlignInBit $ 2*8, Just $ PrefAlign $ AlignInBit $ 2*8))
                                                             ,(SizeInBit 32, (AbiAlign $ AlignInBit $ 4*8, Just $ PrefAlign $ AlignInBit $ 4*8))
                                                             ,(SizeInBit 64, (AbiAlign $ AlignInBit $ 8*8, Just $ PrefAlign $ AlignInBit $ 8*8))
                                                             ,(SizeInBit 128, (AbiAlign $ AlignInBit $ 16*8, Just $ PrefAlign $ AlignInBit $ 16*8))
                                                             ]
                                       , vectors = M.fromList [(SizeInBit 64, (AbiAlign $ AlignInBit $ 8*8, Just $ PrefAlign $ AlignInBit $ 8*8))
                                                              ,(SizeInBit 128, (AbiAlign $ AlignInBit $ 16*8, Just $ PrefAlign $ AlignInBit $ 16*8))
                                                              ]
                                       , aggregate = (AbiAlign $ AlignInBit 0, Just $ PrefAlign $ AlignInBit $ 8*8)
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
                                                 DlA _ aa pa -> p { aggregate = (aa, pa) }
                                                 DlN l -> p { nativeInt = l }
                                                 DlM m -> p { mangling = Just m }
                                                 DlLittleS _ _ _ -> p
                                                 _ -> error $ "unexpected case " ++ show v
                                          ) defaultDataLayoutInfo ls
