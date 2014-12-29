module Llvm.VmCore.DataLayout where

data Endianess = LittleEndian
               | BigEndian deriving (Eq, Ord, Show)

data LayoutAddrSpace = LayoutAddrSpace Integer
                     | LayoutAddrSpaceUnspecified deriving (Eq, Ord, Show)
                                                           
data StackAlign = StackAlign Integer
                | StackAlignUnspecified deriving (Eq, Ord, Show)

data Size = Size Integer deriving (Eq, Ord, Show)

data AbiAlign = AbiAlign Integer deriving (Eq, Ord, Show)

data PrefAlign = PrefAlign Integer deriving (Eq, Ord, Show)

data Mangling = ManglingE
              | ManglingM
              | ManglingO
              | ManglingW deriving (Eq, Ord, Show)
                
data LayoutSpec = DlE Endianess
                | DlS StackAlign
                | DlP LayoutAddrSpace Size AbiAlign (Maybe PrefAlign)
                | DlI Size AbiAlign (Maybe PrefAlign)
                | DlF Size AbiAlign (Maybe PrefAlign)
                | DlV Size AbiAlign (Maybe PrefAlign) 
                | DlA Size AbiAlign (Maybe PrefAlign)
                | DlM Mangling
                | DlN Size Size Size deriving (Eq, Ord, Show)
                
data DataLayout = DataLayout [LayoutSpec] deriving (Eq, Ord, Show)