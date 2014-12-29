module Llvm.VmCore.DataLayoutWriter where
import Data.List
import Llvm.VmCore.DataLayout
import Llvm.VmCore.AsmWriter

instance AsmWriter Endianess where
  toLlvm LittleEndian = "e"
  toLlvm BigEndian = "E"
  
instance AsmWriter LayoutAddrSpace where  
  toLlvm (LayoutAddrSpace n) = show n
  toLlvm (LayoutAddrSpaceUnspecified) = ""
  
  

instance AsmWriter DataLayout where
  toLlvm _ = "unimplemented"