module Llvm.VmCore.DataLayoutWriter where
import Data.List
import Llvm.VmCore.DataLayout
import Llvm.VmCore.AsmWriter

instance AsmWriter Endianness where
  toLlvm LittleEndian = char 'e'
  toLlvm BigEndian = char 'E'
  
instance AsmWriter LayoutAddrSpace where  
  toLlvm (LayoutAddrSpace n) = integer n
  toLlvm (LayoutAddrSpaceUnspecified) = empty
  
instance AsmWriter SizeInBit where  
  toLlvm (SizeInBit n) = integer n
  
instance AsmWriter AlignInBit where  
  toLlvm (AlignInBit n) = integer n
  
instance AsmWriter StackAlign where  
  toLlvm (StackAlign n) = toLlvm n
  toLlvm StackAlignUnspecified = empty
  
instance AsmWriter Mangling where
  toLlvm x = case x of
    ManglingE -> char 'e'
    ManglingM -> char 'm'
    ManglingO -> char 'o'
    ManglingW -> char 'w'
    
instance AsmWriter AbiAlign where    
  toLlvm (AbiAlign n) = toLlvm n

instance AsmWriter PrefAlign where
  toLlvm (PrefAlign n) = toLlvm n
  
  

instance AsmWriter LayoutSpec where  
  toLlvm ls = case ls of
    DlE x -> toLlvm x
    DlS x -> char 'S' <> (toLlvm x)
    DlP ls s a n -> char 'p' <> (toLlvm ls) 
                    <> char ':' <> (toLlvm s) 
                    <> char ':' <> (toLlvm a) 
                    <> sepOptToLlvmX (\x -> char ':' <> x) n
    DlI s a n -> char 'i' <> (toLlvm s)
                 <> char ':' <> (toLlvm a)
                 <> sepOptToLlvmX (\x -> char ':' <> x) n
    DlF s a n -> char 'f' <> (toLlvm s)
                 <> char ':' <> (toLlvm a)
                 <> sepOptToLlvmX (\x -> char ':' <> x) n
    DlV s a n -> char 'v' <> (toLlvm s)
                 <> char ':' <> (toLlvm a)
                 <> sepOptToLlvmX (\x -> char ':' <> x) n
    DlA s a n -> char 'a' <> (maybe empty toLlvm s)
                 <> char ':' <> (toLlvm a)
                 <> sepOptToLlvmX (\x -> char ':' <> x) n
    DlM m -> char 'm' <> char ':' <> (toLlvm m)
    DlN l -> char 'n' <> (hcat $ punctuate (char ':') $ fmap toLlvm l)
  
instance AsmWriter DataLayout where
  toLlvm (DataLayout l) = doubleQuotes (hcat $ punctuate (char '-') $ fmap toLlvm l)