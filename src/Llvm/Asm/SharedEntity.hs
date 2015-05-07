module Llvm.Asm.SharedEntity 
       (module Llvm.Asm.Data.AtomicEntity
       ,module Llvm.Asm.Data.SimpleConst
       ,module Llvm.Asm.Data.DataLayout
       )
       where
{- 
to avoid duplicattion, these data types can 
be directly referred by Hir 
-}
import Llvm.Asm.Data.AtomicEntity
import Llvm.Asm.Data.SimpleConst
import Llvm.Asm.Data.DataLayout

