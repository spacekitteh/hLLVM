{-# LANGUAGE EmptyDataDecls #-}
module Llvm.VmCore.CodeAttribute
       ( module Llvm.VmCore.AtomicEntity
       , module Llvm.VmCore.DataLayout
       ) where

import Llvm.VmCore.AtomicEntity
import Llvm.VmCore.DataLayout

data LoadTag 
data AllocaTag
data StoreTag
data CompxchgTag
data AtomicrmwTag
  

data AllocaAttribute = AllocaAttribute (IsOrIsNot InAllocaAttr) (Maybe Align) deriving (Eq, Ord, Show)

data LoadAttribute = LoadAttribute Atomicity (IsOrIsNot Volatile) (Maybe Align) deriving (Eq, Ord, Show)

data StoreAttribute = StoreAttribute Atomicity (IsOrIsNot Volatile) (Maybe Align) deriving (Eq, Ord, Show)

data CmpxchgAttribute = CmpxchgAttribute (IsOrIsNot Weak) (IsOrIsNot Volatile) (IsOrIsNot SingleThread) deriving (Eq, Ord, Show)

data AtomicrmwAttribute = AtomicrmwAttribute (IsOrIsNot Volatile) (IsOrIsNot SingleThread) deriving (Eq, Ord, Show)