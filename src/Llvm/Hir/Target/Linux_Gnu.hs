{-# LANGUAGE CPP, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Llvm.Hir.Target.Linux_Gnu where

import Llvm.ErrorLoc
#define FLC   (FileLoc $(srcLoc))

import Llvm.Hir.Data.Inst
import Llvm.Hir.Data.Type
import Data.Word (Word8, Word64)
import Data.DoubleWord (Word96)
import Data.Bits(Bits(..), (.&.), (.|.), shiftR)
import Llvm.Hir.DataLayoutMetrics
import qualified Llvm.Asm.Data as A
import qualified Data.Map as M

data I386_Pc_Linux_Gnu = I386_Pc_Linux_Gnu deriving (Show)

{-- e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128 -} 
instance DataLayoutMetrics I386_Pc_Linux_Gnu where
  endianness _ = LittleEndian
  nativeInts _ = [SizeInBit 8, SizeInBit 16, SizeInBit 32]
  integers _ = [SizeInBit 1, SizeInBit 8, SizeInBit 16, SizeInBit 32]
  
  sizeOfPtr _ _ = SizeInBit 32
  alignOfPtr _ _ = AlignMetrics (AlignInBit 32) (Just $ AlignInBit 32)
  
  alignOfStack _ = StackAlign $ AlignInBit 128
  
  alignOfFx _ = M.delete (SizeInBit 128)
                $ M.union (M.fromList [(SizeInBit 64, AlignMetrics (AlignInBit 32) (Just $ AlignInBit 64))
                                      ,(SizeInBit 80, AlignMetrics (AlignInBit 32) Nothing)
                                      ])  floatLayoutMetrics  
  mangling _ = ManglingE

  matchLayoutSpecAndTriple _ dl tt = isI386_Pc_Linux_Gnu dl tt
  toTriple _ = TargetTriple Arch_i386 (Just Vendor_Pc) (Just Os_Linux) (Just OsEnv_Gnu)


isI386_Pc_Linux_Gnu :: [A.LayoutSpec] -> A.TargetTriple  -> Bool
isI386_Pc_Linux_Gnu ls (TargetTriple Arch_i386 (Just Vendor_Pc) (Just Os_Linux) (Just OsEnv_Gnu)) = True


data X86_64_Pc_Linux_Gnu = X86_64_Pc_Linux_Gnu deriving (Eq, Ord, Show)

{- 
  "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
-}
instance DataLayoutMetrics X86_64_Pc_Linux_Gnu where
  endianness _ = LittleEndian
  nativeInts _ = [SizeInBit 8, SizeInBit 16, SizeInBit 32, SizeInBit 64]
  integers _ = [SizeInBit 1, SizeInBit 8, SizeInBit 16, SizeInBit 32, SizeInBit 64]
  
  sizeOfPtr _ _ = SizeInBit 64
  alignOfPtr _ _ = AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64)
  
  alignOfStack _ = StackAlign $ AlignInBit 128
  
  alignOfFx _ = M.union (M.fromList [(SizeInBit 64, AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64))
                                    ,(SizeInBit 80, AlignMetrics (AlignInBit 128) (Just $ AlignInBit 128))
                                    ])  floatLayoutMetrics  
  mangling _ = ManglingE

  matchLayoutSpecAndTriple _ dl tt = isX86_64_Pc_Linux_Gnu dl tt
  toTriple _ = TargetTriple Arch_i386 (Just Vendor_Pc) (Just Os_Linux) (Just OsEnv_Gnu)

isX86_64_Pc_Linux_Gnu :: [A.LayoutSpec] -> A.TargetTriple  -> Bool
isX86_64_Pc_Linux_Gnu ls (TargetTriple Arch_x86_64 (Just Vendor_Pc) (Just Os_Linux) (Just OsEnv_Gnu)) = True
