module Llvm.Asm.Data.Default where

import Llvm.Asm.Data.DataLayout
import Llvm.Asm.Data.AtomicEntity

{- E-m:e-p:64:64:64-S0-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a:0:64 -}
defaultDataLayout :: DataLayout
defaultDataLayout = DataLayout [DlE BigEndian
                               ,DlM ManglingE
                               ,DlP LayoutAddrSpaceUnspecified (SizeInBit 64) (AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64))
                               ,DlS StackAlignUnspecified
                               ,DlI (SizeInBit 1) (AlignMetrics (AlignInBit 8) (Just $ AlignInBit 8))
                               ,DlI (SizeInBit 8) (AlignMetrics (AlignInBit 8) (Just $ AlignInBit 8))
                               ,DlI (SizeInBit 16) (AlignMetrics (AlignInBit 16) (Just $ AlignInBit 16))
                               ,DlI (SizeInBit 32) (AlignMetrics (AlignInBit 32) (Just $ AlignInBit 32))
                               ,DlI (SizeInBit 64) (AlignMetrics (AlignInBit 32) (Just $ AlignInBit 64))
                               ,DlF (SizeInBit 16) (AlignMetrics (AlignInBit 16) (Just $ AlignInBit 16))
                               ,DlF (SizeInBit 32) (AlignMetrics (AlignInBit 32) (Just $ AlignInBit 32))
                               ,DlF (SizeInBit 64) (AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64))
                               ,DlF (SizeInBit 128) (AlignMetrics (AlignInBit 128) (Just $ AlignInBit 128))
                               ,DlV (SizeInBit 64) (AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64))
                               ,DlV (SizeInBit 128) (AlignMetrics (AlignInBit 128) (Just $ AlignInBit 128))
                               ,DlA (AlignMetrics (AlignInBit 1) (Just $ AlignInBit 64))
                               ]
{- i386_pc_linux_gnu -}
defaultTargetTriple :: TargetTriple
defaultTargetTriple = TargetTriple Arch_i386 (Just Vendor_Unknown) (Just Os_Linux) (Just OsEnv_Gnu)