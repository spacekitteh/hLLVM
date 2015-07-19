module Llvm.Pass.PassManager where

import Llvm.Hir.Data
import Llvm.Pass.Mem2Reg
import Llvm.Pass.Liveness
import qualified Data.Set as Ds
import Llvm.Pass.Optimizer
import Compiler.Hoopl

data Step = Mem2Reg | Dce 
          | PrepareRw
          deriving (Show,Eq)

toPass :: (CheckpointMonad m, FuelMonad m) => Step -> Optimization m (Ds.Set (Dtype, GlobalId g)) g a
toPass Mem2Reg = undefined -- mem2reg
toPass Dce = dce

applyPasses1 :: (CheckpointMonad m, FuelMonad m) => [Optimization m (Ds.Set (Dtype, GlobalId g)) g a] -> Module g a -> m (Module g a)
applyPasses1 steps m = undefined -- foldl (\p e -> p >>= optModule e) (return m) steps


applySteps :: (CheckpointMonad m, FuelMonad m) => [Step] -> Module g a -> m (Module g a)
applySteps steps m = let passes = map toPass steps
                     in applyPasses1 passes m
