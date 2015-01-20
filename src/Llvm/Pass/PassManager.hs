module Llvm.Pass.PassManager where

import Llvm.Data.Ir
import Llvm.Pass.Mem2Reg
import Llvm.Pass.Liveness
import qualified Data.Set as Ds
import Llvm.Pass.Optimizer
import Compiler.Hoopl

data Step = Mem2Reg | Dce deriving (Show,Eq)

toPass :: (CheckpointMonad m, FuelMonad m) => Step -> Optimization m (Ds.Set (Type, GlobalId))
toPass Mem2Reg = mem2reg
toPass Dce = dce

applyPasses1 :: (CheckpointMonad m, FuelMonad m) => [Optimization m (Ds.Set (Type, GlobalId))] -> Module -> m Module
applyPasses1 steps m = foldl (\p e -> p >>= optModule e) (return m) steps


applySteps :: (CheckpointMonad m, FuelMonad m) => [Step] -> Module -> m Module
applySteps steps m = let passes = map toPass steps
                     in applyPasses1 passes m
