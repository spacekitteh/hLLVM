module Llvm.Pass.PassTester where

import Llvm.Hir.Data
import Llvm.Query.HirCxt
import Llvm.Query.Hir
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization m a g u x = IrCxt g -> a -> H.Label -> H.Graph (Node g u) H.C H.C -> m (H.Graph (Node g u) H.C H.C, x)
  
opt :: (H.CheckpointMonad m, H.FuelMonad m) => IrCxt g -> a -> Optimization m a g u x -> Toplevel g u -> 
       m [(Toplevel g u, (FunctionInterface g, x))]
opt dl gs f (ToplevelDefine (TlDefine fn entry graph)) = 
  do { (graph', x) <- f dl gs entry graph
     ; return [(ToplevelDefine $ TlDefine fn entry graph', (fn, x))]
     }
opt _ _ _ _ = return []


optModule :: (Show g, Ord g, H.CheckpointMonad m, H.FuelMonad m) => Optimization m (Ds.Set (Dtype, GlobalId g)) g u x -> Module g u -> 
             m (Module g u, [(FunctionInterface g, x)])
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
      dl = irCxtOfModule (Module l)
  in mapM (opt dl gs f) l >>= \x -> let (lx, ly) = unzip $ concat x
                                    in return (Module lx, ly)