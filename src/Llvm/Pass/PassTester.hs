module Llvm.Pass.PassTester where

import Llvm.Hir.Data
import Llvm.Query.HirCxt
import Llvm.Query.Hir
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization m a u x = IrCxt -> a -> H.Label -> H.Graph (Node u) H.C H.C -> m (H.Graph (Node u) H.C H.C, x)
  
opt :: (H.CheckpointMonad m, H.FuelMonad m) => IrCxt -> a -> Optimization m a u x -> Toplevel u -> 
       m [(Toplevel u, (FunctionInterface, x))]
opt dl gs f (ToplevelDefine (TlDefine fn entry graph)) = 
  do { (graph', x) <- f dl gs entry graph
     ; return [(ToplevelDefine $ TlDefine fn entry graph', (fn, x))]
     }
opt _ _ _ _ = return []


optModule :: (H.CheckpointMonad m, H.FuelMonad m) => Optimization m (Ds.Set (Dtype, GlobalId)) u x -> Module u -> 
             m (Module u, [(FunctionInterface, x)])
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
      dl = irCxtOfModule (Module l)
  in mapM (opt dl gs f) l >>= \x -> let (lx, ly) = unzip $ concat x
                                    in return (Module lx, ly)