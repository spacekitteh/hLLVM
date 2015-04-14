module Llvm.Pass.PassTester where

import Llvm.Data.Ir
import Llvm.Query.IrCxt
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization m a u x = IrCxt -> a -> H.Label -> H.Graph (Node u) H.C H.C -> m (H.Graph (Node u) H.C H.C, x)
  
opt :: (H.CheckpointMonad m, H.FuelMonad m) => IrCxt -> a -> Optimization m a u x -> Toplevel u -> m [(Toplevel u, (FunctionPrototype, x))]
opt dl gs f (ToplevelDefine (TlDefine fn entry graph)) = do { (graph', x) <- f dl gs entry graph
                                                            ; return [(ToplevelDefine $ TlDefine fn entry graph', (fn, x))]
                                                            }
opt _ _ _ _ = return []


optModule :: (H.CheckpointMonad m, H.FuelMonad m) => Optimization m (Ds.Set (Dtype, GlobalId)) u x -> Module u -> m (Module u, [(FunctionPrototype, x)])
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
      dl = irCxtOfModule (Module l)
  in mapM (opt dl gs f) l >>= \x -> let (lx, ly) = unzip $ concat x
                                 in return (Module lx, ly)

{-
optModule1 :: (H.CheckpointMonad m, H.FuelMonad m) => a -> Optimization m a x -> Module -> m Module
optModule1 a f (Module l) = 
  mapM (opt a f) l >>= return . Module
-}