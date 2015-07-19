module Llvm.Pass.Optimizer where

import Llvm.Hir.Data
import Llvm.Query.Hir
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization m a g u = a -> H.Label -> H.Graph (Node g u) H.C H.C -> m (H.Graph (Node g u) H.C H.C)
  
opt :: (H.CheckpointMonad m, H.FuelMonad m) => a -> Optimization m a g u -> (Toplevel g u) -> m (Toplevel g u)
opt _ _ (ToplevelAlias s) = return $ ToplevelAlias s
opt _ _ (ToplevelUnamedMd s) = return $ ToplevelUnamedMd s
opt _ _ (ToplevelNamedMd v) = return $ ToplevelNamedMd v
opt _ _ (ToplevelDeclare fn) = return $ ToplevelDeclare fn
opt gs f (ToplevelDefine (TlDefine fn entry graph)) = do { graph' <- f gs entry graph
                                                         ; return $ ToplevelDefine $ TlDefine fn entry graph'
                                                         }
opt _ _ (ToplevelGlobal s) = return $ ToplevelGlobal s
opt _ _ (ToplevelTypeDef a) = return $ ToplevelTypeDef a
opt _ _ (ToplevelDepLibs qs) = return $ ToplevelDepLibs qs
opt _ _ (ToplevelUnamedType i) = return $ ToplevelUnamedType i
opt _ _ (ToplevelModuleAsm s) = return $ ToplevelModuleAsm s
opt _ _ (ToplevelComdat s) = return $ ToplevelComdat s
opt _ _ (ToplevelAttribute s) = return $ ToplevelAttribute s


optModule :: (H.CheckpointMonad m, H.FuelMonad m, Ord g) => Optimization m (Ds.Set (Dtype, GlobalId g)) g u -> Module g u -> m (Module g u)
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
  in mapM (opt gs f) l >>= return . Module

optModule1 :: (H.CheckpointMonad m, H.FuelMonad m) => a -> Optimization m a g u -> Module g u -> m (Module g u)
optModule1 a f (Module l) = 
  mapM (opt a f) l >>= return . Module
