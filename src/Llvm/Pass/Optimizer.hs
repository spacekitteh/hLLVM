module Llvm.Pass.Optimizer where

import Llvm.Data.Ir
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization m a = a -> H.Label -> H.Graph Node H.C H.C -> m (H.Graph Node H.C H.C)
  
opt :: (H.CheckpointMonad m, H.FuelMonad m) => a -> Optimization m a -> Toplevel -> m Toplevel
opt _ _ (ToplevelTriple s) = return $ ToplevelTriple s
opt _ _ (ToplevelDataLayout s) = return $ ToplevelDataLayout s
opt _ _ (ToplevelAlias m1 m2 m3 m4 m5 m6 m7) = return $ ToplevelAlias m1 m2 m3 m4 m5 m6 m7
opt _ _ (ToplevelDbgInit s i) = return $ ToplevelDbgInit s i
opt _ _ (ToplevelStandaloneMd s tv) = return $ ToplevelStandaloneMd s tv
opt _ _ (ToplevelNamedMd v ns) = return $ ToplevelNamedMd v ns
opt _ _ (ToplevelDeclare fn) = return $ ToplevelDeclare fn
opt gs f (ToplevelDefine fn entry graph) = do { graph' <- f gs entry graph
                                              ; return $ ToplevelDefine fn entry graph'
                                              }
opt _ _ (ToplevelGlobal lhs lnk vis dll thl ua as exi gt t c s cd a) = 
  return $ ToplevelGlobal lhs lnk vis dll thl ua as exi gt t c s cd a
opt _ _ (ToplevelTypeDef a t) = return $ ToplevelTypeDef a t
opt _ _ (ToplevelDepLibs qs) = return $ ToplevelDepLibs qs
opt _ _ (ToplevelUnamedType i t) = return $ ToplevelUnamedType i t
opt _ _ (ToplevelModuleAsm s) = return $ ToplevelModuleAsm s
opt _ _ (ToplevelComdat l s) = return $ ToplevelComdat l s
opt _ _ (ToplevelAttribute l s) = return $ ToplevelAttribute l s


optModule :: (H.CheckpointMonad m, H.FuelMonad m) => Optimization m (Ds.Set (Type, GlobalId)) -> Module -> m Module
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
  in mapM (opt gs f) l >>= return . Module

optModule1 :: (H.CheckpointMonad m, H.FuelMonad m) => a -> Optimization m a -> Module -> m Module
optModule1 a f (Module l) = 
  mapM (opt a f) l >>= return . Module
