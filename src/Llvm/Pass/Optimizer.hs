{-# OPTIONS_GHC -Wall #-}
module Llvm.Pass.Optimizer where

import Llvm.VmCore.Ir
import Llvm.VmCore.LabelMapM 
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization a = a -> H.Label -> H.Graph Node H.C H.C -> M (H.Graph Node H.C H.C)
  
opt :: a -> Optimization a -> Toplevel -> M Toplevel
opt _ _ (ToplevelTarget k s) = return $ ToplevelTarget k s
opt _ _ (ToplevelAlias m1 m2 m3 a) = return $ ToplevelAlias m1 m2 m3 a
opt _ _ (ToplevelDbgInit s i) = return $ ToplevelDbgInit s i
opt _ _ (ToplevelStandaloneMd s tv) = return $ ToplevelStandaloneMd s tv
opt _ _ (ToplevelNamedMd v ns) = return $ ToplevelNamedMd v ns
opt _ _ (ToplevelDeclare fn) = return $ ToplevelDeclare fn
opt gs f (ToplevelDefine fn entry graph) = do { graph' <- f gs entry graph
                                              ; return $ ToplevelDefine fn entry graph'
                                              }
opt _ _ (ToplevelGlobal lhs lnk vis thl ua as gt t c s a) = 
  return $ ToplevelGlobal lhs lnk vis thl ua as gt t c s a
opt _ _ (ToplevelTypeDef a t) = return $ ToplevelTypeDef a t
opt _ _ (ToplevelDepLibs qs) = return $ ToplevelDepLibs qs
opt _ _ (ToplevelUnamedType i t) = return $ ToplevelUnamedType i t
opt _ _ (ToplevelModuleAsm s) = return $ ToplevelModuleAsm s


optModule :: Optimization (Ds.Set (Type, GlobalId)) -> Module -> M Module
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
  in
   mapM (opt gs f) l >>= return . Module
   
   
   
optModule1 :: a -> Optimization a -> Module -> M Module
optModule1 a f (Module l) = 
  mapM (opt a f) l >>= return . Module