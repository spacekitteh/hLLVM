{-# OPTIONS_GHC -Wall #-}
module Llvm.Pass.Optimizer where

import Llvm.Data.Ir
import qualified Compiler.Hoopl as H 
import qualified Data.Set as Ds

type Optimization a = a -> H.Label -> H.Graph Node H.C H.C -> H.CheckingFuelMonad H.SimpleUniqueMonad (H.Graph Node H.C H.C)
  
opt :: a -> Optimization a -> Toplevel -> H.CheckingFuelMonad H.SimpleUniqueMonad Toplevel
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



optModule :: Optimization (Ds.Set (Type, GlobalId)) -> Module -> H.CheckingFuelMonad H.SimpleUniqueMonad Module
optModule f (Module l) = 
  let gs = globalIdOfModule (Module l)
  in
   mapM (opt gs f) l >>= return . Module


optModule1 :: a -> Optimization a -> Module -> H.CheckingFuelMonad H.SimpleUniqueMonad Module
optModule1 a f (Module l) = 
  mapM (opt a f) l >>= return . Module
