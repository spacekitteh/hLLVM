{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Llvm.Control.Context 
       (module Llvm.Control.Context
       ,module Control.Monad.Reader
       ,module Control.Monad.State
       ,module Control.Monad
       ,module Control.Monad.Error
       ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Compiler.Hoopl (CheckingFuelMonad, UniqueMonadT, runUniqueMonadT, CheckpointMonad(..))

{- s: State, r: Reader, e: Error -}
newtype Context s r e a = Ctxt { unCtxt :: UniqueMonadT (StateT s (ReaderT r (ErrorT e Identity))) a }

runContextWithSnR :: Error e => Context s r e a -> s -> r -> Either e (a,s)
runContextWithSnR c s r = runIdentity (runErrorT (runReaderT (runStateT (runUniqueMonadT (unCtxt c)) s) r))

instance Error e => Functor (Context s r e) where
  fmap f x = Ctxt (fmap f (unCtxt x))

instance Error e => Applicative (Context s r e) where
  pure x = Ctxt $ pure x 
  (<*>) = ap

instance Error e => Monad (Context s r e) where
  return x = Ctxt $ return x 
  (>>=) m k = Ctxt $ unCtxt m >>= \x -> unCtxt $ k x

instance Error e => MonadReader r (Context s r e) where                        
  ask = Ctxt ask
  local f m =  Ctxt (local f (unCtxt m))

instance Error e => MonadState s (Context s r e) where  
  get = Ctxt get
  put s = Ctxt $ put s

instance Error e => CheckpointMonad (Context s r e) where
  type Checkpoint (Context s r e) = (s, r)
  checkpoint = Ctxt $ liftM2 (,) get ask
  restart (s,r) = Ctxt $ local (\_ -> r) (put s)

type CheckingFuelContextMonad s r e = CheckingFuelMonad (Context s r e)