{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Llvm.Control.Context where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State
import Control.Applicative
import Compiler.Hoopl (CheckingFuelMonad, UniqueMonadT, runUniqueMonadT, CheckpointMonad(..))

newtype Context s r a = Ctxt { unCtxt :: UniqueMonadT (StateT s (ReaderT r Identity)) a }

runContextWithSnR :: Context s r a -> s -> r -> a
runContextWithSnR c s r = fst $ runIdentity (runReaderT (runStateT (runUniqueMonadT (unCtxt c)) s) r)

instance Functor (Context s r) where
  fmap f x = Ctxt (fmap f (unCtxt x))

instance Applicative (Context s r) where
  pure x = Ctxt $ pure x 
  (<*>) = ap

instance Monad (Context s r) where
  return x = Ctxt $ return x 
  (>>=) m k = Ctxt $ unCtxt m >>= \x -> unCtxt $ k x

instance MonadReader r (Context s r) where                        
  ask = Ctxt ask
  local f m =  Ctxt (local f (unCtxt m))

instance MonadState s (Context s r) where  
  get = Ctxt get
  put s = Ctxt $ put s


instance CheckpointMonad (Context s r) where
  type Checkpoint (Context s r) = (s, r)
  checkpoint = Ctxt $ liftM2 (,) get ask
  restart (s,r) = Ctxt $ local (\_ -> r) (put s)

type CheckingFuelContextMonad s r = CheckingFuelMonad (Context s r)