{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell,ScopedTypeVariables #-}
module Llvm.Pass.CodeGenMonad ( Cc, emitNodes, useBase, appendToBase, emitAll
                              , emitGraph, emitGraph_
                              , newNode, theEnd,newCInst, newPInst, new, newValue
                              , newPhiValue
                              , getLocalBase, newLocalId
                              ) where


import Llvm.Hir.Data
import Llvm.Hir.Mangle
import Llvm.Hir.Print

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative hiding (Const)
import Compiler.Hoopl
import qualified Compiler.Hoopl as H
import qualified Data.Set as S
import Llvm.ErrorLoc

#define FLC (FileLoc $(srcLoc))

{- s: State, r: Reader, e: Error -}
newtype Context s r e a = Ctxt { unCtxt :: StateT s (ReaderT r (ErrorT e Identity)) a }


runContextWithSnR :: Error e => Context s r e a -> s -> r -> Either e (a,s)
runContextWithSnR c s r = runIdentity (runErrorT (runReaderT (runStateT (unCtxt c) s) r))

instance Error e => Functor (Context s r e) where
  fmap f x = Ctxt (fmap f (unCtxt x))

instance Error e => Applicative (Context s r e) where
  pure x = Ctxt $ pure x 
  (<*>) = ap

instance Error e => Monad (Context s r e) where
  return x = Ctxt $ return x 
  (>>=) m k = Ctxt $ unCtxt m >>= \x -> unCtxt $ k x

instance Error e => MonadState s (Context s r e) where  
  get = Ctxt get
  put s = Ctxt $ put s
  
instance Error e => MonadReader r (Context s r e) where                        
  ask = Ctxt ask
  local f m =  Ctxt (local f (unCtxt m))
  

data CodeCache g ad = CodeCache { insts :: [Node g ad O O]
                                , usedLhs :: S.Set Lname
                                } deriving (Eq, Ord, Show)

type Cc g ad a = Context (CodeCache g ad) Lname String a


newLocalId :: Lname -> String -> Lname
newLocalId l suffix = case l of
  Lname n -> Lname (n ++ suffix)

getLocalBase :: Mangle g => g -> Lname
getLocalBase g = Lname $ "@" ++ mangle g

        
baseOf :: (IrPrint g, Mangle g) => Value g -> Lname
baseOf nb = case nb of
  Val_ssa s -> s
  Val_const c -> Lname $ mangle c 

useBase :: (IrPrint g, Mangle g) => Value g -> Cc g ad a -> Cc g ad a
useBase nb cca = local (\_ -> baseOf nb) cca

appendToBase :: String -> Cc g ad a -> Cc g ad a
appendToBase suffix cca = local (\x -> newLocalId x suffix) cca

newCInst :: Cinst g -> Cc g ad ()
newCInst inst = newNode (Cnode inst [])

newPInst :: Pinst g -> Cc g ad ()
newPInst inst = newNode (Pnode inst [])

new :: String -> (Lname -> Node g ad O O) -> Cc g ad Lname
new rhsPrefix partialInst = 
  do { s <- get
     ; bs <- ask
     ; if bs == unspecifiedBase then
         return $ Lname "irrefutable error:the new value base name is not specified"
       else do { lhs <- if rhsPrefix == "" then return $ newLocalId bs "rhsPrefix is an empty string"
                        else let x = newLocalId bs rhsPrefix
                             in if S.member x (usedLhs s) 
                                then return x
                                else modify (\cc@CodeCache{..} -> cc {usedLhs = S.insert x usedLhs}) >> return x 
               ; newNode (partialInst lhs)
               ; return lhs
               }
     }

newValue :: String -> (Lname -> Cinst g) -> Cc g ad (Value g)
newValue rhsPrefix partialInst = liftM Val_ssa (new rhsPrefix (\x -> Cnode (partialInst x) []))

newPhiValue :: String -> (Lname -> Pinst g) -> Cc g ad (Value g)
newPhiValue rhsPrefix partialPhi = liftM Val_ssa (new rhsPrefix (\x -> Pnode (partialPhi x) []))

newNode :: Node g ad O O -> Cc g ad ()
newNode n = modify (\cc@CodeCache{..} -> cc { insts = n:insts }) 

theEnd :: Cc g ad ()
theEnd = return ()

unspecifiedBase :: Lname
unspecifiedBase = Lname "base is not specified"

emitNodes :: Cc g ad a -> [Node g ad O O]
emitNodes cca = fst (emitAll cca)


emitAll :: Cc g ad a -> ([Node g ad O O], a)
emitAll cca = case runContextWithSnR cca (CodeCache [] S.empty) unspecifiedBase of
  Left e -> error (show e)
  Right (a,s) -> (reverse (insts s), a)
  
emitGraph :: Cc g ad a -> (Graph (Node g ad) O O, a)  
emitGraph cca = let (nodes, a) = emitAll cca
                in (mkMiddles nodes, a)

emitGraph_ :: Cc g ad a -> (Graph (Node g ad) O O)
emitGraph_ cca = fst $ emitGraph cca