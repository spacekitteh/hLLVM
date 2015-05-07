{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Llvm.Pass.CodeGenMonad ( Cc, emitNodes, useBase, appendToBase, emitAll
                              , newGlobalId, newNode, theEnd,newCInst, new, newValue
                              , getLocalBase, findGlobalAddr, newLocalId
                              ) where


import Llvm.Hir.Data
import Llvm.Hir.Mangle

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative hiding (Const)
import Compiler.Hoopl
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

data CodeCache ad = CodeCache { insts :: [Node ad O O]
                              , usedLhs :: S.Set LocalId
                              } deriving (Eq, Ord, Show)

type Cc ad a = Context (CodeCache ad) LocalId String a

{-
mkNode :: Cinst -> Node ad O O
mkNode c = Cnode c
-}

newLocalId :: LocalId -> String -> LocalId
newLocalId l suffix = case l of
  LocalIdNum n -> LocalIdDqString ((show n) ++ suffix)
  LocalIdAlphaNum s -> LocalIdDqString (s ++ suffix)
  LocalIdDqString s -> LocalIdDqString (s ++ suffix)

newGlobalId :: GlobalId -> String -> GlobalId
newGlobalId l suffix = case l of
  GlobalIdNum n -> GlobalIdDqString ((show n) ++ suffix)
  GlobalIdAlphaNum s -> GlobalIdDqString (s ++ suffix)
  GlobalIdDqString s -> GlobalIdDqString (s ++ suffix)

getLocalBase :: GlobalId -> LocalId
getLocalBase g = case g of 
  GlobalIdNum n -> LocalIdDqString $ "@" ++ show n
  GlobalIdAlphaNum s -> LocalIdDqString $ "@" ++ s
  GlobalIdDqString s -> LocalIdDqString $ "@" ++ s
        
findGlobalAddr :: Const -> Maybe (GlobalId, Const -> Const)
findGlobalAddr cnst = case cnst of
  C_u8 _ -> Nothing
  C_u16 _ -> Nothing
  C_u32 _ -> Nothing
  C_u64 _ -> Nothing
  C_u96 _ -> Nothing
  C_u128 _ -> Nothing
  C_s8 _ -> Nothing
  C_s16 _ -> Nothing
  C_s32 _ -> Nothing
  C_s64 _ -> Nothing
  C_s96 _ -> Nothing
  C_s128 _ -> Nothing
  C_int _ -> Nothing
  C_uhex_int _ -> Nothing
  C_shex_int _ -> Nothing
  C_float _ -> Nothing
  C_null -> Nothing
  C_undef -> Nothing
  C_true -> Nothing
  C_false -> Nothing
  C_zeroinitializer -> Nothing
  C_globalAddr g -> Just (g, id)
  C_getelementptr b (T t c) idx -> case findGlobalAddr c of
    Nothing -> Nothing
    Just (gid, f) -> Just (gid, \x -> C_getelementptr b (T t (f x)) idx)
  C_ptrtoint (T st c) dt -> case findGlobalAddr c of
    Nothing -> Nothing
    Just (gid, f) -> Just (gid, \x -> C_ptrtoint (T st (f x)) dt)
  C_str _ -> errorLoc FLC ("unsupported " ++ show cnst)
  C_struct _ _ -> errorLoc FLC ("unsupported " ++ show cnst)  
  C_vector _ -> errorLoc FLC ("unsupported " ++ show cnst)    
  C_vectorN _ _-> errorLoc FLC ("unsupported " ++ show cnst)      
  C_array _ -> errorLoc FLC ("unsupported " ++ show cnst)        
  C_arrayN _ _ -> errorLoc FLC ("unsupported " ++ show cnst)  
--   C_localId _ -> errorLoc FLC ("unsupported " ++ show cnst)    
  C_labelId _ -> errorLoc FLC ("unsupported " ++ show cnst)
  C_block _ _ -> errorLoc FLC ("unsupported " ++ show cnst)
  _ -> Nothing
                        
       
baseOf :: Value -> LocalId       
baseOf nb = case nb of
  Val_ssa s -> s
  Val_const c -> LocalIdDqString $ mangle c 
                 {- case findGlobalAddr c of
                 Just (g,_) -> getLocalBase g
    Nothing -> LocalIdDqString $ "cannot find global addr of " ++ show nb
-}

useBase :: Value -> Cc ad a -> Cc ad a
useBase nb cca = local (\_ -> baseOf nb) cca

appendToBase :: String -> Cc ad a -> Cc ad a
appendToBase suffix cca = local (\x -> newLocalId x suffix) cca

newCInst :: Cinst -> Cc ad ()
newCInst inst = modify (\cc@CodeCache{..} -> cc { insts = (Cnode inst []):insts })

new :: String -> (LocalId -> Cinst) -> Cc ad LocalId
new rhsPrefix partialInst = 
  do { s <- get
     ; bs <- ask
     ; lhs <- if rhsPrefix == "" then undefined
              else let x = newLocalId bs rhsPrefix
                   in if S.member x (usedLhs s) 
                      then error $ (show x) ++ " is already defined in the current computation with the base: " ++ show bs
                      else modify (\cc@CodeCache{..} -> cc {usedLhs = S.insert x usedLhs}) >> return x 
     ; modify (\cc@CodeCache{..} -> cc { insts = (Cnode (partialInst lhs) []):insts})
     ; return lhs
     }

nativeNewValue :: String -> (LocalId -> Cinst) -> Cc ad Value
nativeNewValue rhsPrefix partialInst  = liftM Val_ssa (new rhsPrefix partialInst)
  
newValue :: String -> (LocalId -> Cinst) -> Cc ad Value
newValue rhsPrefix partialInst = liftM Val_ssa (new rhsPrefix partialInst)

newNode :: Node ad O O -> Cc ad ()
newNode n = modify (\cc@CodeCache{..} -> cc { insts = n:insts }) 

theEnd :: Cc ad ()
theEnd = return ()


emitNodes :: Cc ad a -> [Node ad O O]
emitNodes cca = fst (emitAll cca)

emitAll :: Cc ad a -> ([Node ad O O], a)
emitAll cca = case runContextWithSnR cca (CodeCache [] S.empty) (LocalIdDqString "base is not specified") of
  Left e -> error (show e)
  Right (a,s) -> (reverse (insts s), a)
