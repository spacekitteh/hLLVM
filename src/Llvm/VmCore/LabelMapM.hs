{-# OPTIONS_GHC -cpp #-} 
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Llvm.VmCore.LabelMapM 
       (IdLabelMap,addAlist,getAlist,labelFor,labelIdFor,appendH2A,LabelMapM(..),runLabelMapM,emptyIdLabelMap)
       where
import qualified Compiler.Hoopl as H
import qualified Data.Map as M
import qualified Llvm.VmCore.Ast as A 
#ifdef DEBUG
import Debug.Trace
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.AstWriter
#endif 
{-
-- LabelMapM monad is a CheckingFuelMonad with a data structure IdLabelMap to track 
-- the mapping between LLVM labels and Hoopl labels and the original order LLVM labels    
-}

-- type M = H.CheckingFuelMonad (H.SimpleUniqueMonad)

-- type MX = H.SimpleUniqueMonad


{-- this is executed in a lazy and nondeterministic way --}
{-- we have to keep a list of the original order of the labels 
    as llvm-as is sensitive to order, if we cannot keep the original order of blocks, 
    llvm-as might fail
--}
data IdLabelMap = 
  IdLabelMap 
  { -- | mapping LLVM labels to Hoopl labels
    a2h :: M.Map A.Lstring H.Label
    -- | mapping Hoopl labels to LLVM labels
  , h2a :: M.Map H.Label A.Lstring
    -- | keep tracking the orginal ordered LLVM labels of each function
  , alist :: M.Map A.FunctionPrototype [A.Lstring]
  } deriving (Show)
                  
data LabelMapM m a = LabelMapM (IdLabelMap -> m (IdLabelMap, a))

-- | we need to get fresh labels for each unique LLVM label, so we need to use Hoopl Unique Monad
instance H.UniqueMonad m => Monad (LabelMapM m) where
  return x = LabelMapM (\m -> return (m, x))  
  LabelMapM f1 >>= k = LabelMapM (\m -> do { (m', x) <- f1 m
                                           ; let (LabelMapM f2) = k x
                                           ; f2 m'
                                           })
                       
addAlist :: H.UniqueMonad m => A.FunctionPrototype -> [A.Lstring] -> LabelMapM m ()
#ifdef DEBUG
addAlist fn al | trace ("addAlist " ++ toLlvm fn ++ " -> " ++ (show al)) False = undefined
#endif 
addAlist fn al = LabelMapM (\m -> return (m { alist= M.insert fn al (alist m) }, ()))


getAlist :: H.UniqueMonad m => A.FunctionPrototype -> LabelMapM m [A.Lstring]
getAlist fn = LabelMapM f  
  where f m = case M.lookup fn (alist m) of
                Just fn' -> return (m, fn')
                Nothing -> error "irrefutable"
          
appendH2A :: H.UniqueMonad m => LabelMapM m ()
appendH2A = LabelMapM (\m -> return (m{h2a = (h2a m) `M.union`  (invertMap $ a2h m) }, ()))
     where invertMap :: forall a. M.Map a H.Label -> M.Map H.Label a
           invertMap = M.foldlWithKey (\p k b -> M.insert b k p) M.empty

                     
{-- this is executed in a lazy and nondeterministic way --}
{-- so we cannot get the original order of block --}
{-- we will populate alist and h2a in a deterministic way later --}
labelFor :: H.UniqueMonad m => A.Lstring -> LabelMapM m H.Label
labelFor al = LabelMapM f
  where f m = case M.lookup al (a2h m) of
          Just hl -> return (m, hl)
          Nothing -> do { hl <- H.freshLabel
                        ; let a2h' = (M.insert al hl (a2h m))
                        ; return (m {a2h = a2h'}, hl)
                        }

labelIdFor :: H.UniqueMonad m => H.Label -> LabelMapM m A.Lstring
labelIdFor l = LabelMapM f
  where f m = case M.lookup l (h2a m) of
               Just l' -> return (m, l')
               Nothing -> error "this cannot happen"


mapBlockLabel :: A.BlockLabel -> A.BlockLabel
mapBlockLabel x = x
                  {-(Just x) = x
mapBlockLabel Nothing = A.BlockLabel (A.LabelString "")
-}


revertMap :: A.LabelId -> A.BlockLabel
revertMap (A.LabelNumber _) = error "irrefutable" -- A.ImplicitBlockLabel (A.LabelNumber i)
revertMap x = A.ExplicitBlockLabel x
--revertMap (A.LabelString "") = Nothing

emptyIdLabelMap = (IdLabelMap { a2h = M.empty, h2a = M.empty, alist = M.empty})


runLabelMapM :: H.UniqueMonad m => IdLabelMap -> LabelMapM m a -> m (IdLabelMap, a)
runLabelMapM imap (LabelMapM f) = f imap -- 

