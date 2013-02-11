{-# OPTIONS_GHC -cpp #-} 
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Llvm.VmCore.LabelMap where
import qualified Compiler.Hoopl as H
import qualified Data.Map as M
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
#ifdef DEBUG
import Debug.Trace
import Llvm.VmCore.AsmWriter
import Llvm.VmCore.AstWriter
#endif 
--------------------------------------------------------------------------------
-- The LabelMapM monad
--------------------------------------------------------------------------------

{-- this is executed in lazy and not very deterministic way --}
{-- we have to keep a list of the original order of the labels 
    as llvm-as is sensitive to order, if we cannot keep the original order of blocks, 
    llvm-as might fail
--}
data IdLabelMap = IdLabelMap { a2h :: M.Map A.Lstring H.Label
                             , h2a :: M.Map H.Label A.Lstring
                             -- | keep tracking the orginal labels
                             , alist :: M.Map A.FunctionPrototype [A.Lstring]
                             } deriving (Show)
                  
data LabelMapM a = LabelMapM (IdLabelMap -> I.M (IdLabelMap, a))
instance Monad LabelMapM where
  return x = LabelMapM (\m -> return (m, x))  
  LabelMapM f1 >>= k = LabelMapM (\m -> do { (m', x) <- f1 m
                                           ; let (LabelMapM f2) = k x
                                           ; f2 m'
                                           })
                       
addAlist :: A.FunctionPrototype -> [A.Lstring] -> LabelMapM ()
#ifdef DEBUG
addAlist fn al | trace ("addAlist " ++ toLlvm fn ++ " -> " ++ (show al)) False = undefined
#endif 
addAlist fn al = LabelMapM (\m -> return (m { alist= M.insert fn al (alist m) }, ()))
                 
                 
getAlist :: A.FunctionPrototype -> LabelMapM [A.Lstring]
getAlist fn = LabelMapM f  
  where f m = case M.lookup fn (alist m) of
                Just fn' -> return (m, fn')
                Nothing -> error "irrefutable"
          
appendH2A :: LabelMapM ()
appendH2A = LabelMapM (\m -> return (m{h2a = (h2a m) `M.union`  (invertMap $ a2h m) }, ()))
     where invertMap :: forall a. M.Map a H.Label -> M.Map H.Label a
           invertMap = M.foldlWithKey (\p k b -> M.insert b k p) M.empty

                     
{-- this is executed in a lazy and not very deterministic way --}
{-- so we cannot get the original order of block --}
{-- we will populate alist and h2a later in deterministic way later --}
labelFor :: A.Lstring -> LabelMapM H.Label           
labelFor l = LabelMapM f
  where f m = case M.lookup l (a2h m) of
                Just l' -> return (m, l')
                Nothing -> do { l' <- H.freshLabel
                              ; let a2h' = -- traceStack ("fresh label " ++ (show l') ++ " is assigned for " ++ show l) 
                                           (M.insert l l' (a2h m))
                              ; return (m {a2h = a2h'}, l')
                              }
                           
                                                      
labelIdFor :: H.Label -> LabelMapM A.Lstring
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
