{-# LANGUAGE FlexibleContexts #-}
module Llvm.Query.Translucent where

import Llvm.Data.Shared 
import Llvm.Query.Qerror

class VectorTranslucent a where
  vmap :: (a -> b) -> a -> b
  isVector :: a -> Bool
  vsize :: a -> Maybe Integer
  stripOffVector :: a -> Maybe a

instance VectorTranslucent Type where
  vmap f x = case x of
    Tvector _ ex -> f ex
    _ -> f x
  isVector x = case x of
    Tvector _ _ -> True
    _ -> False
  vsize x = case x of
    Tprimitive _ -> Nothing
    Tvector n _ -> Just n
  stripOffVector x = case x of
    Tvector _ et -> return et
    _ -> return x
  
class UndefTranslucent a where
  umap :: (a -> b) -> a -> b
  isUndef :: a -> Bool
  
