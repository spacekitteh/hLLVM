{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Llvm.VmCore.Converter 
       ( Converter
       , convert
       , maybeM
       , pairM
       ) where

class Converter l1 l2 | l1 -> l2 where
  convert :: l1 -> l2


maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f a = case a of
               Just a' -> f a' >>= return . Just
               Nothing -> return Nothing

pairM :: Monad m => (v -> m v1) -> (l -> m l1) -> (v, l) -> m (v1, l1)
pairM c1 c2 (v, l) = do { v' <- c1 v
                        ; l' <- c2 l
                        ; return $ (v', l')
                        }
