module Llvm.Hir.Data.Commentor where

import qualified Data.Set as S
import qualified Data.Map as M

class Commentor a where
  commentize :: a -> [String]

newtype Cstring = Cstring String  

instance Commentor Cstring where
  commentize (Cstring s) = lines s

instance Commentor a => Commentor (S.Set a) where
  commentize s = ["Set{"] 
                 ++ (concat $ fmap commentize (S.toList s))
                 ++ ["}"]
                 
                 
instance (Commentor k, Commentor v) => Commentor (M.Map k v) where 
  commentize m = ["Map{"]
                 ++ (concat $ fmap (\(k,v) -> commentize k ++ commentize v) (M.toList m))
                 ++ ["}"]
                 
                 
instance Commentor a => Commentor (Maybe a) where                 
  commentize m = case m of
    Just x -> commentize x
    Nothing -> []