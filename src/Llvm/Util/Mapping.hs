module Llvm.Util.Mapping where

import qualified Data.Map as M

getValOrImplError :: (Show a, Ord a) => (M.Map a k, String) -> a -> k
getValOrImplError (mp, mn) x = case M.lookup x mp of
  Just s -> s
  Nothing -> error $ "implementation error: " ++ show x ++ " is not added to " ++ mn