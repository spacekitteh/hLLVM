module Llvm.Query.Hir where

import Llvm.Hir.Data
import qualified Data.Set as S

-- this should be a map, globalid might have an opaque type
globalIdOfModule :: (Module a) -> S.Set (Dtype, GlobalId) 
globalIdOfModule (Module tl) = foldl (\a b -> S.union a (globalIdOf b)) S.empty tl
  where globalIdOf (ToplevelGlobal (TlGlobalDtype lhs _ _ _ _ _ _ _ _ t _ _ _ _)) = S.singleton (t, lhs)
        globalIdOf _ = S.empty


{- a transformation might add globals to a module, LLVM does not allow duplicate global 
   declarations. This function finds out what are not declared. 
-}
{-
selectUndeclaredTlGlobals :: Module a -> [TlGlobal] -> [TlGlobal]
selectUndeclaredTlGlobals (Module l) = 
  let s = S.foldl (\p e -> case e of
                      ToplevelGlobal (TlGlobalDtype{..}) -> maybe p (flip S.insert p) tlg_lhs
                      ToplevelGlobal (TlGlobalOpaque{..}) -> maybe p (flip S.insert p) tlg_lhs   
                      _ -> p
                  ) S.empty l
  in filter (\x -> case x of
                TlGlobalDtype{..} -> maybe 
-}
