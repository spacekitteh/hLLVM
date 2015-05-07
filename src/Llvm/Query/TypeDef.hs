module Llvm.Query.TypeDef where

import Llvm.Hir.Data
import qualified Data.Map as M
  
getTypeByTname :: String -> M.Map LocalId Dtype -> Maybe Dtype
getTypeByTname tn mp = M.lookup (LocalIdAlphaNum tn) mp

getTypeByTquoteName :: String -> M.Map LocalId Dtype -> Maybe Dtype
getTypeByTquoteName tn mp = M.lookup (LocalIdDqString tn) mp

getTypeByTno :: Word32 -> M.Map LocalId Dtype -> Maybe Dtype
getTypeByTno n mp = M.lookup (LocalIdNum n) mp


