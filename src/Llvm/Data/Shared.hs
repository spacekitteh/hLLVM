module Llvm.Data.Shared 
       (module Llvm.Data.Shared.AtomicEntity
       ,module Llvm.Data.Shared.SimpleConst
       ,module Llvm.Data.Shared.DataLayout
       ,module Llvm.Data.Shared.Util
       )
       where

import Llvm.Data.Shared.AtomicEntity
import Llvm.Data.Shared.SimpleConst
-- import Llvm.Data.Shared.Type
import Llvm.Data.Shared.DataLayout
import Llvm.Data.Shared.Util



{-
data Typed v where
  TypedData :: Type -> v -> Typed v 
  UntypedNull :: Typed v -- Const
-}
