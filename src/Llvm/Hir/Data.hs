module Llvm.Hir.Data (module Llvm.Hir.Data.Module
                     ,module Llvm.Hir.Data.Type
                     ,module Llvm.Hir.Data.Inst
                     ,module Llvm.Hir.Data.Commentor
                     ,module Llvm.Hir.Data
                     ) where

import Llvm.Hir.Data.Module
import Llvm.Hir.Data.Type
import Llvm.Hir.Data.Inst
import Llvm.Hir.Data.Commentor

{- 
  SpecializedModule dlm g a 
                     ^  ^ ^
                     |  |  +- specialized Cnode, it's customized by a consumer
                     |  +---- specialized GlobalId, it's customized by a consumer
                     +------- an instance of DataLayoutMetrics to specify 
                              target datalayout and triple 

  For a consumer that does not demand specializing Cnode and GlobalId,
  then SpecializedModule dlm () () is a typical instance, and dlm 
  is a target datalayout and target triple instance. 

  Sorry for such a long type instance with multiple type instantiations, but I 
  haven't found a better way to hide them for the default instance. 
-}
data SpecializedModule dlm g a = SpecializedModule dlm (Module g a)