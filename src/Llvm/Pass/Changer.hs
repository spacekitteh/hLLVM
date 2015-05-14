module Llvm.Pass.Changer where

import Llvm.Hir.Data

data Changer = Changer { change_GlobalId :: GlobalId -> GlobalId
                       , change_LocalId :: LocalId -> LocalId
                       , change_Label :: Label -> Label
                       , change_Const :: Const -> Const
                       } 
               
defaultChanger = Changer { change_GlobalId = id 
                         , change_LocalId = id 
                         , change_Label = id 
                         , change_Const = id -- non-recursive version
                         }