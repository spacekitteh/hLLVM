module Llvm.Pass.Changer where

import Llvm.Data.CoreIr

data Changer = Changer { change_GlobalId :: GlobalId -> GlobalId
                       , change_LocalId :: LocalId -> LocalId
                       , change_Label :: Label -> Label
                       , change_Const :: Const -> Const
                       , change_TypedConstOrNull :: TypedConstOrNull -> TypedConstOrNull
                       } 
               
defaultChanger = Changer { change_GlobalId = id 
                         , change_LocalId = id 
                         , change_Label = id 
                         , change_Const = id -- no recursive version
                         , change_TypedConstOrNull = id -- no recursive version  
                         }