{-# LANGUAGE RankNTypes #-}
module Llvm.Pass.Changer where

import Llvm.Hir.Data

data Changer g h = Changer { change_GlobalId :: GlobalId g -> GlobalId h
                           , change_LocalId :: LocalId -> LocalId
                           , change_Label :: Label -> Label
                           , change_Const :: Const h -> Const h
                           } 
               
defaultChanger :: Changer g g
defaultChanger = Changer { change_GlobalId = id 
                         , change_LocalId = id 
                         , change_Label = id 
                         , change_Const = id -- non-recursive version
                         }
