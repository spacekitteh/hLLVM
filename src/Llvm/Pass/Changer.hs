{-# LANGUAGE RankNTypes #-}
module Llvm.Pass.Changer where

import Llvm.Hir.Data

data Changer g h = Changer { change_GlobalId :: g -> h
                           , change_LocalId :: Lname -> Lname
                           , change_Label :: Label -> Label
                           , change_Const :: Const h -> Const h
                           , change_Ftype :: Type CodeFunB X -> Type CodeFunB X
                           , change_ParamsForDefine :: [FunOperand Lname] -> [FunOperand Lname]
                           , change_ParamsForDeclare :: [FunOperand ()] -> [FunOperand ()]
                           , change_ParamsForCall :: [FunOperand (Value h)] -> [FunOperand (Value h)]
                           }
               
defaultChanger :: Changer g g
defaultChanger = Changer { change_GlobalId = id 
                         , change_LocalId = id 
                         , change_Label = id 
                         , change_Const = id -- non-recursive version
                         , change_Ftype = id
                         , change_ParamsForDefine = id
                         , change_ParamsForDeclare = id
                         , change_ParamsForCall = id                                                     
                         }