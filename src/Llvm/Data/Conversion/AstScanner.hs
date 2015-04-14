module Llvm.Data.Conversion.AstScanner where

import Llvm.Data.Ast

typeDefOfModule :: Module -> [(LocalId, Type)]
typeDefOfModule (Module tl) = 
  let tl0 = filter (\x -> case x of
                       ToplevelTypeDef _ -> True
                       _ -> False
                   ) tl
  in fmap (\(ToplevelTypeDef (TlTypeDef lid ty)) -> (lid,ty)) tl0
     
     
     