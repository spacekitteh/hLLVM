module Llvm.Query.Qerror 
       (module Llvm.Query.Qerror
       ,module Control.Monad.Error
       ) where
import Control.Monad.Error

data Qerror = QerrIsNotInteger String
            | QerrIsNotConst String
            | QerrWithoutInfo
            | QerrWithInfo String
            deriving (Eq, Ord, Show)

instance Error Qerror where
  noMsg = QerrWithoutInfo
  strMsg s = QerrWithInfo s