module Llvm.Pass.IrBuild where

import Llvm.Data.Ir
import Llvm.Control.Context
import qualified Compiler.Hoopl as H
import qualified Data.Set as S
import Llvm.Query.Qerror

data ModuleState = ModuleState { moduleName :: Maybe String 
                               , usedNames :: S.Set String 
                               , count :: Int
                               , newNamePrefix :: String
                               }
data ModuleLayout = ModuleLayout { dataLayout :: DataLayout
                                 , curFun :: String
                                 }

type Mc a = CheckingFuelContextMonad ModuleState ModuleLayout Qerror a

getNewName :: Mc String
getNewName = do { s <- get
                ; modify (\ms -> ms {count = (count ms) + 1})
                ; return $ (newNamePrefix s) ++ show (count s)
                }

mkModule :: String -> Mc ()
mkModule mName = modify (\ms -> ms { moduleName = Just mName }) 

mkBase :: Mc ()
mkBase = undefined