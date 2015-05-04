module Llvm.Query.IrCxt where
import qualified Llvm.Data.CoreIr as Ci
import qualified Data.Map as M
import Llvm.Data.Ir

data TypeEnv = TypeEnv { dataLayout :: Ci.DataLayoutInfo
                       , targetTriple :: Ci.TargetTriple
                       , typedefs :: M.Map Ci.LocalId Ci.Dtype
                       , opaqueTypeDefs :: M.Map Ci.LocalId (Ci.Type OpaqueB D)
                       } deriving (Eq, Ord, Show)

data FunCxt = FunCxt { funName :: String 
                     , funParameters :: M.Map Ci.LocalId Ci.Dtype
                     } deriving (Eq, Ord, Show)

data GlobalCxt = GlobalCxt { typeEnv :: TypeEnv
                           , globals :: M.Map Ci.GlobalId (TlGlobal, Ci.Dtype)
                           } deriving (Eq, Ord, Show)
                                
data IrCxt = IrCxt { globalCxt :: GlobalCxt
                   , funCxt :: FunCxt
                   } deriving (Eq, Ord, Show)

irCxtOfModule :: Module a -> IrCxt
irCxtOfModule (Module tl) = 
  let [ToplevelDataLayout (TlDataLayout dl)] = filter (\x -> case x of
                                                          ToplevelDataLayout _ -> True
                                                          _ -> False
                                                      ) tl
      [ToplevelTriple (TlTriple tt)] = filter (\x -> case x of
                                                  ToplevelTriple _ -> True
                                                  _ -> False
                                              ) tl
      tdefs = fmap (\(ToplevelTypeDef td) -> case td of
                       TlDatTypeDef lhs def -> (lhs, def)) 
              $ filter (\x -> case x of
                           ToplevelTypeDef (TlDatTypeDef _ _) -> True
                           _ -> False
                       ) tl
      glbs = fmap (\(ToplevelGlobal g@(TlGlobalDtype lhs _ _ _ _ _ _ _ _ t _ _ _ _)) -> (lhs, (g,t))) 
             $ filter (\x -> case x of
                          ToplevelGlobal _ -> True
                          _ -> False
                      ) tl
  in IrCxt { globalCxt = GlobalCxt { typeEnv = TypeEnv { dataLayout = getDataLayoutInfo dl
                                                       , targetTriple = tt
                                                       , typedefs = M.fromList tdefs
                                                       , opaqueTypeDefs = M.empty
                                                       }
                                   , globals = M.fromList glbs
                                   }
           , funCxt = FunCxt { funName = ""
                             , funParameters = M.empty
                             }
           }