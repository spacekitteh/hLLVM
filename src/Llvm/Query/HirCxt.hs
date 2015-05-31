{-# LANGUAGE RecordWildCards, GADTs #-}
module Llvm.Query.HirCxt where
import qualified Llvm.Hir.Data as Ci
import qualified Data.Map as M
import Llvm.Hir.Data
import Llvm.Hir.Print
import qualified Compiler.Hoopl as H

data TypeEnv = TypeEnv { dataLayout :: DataLayoutInfo
                       , targetTriple :: Ci.TargetTriple
                       , typedefs :: M.Map Ci.LocalId Ci.Dtype
                       , opaqueTypeDefs :: M.Map Ci.LocalId (Ci.Type OpaqueB D)
                       } deriving (Eq, Ord, Show)

data FunCxt = FunCxt { funInterface :: FunctionInterface
                     , dbgDeclares :: M.Map Ci.LocalId Ci.MdRef
                     } deriving (Eq, Ord, Show)

data GlobalCxt = GlobalCxt { typeEnv :: TypeEnv
                           , globals :: M.Map Ci.GlobalId (TlGlobal, Ci.Dtype)
                           , functions :: M.Map Ci.GlobalId FunctionDeclare
                           , attributes :: M.Map Word32 [FunAttr]
                           , unamedMetadata :: M.Map Word32 MetaKindedConst
                           } deriving (Eq, Ord, Show)

data IrCxt = IrCxt { globalCxt :: GlobalCxt
                   , funCxt :: FunCxt
                   } deriving (Eq, Ord, Show)


convert_to_FunctionDeclareType
  (FunctionInterface {..}) =
    FunctionDeclare { fd_linkage = fi_linkage
                    , fd_visibility = fi_visibility
                    , fd_dllstorage = fi_dllstorage
                    , fd_call_conv = fi_call_conv
                    , fd_param_attrs = fi_param_attrs
                    , fd_ret_type = fi_ret_type
                    , fd_fun_name = fi_fun_name
                    , fd_param_list = convert_to_FormalParamTypeList fi_param_list
                    , fd_addr_naming = fi_addr_naming
                    , fd_fun_attrs = fi_fun_attrs
                    , fd_section = fi_section
                    , fd_comdat = fi_comdat
                    , fd_alignment = fi_alignment
                    , fd_gc = fi_gc
                    , fd_prefix = fi_prefix
                    , fd_prologue = fi_prologue
                    }

convert_to_FormalParamTypeList :: FunParamList -> FunParamTypeList
convert_to_FormalParamTypeList (FunParamList l ma fas) =
  FunParamTypeList (fmap convert_to_FormalParamType l) ma fas

convert_to_FormalParamType :: FunParam -> FunParamType
convert_to_FormalParamType x = case x of
  FunParamData dt pas ma v -> FunParamDataType dt pas ma
  FunParamByVal dt pas ma v -> FunParamByValType dt pas ma


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
      funs = fmap (\tl -> case tl of
                      ToplevelDeclare (TlDeclare fp@FunctionDeclare{..}) -> (fd_fun_name, fp)
                      ToplevelDefine (TlDefine fp@FunctionInterface{..} _ _) -> 
                        (fi_fun_name, convert_to_FunctionDeclareType fp))
             $ filter (\x -> case x of
                          ToplevelDeclare _ -> True
                          ToplevelDefine{..} -> True
                          _ -> False
                      ) tl
      attrs = fmap (\(ToplevelAttribute (TlAttribute n l)) -> (n, l))
              $ filter (\x -> case x of
                           ToplevelAttribute _ -> True
                           _ -> False
                       ) tl
      unameMeta = fmap (\(ToplevelUnamedMd (TlUnamedMd n mc)) -> (n, mc))
                  $ filter (\x -> case x of
                               ToplevelUnamedMd _ -> True
                               _ -> False
                           ) tl
  in IrCxt { globalCxt = GlobalCxt { typeEnv = TypeEnv { dataLayout = getDataLayoutInfo dl
                                                       , targetTriple = tt
                                                       , typedefs = M.fromList tdefs
                                                       , opaqueTypeDefs = M.empty
                                                       }
                                   , globals = M.fromList glbs
                                   , functions = M.fromList funs
                                   , attributes = M.fromList attrs
                                   , unamedMetadata = M.fromList unameMeta
                                   }
           , funCxt = FunCxt { funInterface = error "funInterface is not initialized." 
                             , dbgDeclares = error "dbgDeclare is not initialized."
                             }
           }

instance IrPrint TypeEnv where
  printIr (TypeEnv dl tt td otd) = text "datalayout:" <+> printIr dl
                               $+$ text "triple:" <+> printIr tt
                               $+$ text "typedefs:" <+> printIr td
                               $+$ text "opaqueTypedefs:" <+> printIr otd

instance IrPrint GlobalCxt where
  printIr (GlobalCxt te gl fns atts um) = text "typeEnv:" <+> printIr te
                                          $+$ text "globals:" <+> printIr gl
                                          $+$ text "functions:" <+> printIr fns
                                          $+$ text "attributes:" <+> printIr atts
                                          $+$ text "unamedMetadata:" <+> printIr um

instance IrPrint FunCxt where
  printIr (FunCxt fi dbgDeclares) = text "funInterface:" <+> printIr fi
                                    $+$ text "dbgDeclares:" <+> printIr dbgDeclares

instance IrPrint IrCxt where
  printIr (IrCxt g l) = text "globalCxt:" <+> printIr g
                        $+$ text "funCxt:" <+> printIr l



funCxtOfTlDefine :: TlDefine a -> FunCxt
funCxtOfTlDefine (TlDefine fi _ graph) = 
  let dbgs = H.foldGraphNodes fld graph M.empty
  in FunCxt { funInterface = fi
            , dbgDeclares = dbgs }
  where
    fld :: Node a e x -> M.Map Ci.LocalId Ci.MdRef -> M.Map Ci.LocalId Ci.MdRef
    fld n = case n of
      Ci.Mnode v _ -> id
      _ -> id