{-# LANGUAGE RecordWildCards #-}
module Llvm.Pass.CfCxtTransform where

import Llvm.Data.Ir
import Llvm.Control.Context


data Transformer s r e = Transformer 
                         { trans_triple :: TpTriple -> CfCxt s r e [Toplevel]
                         , trans_datalayout :: DataLayout -> CfCxt s r e [Toplevel]
                         , trans_alias :: TpAlias -> CfCxt s r e [Toplevel]
                         , trans_dbginit :: TpDbgInit -> CfCxt s r e [Toplevel]
                         , trans_standaloneMd :: TpStandaloneMd -> CfCxt s r e [Toplevel]
                         , trans_namedMd :: TpNamedMd -> CfCxt s r e [Toplevel]
                         , trans_declare :: FunctionPrototype -> CfCxt s r e [Toplevel]
                         , trans_define :: TpDefine -> CfCxt s r e [Toplevel]
                         , trans_global :: TpGlobal -> CfCxt s r e [Toplevel]
                         , trans_typedef :: TpTypeDef -> CfCxt s r e [Toplevel]
                         , trans_deplibs :: TpDepLibs -> CfCxt s r e [Toplevel]
                         , trans_unamedType :: TpUnamedType -> CfCxt s r e [Toplevel]
                         , trans_moduleAsm :: TpModuleAsm -> CfCxt s r e [Toplevel]
                         , trans_attribute :: TpAttribute -> CfCxt s r e [Toplevel]
                         , trans_comdat :: TpComdat -> CfCxt s r e [Toplevel]
                         }
                     
idTransformer :: Error e => Transformer s r e
idTransformer = Transformer { trans_triple = one . ToplevelTriple
                            , trans_datalayout = one . ToplevelDataLayout
                            , trans_alias = one . ToplevelAlias
                            , trans_dbginit = one . ToplevelDbgInit
                            , trans_standaloneMd = one . ToplevelStandaloneMd
                            , trans_namedMd = one . ToplevelNamedMd
                            , trans_declare = one . ToplevelDeclare
                            , trans_define = one . ToplevelDefine
                            , trans_global = one . ToplevelGlobal
                            , trans_typedef = one . ToplevelTypeDef
                            , trans_deplibs = one . ToplevelDepLibs
                            , trans_unamedType = one . ToplevelUnamedType
                            , trans_moduleAsm = one . ToplevelModuleAsm
                            , trans_attribute = one . ToplevelAttribute
                            , trans_comdat = one . ToplevelComdat
                            }
  where one x = return [x]

trans :: Error e => Transformer s r e -> Toplevel -> CfCxt s r e [Toplevel]
trans (Transformer {..}) tl = case tl of
  (ToplevelTriple s) -> trans_triple s
  (ToplevelDataLayout s) -> trans_datalayout s
  (ToplevelAlias s) -> trans_alias s
  (ToplevelDbgInit s) -> trans_dbginit s
  (ToplevelStandaloneMd s) -> trans_standaloneMd s
  (ToplevelNamedMd s) -> trans_namedMd s
  (ToplevelDeclare s) -> trans_declare s
  (ToplevelDefine s) -> trans_define s
  (ToplevelGlobal s) -> trans_global s
  (ToplevelTypeDef s) -> trans_typedef s
  (ToplevelDepLibs s) -> trans_deplibs s
  (ToplevelUnamedType s) -> trans_unamedType s
  (ToplevelModuleAsm s) -> trans_moduleAsm s
  (ToplevelComdat s) -> trans_comdat s
  (ToplevelAttribute s) -> trans_attribute s


transModule :: Error e => Transformer s r e -> Module -> CfCxt s r e Module
transModule tr (Module l) = 
  mapM (trans tr) l >>= return . Module . concat