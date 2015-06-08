{-# LANGUAGE GADTs, RecordWildCards #-}
module Llvm.Hir.Data.Module
    (module Llvm.Hir.Data.Module
    , module Llvm.Hir.Data.Inst
    , module Llvm.Hir.Data.Commentor
    , module Data.Word
    )
    where
import Llvm.Hir.Data.Inst
import Llvm.Hir.Data.Commentor
import qualified Llvm.Hir.Data.Inst as Ci
import qualified Compiler.Hoopl as H
import Data.Word (Word32)

{- An intermediate representation that is suitable for Hoopl -}
data Toplevel a = ToplevelTriple TlTriple
                | ToplevelDataLayout TlDataLayout
                | ToplevelAlias TlAlias
                | ToplevelUnamedMd TlUnamedMd
                | ToplevelNamedMd TlNamedMd
                | ToplevelDeclare TlDeclare
                | ToplevelDefine (TlDefine a)
                | ToplevelGlobal TlGlobal
                | ToplevelTypeDef TlTypeDef
                | ToplevelDepLibs TlDepLibs
                | ToplevelUnamedType TlUnamedType
                | ToplevelModuleAsm TlModuleAsm
                | ToplevelAttribute TlAttribute
                | ToplevelComdat TlComdat
                | ToplevelIntrinsic TlIntrinsic

data TlIntrinsic = TlIntrinsic_llvm_used { tli_type :: Type RecordB D
                                         , tli_const :: Const 
                                         , tli_section :: Maybe Section
                                         }
                 | TlIntrinsic_llvm_compiler_used { tli_type :: Type RecordB D
                                                  , tli_const :: Const 
                                                  , tli_section :: Maybe Section
                                                  }
                 | TlIntrinsic_llvm_global_ctors { tli_type :: Type RecordB D
                                                 , tli_const :: Const 
                                                 , tli_section :: Maybe Section
                                                 }
                 | TlIntrinsic_llvm_global_dtors { tli_type :: Type RecordB D
                                                 , tli_const :: Const 
                                                 , tli_section :: Maybe Section
                                                 } 
                                           
data TlTriple = TlTriple Ci.TargetTriple deriving (Eq)

data TlDataLayout = TlDataLayout Ci.DataLayout deriving (Eq)

data TlAlias = TlAlias { tla_lhs :: Ci.GlobalId
                       , tla_visibility :: Maybe Ci.Visibility
                       , tla_dllstorage :: Maybe Ci.DllStorageClass
                       , tla_tls :: Maybe Ci.ThreadLocalStorage
                       , tla_addrnaming :: AddrNaming
                       , tla_linkage :: Maybe Ci.Linkage
                       , tla_aliasee :: Ci.Aliasee
                       } deriving (Eq, Ord, Show)

data TlUnamedMd = TlUnamedMd Word32 MetaKindedConst 
                | TlUnamedMd_DW_subprogram Word32 [MetaKindedConst]
                | TlUnamedMd_DW_file_type Word32 MetaKindedConst
                deriving (Eq, Ord, Show)


data TlNamedMd = TlNamedMd String [Ci.MdNode] deriving (Eq, Ord, Show)

data FunParamType = FunParamDataType Dtype [ParamAttr] (Maybe Alignment) 
                  | FunParamByValType Dtype [ParamAttr] (Maybe Alignment)
                  | FunParamMetaType MetaKind Fparam
                  deriving (Eq,Ord,Show)
                                                      

data FunParamTypeList = FunParamTypeList [FunParamType] (Maybe VarArgParam) [FunAttr]
                      deriving (Eq,Ord,Show)
                               

data FunctionDeclare = FunctionDeclare { fd_linkage :: Maybe Linkage
                                       , fd_visibility :: Maybe Visibility
                                       , fd_dllstorage :: Maybe DllStorageClass
                                       , fd_call_conv :: Maybe CallConv
                                       , fd_param_attrs :: [ParamAttr]
                                       , fd_ret_type :: Rtype
                                       , fd_fun_name :: GlobalId
                                       , fd_param_list :: FunParamTypeList
                                       , fd_addr_naming :: Maybe AddrNaming
                                       , fd_fun_attrs :: [FunAttr]
                                       , fd_section :: Maybe Section
                                       , fd_comdat :: Maybe Comdat
                                       , fd_alignment :: Maybe Alignment
                                       , fd_gc :: Maybe Gc
                                       , fd_prefix :: Maybe Prefix
                                       , fd_prologue :: Maybe Prologue
                                       } deriving (Eq,Ord,Show)
                                                  
data TlDeclare = TlDeclare FunctionDeclare deriving (Eq)

type NOOP = ()

data TlDefine a = TlDefine FunctionInterface H.Label (H.Graph (Node a) H.C H.C) 


data TlGlobal = TlGlobalDtype { tlg_lhs :: Ci.GlobalId
                              , tlg_linkage :: (Maybe Ci.Linkage)
                              , tlg_visibility :: (Maybe Ci.Visibility)
                              , tlg_dllstorage :: (Maybe Ci.DllStorageClass)
                              , tlg_tls :: (Maybe Ci.ThreadLocalStorage)
                              , tlg_addrnaming :: AddrNaming
                              , tlg_addrspace :: (Maybe Ci.AddrSpace)
                              , tlg_externallyInitialized :: (IsOrIsNot Ci.ExternallyInitialized)
                              , tlg_globalType :: Ci.GlobalType
                              , tlg_dtype :: Ci.Dtype
                              , tlg_const :: (Maybe Ci.Const)
                              , tlg_section :: (Maybe Ci.Section)
                              , tlg_comdat :: (Maybe Ci.Comdat)
                              , tlg_alignment :: (Maybe Ci.Alignment)
                              }
              | TlGlobalOpaque { tlg_lhs :: Ci.GlobalId
                               , tlg_linkage :: (Maybe Ci.Linkage)
                               , tlg_visiblity :: (Maybe Ci.Visibility)
                               , tlg_dllstorage :: (Maybe Ci.DllStorageClass)
                               , tlg_tls :: (Maybe Ci.ThreadLocalStorage)
                               , tlg_addrnaming :: AddrNaming
                               , tlg_addrspace :: (Maybe Ci.AddrSpace)
                               , tlg_externallyInitialized :: (IsOrIsNot Ci.ExternallyInitialized)
                               , tlg_globalType :: Ci.GlobalType
                               , tlg_otype :: (Ci.Type Ci.OpaqueB D)
                               , tlg_const :: (Maybe Ci.Const)
                               , tlg_section :: (Maybe Ci.Section)
                               , tlg_comdat :: (Maybe Ci.Comdat)
                               , tlg_alignment :: (Maybe Ci.Alignment)
                               } deriving (Eq, Ord, Show)

data TlTypeDef = TlDatTypeDef Ci.LocalId Ci.Dtype
               | TlOpqTypeDef Ci.LocalId (Ci.Type OpaqueB D)
               | TlFunTypeDef Ci.LocalId (Ci.Type CodeFunB X) deriving (Eq, Ord, Show)

data TlDepLibs = TlDepLibs [Ci.DqString] deriving (Eq, Ord, Show)

data TlUnamedType = TlUnamedType Word32 Ci.Dtype deriving (Eq, Ord, Show)

data TlModuleAsm = TlModuleAsm Ci.DqString deriving (Eq, Ord, Show)

data TlAttribute = TlAttribute Word32 [FunAttr] deriving (Eq, Ord, Show)

data TlComdat = TlComdat Ci.DollarId Ci.SelectionKind deriving (Eq, Ord, Show)

data Module a = Module [Toplevel a] 

  
  
data Node a e x where
  -- | Lnode represents the name of a basic block
  Lnode :: H.Label -> Node a H.C H.O
  -- | Pnode, Phi nodes, if they ever exist, always follow up a Lnode in a basic block
  Pnode  :: Ci.Pinst -> [Dbg] -> Node a H.O H.O
  -- | Cnode, Computation nodes, it can be anyway between Lnode(+ Possibly Pnodes) and Tnode
  Cnode  :: Ci.Cinst -> [Dbg] -> Node a H.O H.O
  -- | Mnode, Metadata nodes, dataflow analysis can refer to, but should not depend on, the information in Metadata nodes
  Mnode :: Ci.Minst -> [Dbg] -> Node a H.O H.O
  -- | It's handy if we can communicate some decisions or changes made by
  -- | dataflow analyses/transformations back as comments, so I create this
  -- | Comment node, it is NOOP and will display as a comment in LLVM assembly codes
  Comment :: Commentor s => s -> Node a H.O H.O
  -- | Extension node, it could be NOOP or any customization node used by
  -- | backends to facilitate dataflow analysis and transformations.
  -- | The clients of this library should never handle this node. If 
  -- | a Enode represents anything other than NOOP, which is (), it should alwasy be  
  -- | converted back to other nodes. 
  Enode :: a -> [Dbg] -> Node a H.O H.O
  Tnode  :: Ci.Tinst -> [Dbg] -> Node a H.O H.C  

instance Show a => Show (Node a e x) where
  show x = case x of
    Lnode v -> "Lnode: Node C O:" ++ show v
    Pnode v dbgs -> "Pnode : Node O O:" ++ show v ++ show dbgs
    Cnode v dbgs -> "Cnode : Node O O:" ++ show v ++ show dbgs
    Mnode v dbgs -> "Mnode : Node O O:" ++ show v ++ show dbgs    
    Comment s -> "Comment : Node O O:" ++ show (commentize s)
    Enode a dbgs -> "Enode : Node O O:" ++ show a ++ show dbgs
    Tnode v dbgs -> "Tnode : Node O C:" ++ show v ++ show dbgs    

instance Eq a => Eq (Node a e x) where
  (==) x1 x2 = case (x1, x2) of
    (Lnode l1, Lnode l2) -> l1 == l2
    (Pnode v1 d1, Pnode v2 d2) -> v1 == v2 && d1 == d2
    (Cnode v1 d1, Cnode v2 d2) -> v1 == v2 && d1 == d2
    (Mnode v1 d1, Mnode v2 d2) -> v1 == v2 && d1 == d2    
    (Comment v1, Comment v2) -> commentize v1 == commentize v2
    (Enode a1 d1, Enode a2 d2) -> a1 == a2 && d1 == d2
    (Tnode v1 d1, Tnode v2 d2) -> v1 == v2 && d1 == d2    
    (_, _) -> False

instance (Show a, Ord a) => Ord (Node a e x) where
  compare x1 x2 = case (x1, x2) of
    (Lnode l1, Lnode l2) -> compare l1 l2
    (Pnode v1 d1, Pnode v2 d2) -> compare (v1,d1) (v2,d2)
    (Cnode v1 d1, Cnode v2 d2) -> compare (v1,d1) (v2,d2)
    (Mnode v1 d1, Mnode v2 d2) -> compare (v1,d1) (v2,d2)    
    (Comment v1, Comment v2) -> compare (commentize v1) (commentize v2)
    (Enode v1 d1, Enode v2 d2) -> compare (v1,d1) (v2,d2)
    (Tnode v1 d1, Tnode v2 d2) -> compare (v1,d1) (v2,d2)
    (_, _) -> compare (show x1) (show x2)

instance H.NonLocal (Node a) where
  entryLabel (Lnode l) = l
  successors (Tnode inst _) = case inst of
    Ci.T_unreachable -> []
    Ci.T_return _ -> []
    Ci.T_ret_void -> []
    Ci.T_br l -> [l]
    Ci.T_cbr{..} -> [trueL, falseL]
    Ci.T_indirectbr _ ls -> ls
    Ci.T_switch d ls -> (snd d):(map snd ls)
    Ci.T_invoke{..} -> [invoke_normal_label, invoke_exception_label]
    Ci.T_invoke_asm{..} -> [invoke_normal_label, invoke_exception_label]
    Ci.T_resume _ -> []
    Ci.T_unwind -> error "what is unwind"