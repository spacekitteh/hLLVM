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
data Toplevel g a = ToplevelAlias (TlAlias g)
                  | ToplevelUnamedMd (TlUnamedMd g)
                  | ToplevelNamedMd TlNamedMd
                  | ToplevelDeclare (TlDeclare g)
                  | ToplevelDefine (TlDefine g a)
                  | ToplevelGlobal (TlGlobal g)
                  | ToplevelTypeDef TlTypeDef
                  | ToplevelDepLibs TlDepLibs
                  | ToplevelUnamedType TlUnamedType
                  | ToplevelModuleAsm TlModuleAsm
                  | ToplevelAttribute TlAttribute
                  | ToplevelComdat (TlComdat g)
                  | ToplevelIntrinsic (TlIntrinsic g)

data TlIntrinsic g = TlIntrinsic_llvm_used { tli_type :: Type RecordB D
                                           , tli_const :: Const g
                                           , tli_section :: Maybe Section
                                           }
                   | TlIntrinsic_llvm_compiler_used { tli_type :: Type RecordB D
                                                    , tli_const :: Const g
                                                    , tli_section :: Maybe Section
                                                    }
                   | TlIntrinsic_llvm_global_ctors { tli_type :: Type RecordB D
                                                   , tli_const :: Const g
                                                   , tli_section :: Maybe Section
                                                   }
                   | TlIntrinsic_llvm_global_dtors { tli_type :: Type RecordB D
                                                   , tli_const :: Const g
                                                   , tli_section :: Maybe Section
                                                   } 
                                           
data TlAlias g = TlAlias { tla_lhs :: g
                         , tla_visibility :: Maybe Ci.Visibility
                         , tla_dllstorage :: Maybe Ci.DllStorageClass
                         , tla_tls :: Maybe Ci.ThreadLocalStorage
                         , tla_addrnaming :: AddrNaming
                         , tla_linkage :: Maybe Ci.Linkage
                         , tla_aliasee :: Ci.Aliasee g
                         } deriving (Eq, Ord, Show)

data TlUnamedMd g = TlUnamedMd Word32 (MetaKindedConst g)
                  | TlUnamedMd_Tagged Word32 DW_TAG [MetaKindedConst g]
                  deriving (Eq, Ord, Show)

data DW_TAG = DW_TAG_array_type
            | DW_TAG_class_type
            | DW_TAG_entry_point
            | DW_TAG_enumeration_type
            | DW_TAG_formal_parameter
            | DW_TAG_imported_declaration
            | DW_TAG_label
            | DW_TAG_lexical_block
            | DW_TAG_member
            | DW_TAG_pointer_type
            | DW_TAG_reference_type
            | DW_TAG_compile_unit
            | DW_TAG_string_type
            | DW_TAG_structure_type
            | DW_TAG_subroutine_type
            | DW_TAG_typedef
            | DW_TAG_union_type
            | DW_TAG_unspecified_parameters
            | DW_TAG_variant
            | DW_TAG_common_block
            | DW_TAG_common_inclusion
            | DW_TAG_inheritance
            | DW_TAG_inlined_subroutine
            | DW_TAG_module
            | DW_TAG_ptr_to_member_type
            | DW_TAG_set_type
            | DW_TAG_subrange_type
            | DW_TAG_with_stmt
            | DW_TAG_access_declaration
            | DW_TAG_base_type
            | DW_TAG_catch_block
            | DW_TAG_const_type
            | DW_TAG_constant
            | DW_TAG_enumerator
            | DW_TAG_file_type
            | DW_TAG_friend
            | DW_TAG_namelist
            | DW_TAG_namelist_item
            | DW_TAG_packed_type
            | DW_TAG_subprogram
            | DW_TAG_template_type_parameter
            | DW_TAG_template_value_parameter
            | DW_TAG_thrown_type
            | DW_TAG_try_block
            | DW_TAG_variant_part
            | DW_TAG_variable
            | DW_TAG_volatile_type
            | DW_TAG_dwarf_procedure
            | DW_TAG_restrict_type
            | DW_TAG_interface_type
            | DW_TAG_namespace
            | DW_TAG_imported_module
            | DW_TAG_unspecified_type
            | DW_TAG_partial_unit
            | DW_TAG_imported_unit
            | DW_TAG_condition
            | DW_TAG_shared_type
            | DW_TAG_type_unit
            | DW_TAG_rvalue_reference_type
            | DW_TAG_template_alias
            | DW_TAG_coarray_type
            | DW_TAG_generic_subrange
            | DW_TAG_dynamic_type
            | DW_TAG_auto_variable
            | DW_TAG_arg_variable
            | DW_TAG_user_base
            | DW_TAG_MIPS_loop
            | DW_TAG_format_label
            | DW_TAG_function_template
            | DW_TAG_class_template
            | DW_TAG_GNU_template_template_param
            | DW_TAG_GNU_template_parameter_pack
            | DW_TAG_GNU_formal_parameter_pack
            | DW_TAG_lo_user
            | DW_TAG_APPLE_property
            | DW_TAG_hi_user
            deriving (Eq, Show, Ord)


data TlNamedMd = TlNamedMd String [Ci.MdNode] deriving (Eq, Ord, Show)


data FunctionDeclare g = FunctionDeclareData { fd_linkage :: Maybe Linkage
                                             , fd_visibility :: Maybe Visibility
                                             , fd_dllstorage :: Maybe DllStorageClass
                                             , fd_signature :: FunSignature ()
                                             , fd_fun_name :: g
                                             , fd_addr_naming :: Maybe AddrNaming
                                             , fd_fun_attrs :: [FunAttr]
                                             , fd_section :: Maybe Section
                                             , fd_comdat :: Maybe (Comdat g)
                                             , fd_alignment :: Maybe AlignInByte
                                             , fd_gc :: Maybe Gc
                                             , fd_prefix :: Maybe (Prefix g)
                                             , fd_prologue :: Maybe (Prologue g)
                                             } 
                       | FunctionDeclareMeta { fd_fun_name :: g
                                             , fd_fun_attrs :: [FunAttr]
                                             , fd_retType :: Rtype
                                             , fd_metakinds :: [Either MetaKind Dtype]
                                             }
                       deriving (Eq,Ord,Show)
                                                  
data TlDeclare g = TlDeclare (FunctionDeclare g) deriving (Eq)

type NOOP = ()

data TlDefine g a = TlDefine (FunctionInterface g) H.Label (H.Graph (Node g a) H.C H.C) 


data TlGlobal g = TlGlobalDtype { tlg_lhs :: g
                                , tlg_linkage :: (Maybe Ci.Linkage)
                                , tlg_visibility :: (Maybe Ci.Visibility)
                                , tlg_dllstorage :: (Maybe Ci.DllStorageClass)
                                , tlg_tls :: (Maybe Ci.ThreadLocalStorage)
                                , tlg_addrnaming :: AddrNaming
                                , tlg_addrspace :: (Maybe Ci.AddrSpace)
                                , tlg_externallyInitialized :: (IsOrIsNot Ci.ExternallyInitialized)
                                , tlg_globalType :: Ci.GlobalType
                                , tlg_dtype :: Ci.Dtype
                                , tlg_const :: Maybe (Ci.Const g)
                                , tlg_section :: Maybe Ci.Section
                                , tlg_comdat :: Maybe (Ci.Comdat g)
                                , tlg_alignment :: Maybe Ci.AlignInByte
                                }
                | TlGlobalOpaque { tlg_lhs :: g
                                 , tlg_linkage :: (Maybe Ci.Linkage)
                                 , tlg_visiblity :: (Maybe Ci.Visibility)
                                 , tlg_dllstorage :: (Maybe Ci.DllStorageClass)
                                 , tlg_tls :: (Maybe Ci.ThreadLocalStorage)
                                 , tlg_addrnaming :: AddrNaming
                                 , tlg_addrspace :: (Maybe Ci.AddrSpace)
                                 , tlg_externallyInitialized :: (IsOrIsNot Ci.ExternallyInitialized)
                                 , tlg_globalType :: Ci.GlobalType
                                 , tlg_otype :: Ci.Type Ci.OpaqueB D
                                 , tlg_const :: Maybe (Ci.Const g)
                                 , tlg_section :: Maybe Ci.Section
                                 , tlg_comdat :: Maybe (Ci.Comdat g)
                                 , tlg_alignment :: (Maybe Ci.AlignInByte)
                                 } deriving (Eq, Ord, Show)

data TlTypeDef = TlDatTypeDef Ci.Lname Ci.Dtype
               | TlOpqTypeDef Ci.Lname (Ci.Type OpaqueB D)
               | TlFunTypeDef Ci.Lname (Ci.Type CodeFunB X) deriving (Eq, Ord, Show)

data TlDepLibs = TlDepLibs [Ci.DqString] deriving (Eq, Ord, Show)

data TlUnamedType = TlUnamedType Word32 Ci.Dtype deriving (Eq, Ord, Show)

data TlModuleAsm = TlModuleAsm Ci.DqString deriving (Eq, Ord, Show)

data TlAttribute = TlAttribute Word32 [FunAttr] deriving (Eq, Ord, Show)

data TlComdat g = TlComdat g Ci.SelectionKind deriving (Eq, Ord, Show)

{- 
  If a consumer does not demands specializing Cnode and GlobalId,
  then Module GlobalId () is a typical instance
-}
data Module g a = Module [Toplevel g a]
  
{-
   Node g a e x
        ^ ^ ^ ^
        | | | +-- the shape of exit
        | | +---- the shape of entry
        | +------ the enode parameter
        +-- global id parameter

  g and a are parameters used to specialize GlobalId and Cnode
  If a consumer does not need to specialize the above two, the unit type () 
  can be used to instantiate g and a.

  So Node () () e x is a typical instantiation for many consumers.
-}
data Node g a e x where
  -- | Lnode represents the name of a basic block
  Lnode :: H.Label -> Node g a H.C H.O
  -- | Pnode, Phi nodes, if they ever exist, always follow up a Lnode in a basic block
  Pnode  :: Ci.Pinst g -> [Dbg g] -> Node g a H.O H.O
  -- | Cnode, Computation nodes, it can be anyway between Lnode(+ Possibly Pnodes) and Tnode
  Cnode  :: Ci.Cinst g -> [Dbg g] -> Node g a H.O H.O
  -- | Mnode, Metadata nodes, dataflow analysis can refer to, but should not depend on, the information in Metadata nodes
  Mnode :: Ci.Minst g -> [Dbg g] -> Node g a H.O H.O
  -- | It's handy if we can communicate some decisions or changes made by
  -- | dataflow analyses/transformations back as comments, so I create this
  -- | Comment node, it is NOOP and will display as a comment in LLVM assembly codes
  Comment :: Commentor s => s -> Node g a H.O H.O
  -- | Extension node, it could be NOOP or any customization node used by
  -- | backends to facilitate dataflow analysis and transformations.
  -- | The clients of this library should never handle this node. If 
  -- | a Enode represents anything other than NOOP, which is (), it should alwasy be  
  -- | converted back to other nodes. 
  Enode :: a -> [Dbg g] -> Node g a H.O H.O
  Tnode  :: Ci.Tinst g -> [Dbg g] -> Node g a H.O H.C  

instance (Show g, Show a) => Show (Node g a e x) where
  show x = case x of
    Lnode v -> "Lnode: Node C O:" ++ show v
    Pnode v dbgs -> "Pnode : Node O O:" ++ show v ++ show dbgs
    Cnode v dbgs -> "Cnode : Node O O:" ++ show v ++ show dbgs
    Mnode v dbgs -> "Mnode : Node O O:" ++ show v ++ show dbgs    
    Comment s -> "Comment : Node O O:" ++ show (commentize s)
    Enode a dbgs -> "Enode : Node O O:" ++ show a ++ show dbgs
    Tnode v dbgs -> "Tnode : Node O C:" ++ show v ++ show dbgs    

instance (Eq g, Show g, Eq a) => Eq (Node g a e x) where
  (==) x1 x2 = case (x1, x2) of
    (Lnode l1, Lnode l2) -> l1 == l2
    (Pnode v1 d1, Pnode v2 d2) -> v1 == v2 && d1 == d2
    (Cnode v1 d1, Cnode v2 d2) -> v1 == v2 && d1 == d2
    (Mnode v1 d1, Mnode v2 d2) -> v1 == v2 && d1 == d2    
    (Comment v1, Comment v2) -> commentize v1 == commentize v2
    (Enode a1 d1, Enode a2 d2) -> a1 == a2 && d1 == d2
    (Tnode v1 d1, Tnode v2 d2) -> v1 == v2 && d1 == d2    
    (_, _) -> False

instance (Ord g, Show g, Show a, Ord a) => Ord (Node g a e x) where
  compare x1 x2 = case (x1, x2) of
    (Lnode l1, Lnode l2) -> compare l1 l2
    (Pnode v1 d1, Pnode v2 d2) -> compare (v1,d1) (v2,d2)
    (Cnode v1 d1, Cnode v2 d2) -> compare (v1,d1) (v2,d2)
    (Mnode v1 d1, Mnode v2 d2) -> compare (v1,d1) (v2,d2)    
    (Comment v1, Comment v2) -> compare (commentize v1) (commentize v2)
    (Enode v1 d1, Enode v2 d2) -> compare (v1,d1) (v2,d2)
    (Tnode v1 d1, Tnode v2 d2) -> compare (v1,d1) (v2,d2)
    (_, _) -> compare (show x1) (show x2)

instance H.NonLocal (Node g a) where
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