{-# LANGUAGE GADTs #-}
module Llvm.Data.Ir
    (module Llvm.Data.Ir
    , module Llvm.Data.CoreIr
    , module Data.Word
    )
    where
import Llvm.Data.CoreIr
import qualified Llvm.Data.CoreIr as Ci
import qualified Compiler.Hoopl as H
import qualified Data.Set as S
import Data.Word (Word32)

{- An intermediate representation that is suitable for Hoopl -}

data Toplevel a = ToplevelTriple TlTriple
                | ToplevelDataLayout TlDataLayout
                | ToplevelAlias TlAlias
                | ToplevelDbgInit TlDbgInit
                | ToplevelStandaloneMd TlStandaloneMd
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

data TlDbgInit = TlDbgInit String Word32 deriving (Eq, Ord, Show)

data TlStandaloneMd = TlStandaloneMd String MetaKindedConst deriving (Eq, Ord, Show)

data TlNamedMd = TlNamedMd Ci.MdVar [Ci.MdNode] deriving (Eq, Ord, Show)

data TlDeclare = TlDeclare Ci.FunctionPrototype deriving (Eq)

type NOOP = ()

data TlDefine a = TlDefine Ci.FunctionPrototype H.Label (H.Graph (Node a) H.C H.C) 

data TlGlobal = TlGlobalDtype { tlg_lhs :: (Maybe Ci.GlobalId)
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
              | TlGlobalOpaque { tlg_lhs :: (Maybe Ci.GlobalId)
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

-- each instruction represents a node
data Node a e x where
  Nlabel :: H.Label -> Node a H.C H.O
  Pinst  :: Ci.PhiInstWithDbg -> Node a H.O H.O
  Cinst  :: Ci.CInstWithDbg -> Node a H.O H.O
  Comment :: String -> Node a H.O H.O
  Tinst  :: Ci.TerminatorInstWithDbg -> Node a H.O H.C
  Additional :: a -> Node a H.O H.O

instance Show a => Show (Node a e x) where
  show x = case x of
    Nlabel v -> "Nlabel: Node C O:" ++ show v
    Pinst v -> "Pinst : Node O O:" ++ show v
    Cinst v -> "Cinst : Node O O:" ++ show v
    Tinst v -> "Tinst : Node O C:" ++ show v
    Comment s -> "Comment : Node O O:" ++ s
    Additional a -> "Additional : Node O O:" ++ show a

instance Eq a => Eq (Node a e x) where
  (==) x1 x2 = case (x1, x2) of
    (Nlabel l1, Nlabel l2) -> l1 == l2
    (Pinst v1, Pinst v2) -> v1 == v2
    (Cinst v1, Cinst v2) -> v1 == v2
    (Tinst v1, Tinst v2) -> v1 == v2
    (Comment v1, Comment v2) -> v1 == v2
    (Additional a1, Additional a2) -> a1 == a2
    (_, _) -> False

instance (Show a, Ord a) => Ord (Node a e x) where
  compare x1 x2 = case (x1, x2) of
    (Nlabel l1, Nlabel l2) -> compare l1 l2
    (Pinst v1, Pinst v2) -> compare v1 v2
    (Cinst v1, Cinst v2) -> compare v1 v2
    (Tinst v1, Tinst v2) -> compare v1 v2
    (Comment v1, Comment v2) -> compare v1 v2
    (Additional v1, Additional v2) -> compare v1 v2
    (_, _) -> compare (show x1) (show x2)

instance H.NonLocal (Node a) where
  entryLabel (Nlabel l) = l
  successors (Tinst (Ci.TerminatorInstWithDbg inst _)) = suc inst
    where
      suc (Ci.Unreachable) = []
      suc (Ci.Return _) = []
      suc (Ci.RetVoid) = []
      suc (Ci.Br l) = [l]
      suc (Ci.Cbr _ l1 l2) = [l1, l2]
      suc (Ci.IndirectBr _ ls) = ls
      suc (Ci.Switch _ d ls) = (d):(map snd ls)
      suc (Ci.Invoke  _ l1 l2 _) = [l1, l2]
      suc (Ci.InvokeCmd _ l1 l2) = [l1, l2]
      suc (Ci.Resume _) = error "what is resume"
      suc (Ci.Unwind) = error "what is unwind"

globalIdOfModule :: (Module a) -> S.Set (Ci.Dtype, Ci.GlobalId) -- this should be a map, globalid might have an opaque type
globalIdOfModule (Module tl) = foldl (\a b -> S.union a (globalIdOf b)) S.empty tl
                               where globalIdOf (ToplevelGlobal (TlGlobalDtype lhs _ _ _ _ _ _ _ _ t _ _ _ _)) =
                                       maybe S.empty (\x -> S.singleton (t, x)) lhs
                                     globalIdOf _ = S.empty
