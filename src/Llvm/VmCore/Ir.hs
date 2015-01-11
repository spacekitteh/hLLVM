{-# LANGUAGE GADTs #-}
module Llvm.VmCore.Ir 
    (module Llvm.VmCore.Ir
    , module Llvm.VmCore.CoreIr
    )
    where
import Llvm.VmCore.CoreIr
import qualified Llvm.VmCore.CoreIr as Ci
import qualified Compiler.Hoopl as H
import qualified Data.Map as M
import qualified Data.Set as S

{- An intermediate representation that is suitable for Hoopl -}

data Toplevel = ToplevelTriple Ci.QuoteStr
              | ToplevelDataLayout Ci.DataLayout
              | ToplevelAlias Ci.GlobalId (Maybe Ci.Visibility) (Maybe Ci.DllStorage) (Maybe Ci.ThreadLocalStorage) AddrNaming (Maybe Ci.Linkage) Ci.Aliasee
              | ToplevelDbgInit String Integer
              | ToplevelStandaloneMd String Ci.TypedValue
              | ToplevelNamedMd Ci.MdVar [Ci.MdNode]
              | ToplevelDeclare Ci.FunctionPrototype
              | ToplevelDefine Ci.FunctionPrototype H.Label (H.Graph Node H.C H.C)
              | ToplevelGlobal { toplevelGlobalLhs :: Maybe Ci.GlobalId
                               , toplevelGlobalLinkage :: Maybe Ci.Linkage
                               , toplevelGlobalVisibility :: Maybe Ci.Visibility
                               , toplevelGlobalDllStorage :: Maybe Ci.DllStorage
                               , toplevelGlobalThreadLocation :: Maybe Ci.ThreadLocalStorage
                               , toplevelGlobalUnamedAddr :: AddrNaming
                               , toplevelGlobalAddrSpace :: Maybe Ci.AddrSpace
                               , toplevelGlobalExternallyInitialized :: IsOrIsNot Ci.ExternallyInitialized
                               , toplevelGlobalGlobalType :: Ci.GlobalType
                               , toplevelGlobalType :: Ci.Type
                               , toplevelGlobalConst :: Maybe Ci.Const
                               , toplevelGlobalSection :: Maybe Ci.Section
                               , toplevelGlobalComdat :: Maybe Ci.Comdat
                               , toplevelGlobalAlign :: Maybe Ci.Alignment
                               }
              | ToplevelTypeDef Ci.LocalId Ci.Type
              | ToplevelDepLibs [Ci.QuoteStr]
              | ToplevelUnamedType Integer Ci.Type
              | ToplevelModuleAsm Ci.QuoteStr
              | ToplevelAttribute Integer [FunAttr]
              | ToplevelComdat Ci.DollarId Ci.SelectionKind
                
data Module = Module [Toplevel] 

-- each instruction represents a node
data Node e x where
    Nlabel :: Ci.BlockLabel -> Node H.C H.O
    Pinst  :: Ci.PhiInst -> Node H.O H.O
    Cinst  :: Ci.ComputingInstWithDbg -> Node H.O H.O
    Tinst  :: Ci.TerminatorInstWithDbg -> Node H.O H.C


getLabel :: Ci.TargetLabel -> H.Label
getLabel (Ci.TargetLabel (Ci.PercentLabel l)) = Ci.toLabel l

instance H.NonLocal Node where
    entryLabel (Nlabel (Ci.BlockLabel l)) = Ci.toLabel l
    successors (Tinst (Ci.TerminatorInstWithDbg inst l)) = succ inst
      where
        succ (Ci.Unreachable) = []
        succ (Ci.Return _) = []
        succ (Ci.Br l) = [getLabel l]
        succ (Ci.Cbr _ l1 l2) = [getLabel l1, getLabel l2]
        succ (Ci.IndirectBr c ls) = map getLabel ls
        succ (Ci.Switch  _ d ls) = (getLabel d):(map (getLabel . snd) ls)
        succ (Ci.Invoke _  _ l1 l2) = [getLabel l1, getLabel l2]
        succ (Ci.Resume _) = []


globalIdOfModule :: Module -> S.Set (Ci.Type, Ci.GlobalId)
globalIdOfModule (Module tl) = foldl (\a b -> S.union a (globalIdOf b)) S.empty tl
                               where globalIdOf (ToplevelGlobal lhs _ _ _ _ _ _ _ _ t _ _ _ _) = maybe S.empty (\x -> S.singleton (t, x)) lhs
                                     globalIdOf _ = S.empty
