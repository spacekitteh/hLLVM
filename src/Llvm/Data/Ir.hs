{-# LANGUAGE GADTs #-}
module Llvm.Data.Ir 
    (module Llvm.Data.Ir
    , module Llvm.Data.CoreIr
    )
    where
import Llvm.Data.CoreIr
import qualified Llvm.Data.CoreIr as Ci
import qualified Compiler.Hoopl as H
import qualified Data.Set as S

{- An intermediate representation that is suitable for Hoopl -}

data Toplevel = ToplevelTriple Ci.DqString
              | ToplevelDataLayout Ci.DataLayout
              | ToplevelAlias Ci.GlobalId (Maybe Ci.Visibility) (Maybe Ci.DllStorageClass) (Maybe Ci.ThreadLocalStorage) AddrNaming (Maybe Ci.Linkage) Ci.Aliasee
              | ToplevelDbgInit String Integer
              | ToplevelStandaloneMd String Ci.TypedValue
              | ToplevelNamedMd Ci.MdVar [Ci.MdNode]
              | ToplevelDeclare Ci.FunctionPrototype
              | ToplevelDefine Ci.FunctionPrototype H.Label (H.Graph Node H.C H.C)
              | ToplevelGlobal 
                (Maybe Ci.GlobalId)
                (Maybe Ci.Linkage)
                (Maybe Ci.Visibility)
                (Maybe Ci.DllStorageClass)
                (Maybe Ci.ThreadLocalStorage)
                AddrNaming
                (Maybe Ci.AddrSpace)
                (IsOrIsNot Ci.ExternallyInitialized)
                Ci.GlobalType
                Ci.Type
                (Maybe Ci.Const)
                (Maybe Ci.Section)
                (Maybe Ci.Comdat)
                (Maybe Ci.Alignment)
              | ToplevelTypeDef Ci.LocalId Ci.Type
              | ToplevelDepLibs [Ci.DqString]
              | ToplevelUnamedType Integer Ci.Type
              | ToplevelModuleAsm Ci.DqString
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
    successors (Tinst (Ci.TerminatorInstWithDbg inst _)) = suc inst
      where
        suc (Ci.Unreachable) = []
        suc (Ci.Return _) = []
        suc (Ci.Br l) = [getLabel l]
        suc (Ci.Cbr _ l1 l2) = [getLabel l1, getLabel l2]
        suc (Ci.IndirectBr c ls) = map getLabel ls
        suc (Ci.Switch  _ d ls) = (getLabel d):(map (getLabel . snd) ls)
        suc (Ci.Invoke _  _ l1 l2) = [getLabel l1, getLabel l2]
        suc (Ci.Resume _) = []


globalIdOfModule :: Module -> S.Set (Ci.Type, Ci.GlobalId)
globalIdOfModule (Module tl) = foldl (\a b -> S.union a (globalIdOf b)) S.empty tl
                               where globalIdOf (ToplevelGlobal lhs _ _ _ _ _ _ _ _ t _ _ _ _) = maybe S.empty (\x -> S.singleton (t, x)) lhs
                                     globalIdOf _ = S.empty
