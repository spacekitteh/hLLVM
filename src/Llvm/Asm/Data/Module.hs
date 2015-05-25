{-# LANGUAGE GADTs #-}
module Llvm.Asm.Data.Module
       ( module Llvm.Asm.Data.Module
       , module Llvm.Asm.Data.SimpleConst
       , module Llvm.Asm.Data.AtomicEntity
       , module Llvm.Asm.Data.Type
       , module Llvm.Asm.Data.DataLayout
       ) where

import Llvm.Asm.Data.SimpleConst
import Llvm.Asm.Data.AtomicEntity
import Llvm.Asm.Data.DataLayout
import Llvm.Asm.Data.Type
import qualified Data.Map as M
import Data.Word (Word32)

{-
  This AST is designed to make parsing and pretty print easy.
  It's not meant to be friendly to pattern matching and manual
  construction.
-}

-- | quotation does not change a label value
-- | it's still unclear when a quoted verion is used
-- | we keep the original format to make llvm-as happy
data LabelId = LabelString String
             | LabelDqString String -- a string enclosed by double quotes
             | LabelNumber Word32
             | LabelDqNumber Word32 -- a number enclosed by double quotes
             deriving (Eq,Ord,Show)

data BlockLabel = ExplicitBlockLabel LabelId
                | ImplicitBlockLabel (String, Int, Int)
                deriving (Eq, Ord, Show)

data PercentLabel = PercentLabel LabelId deriving (Eq, Ord, Show)
data TargetLabel = TargetLabel PercentLabel deriving (Eq,Ord,Show)

data IbinOp = Add | Sub | Mul | Udiv
            | Sdiv | Urem | Srem
            | Shl | Lshr | Ashr | And | Or | Xor
            deriving (Eq,Ord,Show)

ibinOpMap :: M.Map IbinOp String
ibinOpMap = M.fromList [(Add, "add"), (Sub, "sub"), (Mul, "mul")
                       ,(Udiv, "udiv"), (Sdiv, "sdiv")
                       ,(Urem, "urem"), (Srem, "srem")
                       ,(Shl, "shl"), (Lshr, "lshr"), (Ashr, "ashr")
                       ,(And, "and"), (Or, "or"), (Xor, "xor")
                       ]

data FbinOp = Fadd | Fsub | Fmul | Fdiv | Frem
            deriving (Eq,Ord,Show)

fbinOpMap :: M.Map FbinOp String
fbinOpMap = M.fromList [(Fadd, "fadd"), (Fsub, "fsub"), (Fmul, "fmul"), (Fdiv, "fdiv"), (Frem, "frem")]

data TrapFlag = Nuw | Nsw | Exact deriving (Eq,Ord,Show)

trapFlagMap :: M.Map TrapFlag String
trapFlagMap = M.fromList [(Nuw, "nuw"), (Nsw, "nsw"), (Exact, "exact")]

-- | Binary Operations <http://llvm.org/releases/3.0/docs/LangRef.html#binaryops>
data IbinExpr v = IbinExpr IbinOp [TrapFlag] Type v v deriving (Eq,Ord,Show)
data FbinExpr v = FbinExpr FbinOp FastMathFlags Type v v deriving (Eq,Ord,Show)
data BinExpr v = Ie (IbinExpr v)
               | Fe (FbinExpr v)
               deriving (Eq, Ord, Show)
data GetElementPtr v = GetElementPtr (IsOrIsNot InBounds) (Pointer (Typed v)) [Typed v] deriving (Eq,Ord,Show)
data Select v = Select (Typed v) (Typed v) (Typed v) deriving (Eq,Ord,Show)
data Icmp v = Icmp IcmpOp Type v v deriving (Eq,Ord,Show)
data Fcmp v = Fcmp FcmpOp Type v v deriving (Eq,Ord,Show)


-- | Vector Operations <http://llvm.org/releases/3.0/docs/LangRef.html#vectorops>
data ExtractElement v = ExtractElement (Typed v) (Typed v) deriving (Eq,Ord,Show)
data InsertElement v = InsertElement (Typed v) (Typed v) (Typed v) deriving (Eq,Ord,Show)
data ShuffleVector v = ShuffleVector (Typed v) (Typed v) (Typed v) deriving (Eq,Ord,Show)

-- | Aggregate Operations <http://llvm.org/releases/3.0/docs/LangRef.html#aggregateops>
data ExtractValue v = ExtractValue (Typed v) [Word32] deriving (Eq,Ord,Show)
data InsertValue v = InsertValue (Typed v) (Typed v) [Word32] deriving (Eq,Ord,Show)

-- | Conversion Operations <http://llvm.org/releases/3.0/docs/LangRef.html#convertops>
data Conversion v = Conversion ConvertOp (Typed v) Type deriving (Eq,Ord,Show)

data ConvertOp = Trunc | Zext | Sext | FpTrunc | FpExt | FpToUi
               | FpToSi | UiToFp | SiToFp | PtrToInt | IntToPtr
               | Bitcast | AddrSpaceCast
               deriving (Eq,Ord,Show)

convertOpMap :: M.Map ConvertOp String
convertOpMap = M.fromList [(Trunc, "trunc"), (Zext, "zext"), (Sext, "sext")
                          ,(FpTrunc, "fptrunc"), (FpExt, "fpext"), (FpToUi, "fptoui")
                          ,(FpToSi, "fptosi"), (UiToFp, "uitofp"), (SiToFp, "sitofp")
                          ,(PtrToInt, "ptrtoint"), (IntToPtr, "inttoptr"), (Bitcast, "bitcast")
                          ,(AddrSpaceCast, "addrspacecast")]

-- | Complex Constants <http://llvm.org/releases/3.0/docs/LangRef.html#complexconstants>
data ComplexConstant = Cstruct Packing [TypedConstOrNull]
                     | Carray [TypedConstOrNull]
                     | Cvector [TypedConstOrNull]
                       deriving (Eq,Ord,Show)

-- | Constants <http://llvm.org/releases/3.5.0/docs/LangRef.html#constant-expressions>
data Const = C_simple SimpleConstant
           | C_complex ComplexConstant
           | C_labelId LabelId
           -- | Addresses of Basic Block <http://llvm.org/releases/3.0/docs/LangRef.html#blockaddress>
           | C_blockAddress GlobalId PercentLabel
           | C_binexp (BinExpr Const)
           | C_conv (Conversion Const)
           | C_gep (GetElementPtr Const)
           | C_select (Select Const)
           | C_icmp (Icmp Const)
           | C_fcmp (Fcmp Const)
           | C_shufflevector (ShuffleVector Const)
           | C_extractvalue (ExtractValue Const)
           | C_insertvalue (InsertValue Const)
           | C_extractelement (ExtractElement Const)
           | C_insertelement (InsertElement Const)
           | C_null
           deriving (Eq,Ord,Show)

data Prefix = Prefix (TypedConstOrNull) deriving (Eq, Ord, Show)
data Prologue = Prologue (TypedConstOrNull) deriving (Eq, Ord, Show)

data MdVar = MdVar String deriving (Eq,Ord,Show)
data MdNode = MdNode String deriving (Eq,Ord,Show)
data MetaConst = McStruct [MetaKindedConst]
               | McString DqString
               | McMn MdNode
               | McMv MdVar
               | McRef LocalId
               | McSimple Const
               deriving (Eq,Ord,Show)

data MetaKindedConst = MetaKindedConst MetaKind MetaConst
                     | UnmetaKindedNull
                     deriving (Eq, Ord, Show)

data GetResult = GetResult (Typed Value) String deriving (Eq, Ord, Show)


data Expr = ExprGetElementPtr (GetElementPtr Value)
          | ExprIcmp (Icmp Value)
          | ExprFcmp (Fcmp Value)
          | ExprBinExpr (BinExpr Value)
          | ExprConversion (Conversion Value)
          | ExprSelect (Select Value)
          deriving (Eq,Ord,Show)

-- | Memory Access and Addressing Operations <http://llvm.org/releases/3.5.0/docs/LangRef.html#memory-access-and-addressing-operations>
data MemOp = Alloca (IsOrIsNot InAllocaAttr) Type (Maybe (Typed Value)) (Maybe Alignment)
           | Load (IsOrIsNot Volatile) (Pointer (Typed Value)) (Maybe Alignment) (Maybe Nontemporal)
             (Maybe InvariantLoad) (Maybe Nonnull)
           | LoadAtomic Atomicity (IsOrIsNot Volatile) (Pointer (Typed Value)) (Maybe Alignment)
           | Store (IsOrIsNot Volatile) (Typed Value) (Pointer (Typed Value)) (Maybe Alignment) (Maybe Nontemporal)
           | StoreAtomic Atomicity (IsOrIsNot Volatile) (Typed Value) (Pointer (Typed Value)) (Maybe Alignment)
           | Fence (IsOrIsNot SingleThread) AtomicMemoryOrdering
           | CmpXchg (IsOrIsNot Weak) (IsOrIsNot Volatile) (Pointer (Typed Value)) (Typed Value) (Typed Value)
             (IsOrIsNot SingleThread) AtomicMemoryOrdering AtomicMemoryOrdering
           | AtomicRmw (IsOrIsNot Volatile) AtomicOp (Pointer (Typed Value)) (Typed Value)
             (IsOrIsNot SingleThread) AtomicMemoryOrdering deriving (Eq,Ord,Show)

data Pointer v = Pointer v deriving (Eq, Ord, Show)

instance Functor Pointer where
  fmap f (Pointer x) = Pointer (f x)

data FunName = FunNameGlobal GlobalOrLocalId
             | FunNameBitcast (Typed Const) Type
             | FunNameInttoptr (Typed Const) Type
               -- well, it's a little weird, but LLVM allows this
             | FunName_null
             | FunName_undef
             | FunName_zero
             deriving (Eq,Ord,Show)

data CallSite = CallSiteFun (Maybe CallConv) [ParamAttr] Type FunName [ActualParam] [FunAttr]
              deriving (Eq,Ord,Show)
                       
data InlineAsmExp = InlineAsmExp Type (Maybe SideEffect) (Maybe AlignStack) AsmDialect DqString DqString [ActualParam] [FunAttr]
                  deriving (Eq, Ord, Show)

data Clause = ClauseCatch (Typed Value)
            | ClauseFilter TypedConstOrNull
            | ClauseConversion (Conversion Value)
            deriving (Eq,Ord,Show)

data TypedConstOrNull = TypedConst (Typed Const)
                      | UntypedNull
                      deriving (Eq, Ord, Show)

data Rhs = RhsMemOp MemOp
         | RhsExpr Expr
         | RhsCall TailCall CallSite
         | RhsInlineAsm InlineAsmExp
         | RhsExtractElement (ExtractElement Value)
         | RhsInsertElement (InsertElement Value)
         | RhsShuffleVector (ShuffleVector Value)
         | RhsExtractValue (ExtractValue Value)
         | RhsInsertValue (InsertValue Value)
         | RhsVaArg VaArg
         | RhsLandingPad LandingPad
         deriving (Eq,Ord,Show)

data VaArg = VaArg (Typed Value) Type deriving (Eq, Ord, Show)

data LandingPad = LandingPad Type Type FunName (Maybe Cleanup) [Clause] deriving (Eq, Ord, Show)

data Dbg = Dbg MdVar MetaConst deriving (Eq,Show)

data PhiInst = PhiInst (Maybe LocalId) Type [(Value, PercentLabel)] deriving (Eq,Show)

data PhiInstWithDbg = PhiInstWithDbg PhiInst [Dbg] deriving (Eq, Show)

data ComputingInst = ComputingInst (Maybe LocalId) Rhs deriving (Eq,Show)
                                                                
data ComputingInstWithDbg = ComputingInstWithDbg ComputingInst [Dbg]
                          | ComputingInstWithComment String
                          deriving (Eq,Show)

-- | Terminator Instructions <http://llvm.org/releases/3.0/docs/LangRef.html#terminators>
data TerminatorInst =
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_ret>
    RetVoid
    | Return [(Typed Value)]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_br>
    | Br TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_br>
    | Cbr Value TargetLabel TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_switch>
    | Switch (Typed Value) TargetLabel [((Typed Value), TargetLabel)]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_indirectbr>
    | IndirectBr (Typed Value) [TargetLabel]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_invoke>
    | Invoke (Maybe LocalId) CallSite TargetLabel TargetLabel
    | InvokeInlineAsm (Maybe LocalId) InlineAsmExp TargetLabel TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_unwind>
    | Unwind
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_resume>
    | Resume (Typed Value)
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_unreachable>
    | Unreachable
      deriving (Eq,Show)

data TerminatorInstWithDbg = TerminatorInstWithDbg TerminatorInst [Dbg]
                             deriving (Eq,Show)

data ActualParam = ActualParamData Type [ParamAttr] (Maybe Alignment) Value [ParamAttr]
                 | ActualParamLabel Type [ParamAttr] (Maybe Alignment) PercentLabel [ParamAttr]
                 | ActualParamMeta MetaKindedConst
                 deriving (Eq,Ord,Show)

data Value = Val_local LocalId
           | Val_const Const
           deriving (Eq,Ord,Show)

data Typed v = Typed Type v deriving (Eq, Ord, Show)

data Aliasee = AliaseeTv (Typed Value)
             | AliaseeConversion (Conversion Const)
             | AliaseeGetElementPtr (GetElementPtr Const)
             deriving (Eq,Show)

data FunctionPrototype = FunctionPrototype
                         (Maybe Linkage)
                         (Maybe Visibility)
                         (Maybe DllStorageClass)
                         (Maybe CallConv)
                         [ParamAttr]
                         Type
                         GlobalId
                         FormalParamList
                         (Maybe AddrNaming)
                         [FunAttr]
                         (Maybe Section)
                         (Maybe Comdat)
                         (Maybe Alignment)
                         (Maybe Gc)
                         (Maybe Prefix)
                         (Maybe Prologue)
                       deriving (Eq,Ord,Show)


data Toplevel = ToplevelTriple TlTriple
              | ToplevelDataLayout TlDataLayout
              | ToplevelAlias TlAlias
              | ToplevelDbgInit TlDbgInit
              | ToplevelStandaloneMd TlStandaloneMd
              | ToplevelNamedMd TlNamedMd
              | ToplevelDeclare TlDeclare
              | ToplevelDefine TlDefine
              | ToplevelGlobal TlGlobal
              | ToplevelTypeDef TlTypeDef
              | ToplevelDepLibs TlDepLibs
              | ToplevelUnamedType TlUnamedType
              | ToplevelModuleAsm TlModuleAsm
              | ToplevelAttribute TlAttribute
              | ToplevelComdat TlComdat
              deriving (Eq,Show)


data TlTriple = TlTriple TargetTriple deriving (Eq, Show)

data TlDataLayout = TlDataLayout DataLayout deriving (Eq, Show)

data TlAlias = TlAlias GlobalId (Maybe Visibility) (Maybe DllStorageClass) (Maybe ThreadLocalStorage)
               AddrNaming (Maybe Linkage) Aliasee deriving (Eq, Show)

data TlDbgInit = TlDbgInit String Word32 deriving (Eq, Show)

data TlStandaloneMd = TlStandaloneMd String MetaKindedConst deriving (Eq, Show)

data TlNamedMd = TlNamedMd MdVar [MdNode] deriving (Eq, Show)

data TlDeclare = TlDeclare FunctionPrototype deriving (Eq, Show)

data TlDefine = TlDefine FunctionPrototype [Block] deriving (Eq, Show)

data TlGlobal = TlGlobal (Maybe GlobalId)
                (Maybe Linkage)
                (Maybe Visibility)
                (Maybe DllStorageClass)
                (Maybe ThreadLocalStorage)
                AddrNaming
                (Maybe AddrSpace)
                (IsOrIsNot ExternallyInitialized)
                GlobalType
                Type
                (Maybe Const)
                (Maybe Section)
                (Maybe Comdat)
                (Maybe Alignment)
              deriving (Eq, Show)

data TlTypeDef = TlTypeDef LocalId Type deriving (Eq, Show)

data TlDepLibs = TlDepLibs [DqString] deriving (Eq, Show)

data TlUnamedType = TlUnamedType Word32 Type deriving (Eq, Show)

data TlModuleAsm = TlModuleAsm DqString deriving (Eq, Show)

data TlAttribute = TlAttribute Word32 [FunAttr] deriving (Eq, Show)

data TlComdat = TlComdat DollarId SelectionKind deriving (Eq, Show)

data Block = Block BlockLabel [PhiInstWithDbg] [ComputingInstWithDbg] TerminatorInstWithDbg
           deriving (Eq,Show)

blockLabel :: Block -> BlockLabel
blockLabel (Block v _ _ _) = v

data Module = Module [Toplevel] deriving (Eq,Show)

typeDefOfModule :: Module -> [(LocalId, Type)]
typeDefOfModule (Module tl) =
  let tl0 = filter (\x -> case x of
                       ToplevelTypeDef _ -> True
                       _ -> False
                   ) tl
  in fmap (\(ToplevelTypeDef (TlTypeDef lid ty)) -> (lid,ty)) tl0
