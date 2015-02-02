{-# LANGUAGE GADTs #-}
module Llvm.Data.Ast
       ( module Llvm.Data.Ast
       , module Llvm.Data.Shared
       ) where

import Llvm.Data.Shared
import qualified Data.Map as M


-- | quotation does not change a label value
-- | it's still unclear when a quoted verion is used  
-- | we keep the original format to make llvm-as happy 
data LabelId = LabelString Lstring
             | LabelDqString Lstring -- a string enclosed by double quotes
             | LabelNumber Integer
             | LabelDqNumber Integer -- a number enclosed by double quotes
             deriving (Eq,Ord,Show)

labelIdToLstring :: LabelId -> Lstring
labelIdToLstring (LabelString s) = s
labelIdToLstring (LabelDqString s) = s
labelIdToLstring (LabelNumber n) = Lstring $ show n
labelIdToLstring (LabelDqNumber n) = Lstring $ show n

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
ibinOpMap = M.fromList [(Add, "add"), (Sub, "sub"), (Mul, "mul"), (Udiv, "udiv"), (Sdiv, "sdiv")
                       ,(Urem, "urem"), (Srem, "srem")
                       , (Shl, "shl"), (Lshr, "lshr"), (Ashr, "ashr")
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
data GetElemPtr v = GetElemPtr (IsOrIsNot InBounds) v [v] deriving (Eq,Ord,Show)
data Select v = Select v v v deriving (Eq,Ord,Show)
data Icmp v = Icmp IcmpOp Type v v deriving (Eq,Ord,Show)
data Fcmp v = Fcmp FcmpOp Type v v deriving (Eq,Ord,Show)


-- | Vector Operations <http://llvm.org/releases/3.0/docs/LangRef.html#vectorops>
data ExtractElem v = ExtractElem v v deriving (Eq,Ord,Show)
data InsertElem v = InsertElem v v v deriving (Eq,Ord,Show)
data ShuffleVector v = ShuffleVector v v v deriving (Eq,Ord,Show)

-- | Aggregate Operations <http://llvm.org/releases/3.0/docs/LangRef.html#aggregateops>
data ExtractValue v = ExtractValue v [String] deriving (Eq,Ord,Show)
data InsertValue v = InsertValue v v [String] deriving (Eq,Ord,Show)

-- | Conversion Operations <http://llvm.org/releases/3.0/docs/LangRef.html#convertops>
data Conversion v = Conversion ConvertOp v Type deriving (Eq,Ord,Show)

-- | Complex Constants <http://llvm.org/releases/3.0/docs/LangRef.html#complexconstants>  
data ComplexConstant = Cstruct Packing [(Typed Const)] 
                     | Carray [(Typed Const)]
                     | Cvector [(Typed Const)]
                       deriving (Eq,Ord,Show)
                         
-- | Constants <http://llvm.org/releases/3.5.0/docs/LangRef.html#constant-expressions>
data Const = Ccp SimpleConstant
           | Cca ComplexConstant
           | CmL LocalId
           | Cl LabelId
           -- | Addresses of Basic Block <http://llvm.org/releases/3.0/docs/LangRef.html#blockaddress>
           | CblockAddress GlobalId PercentLabel
           | Cb (BinExpr Const)
           | Cconv (Conversion (Typed Const))
           | CgEp (GetElemPtr (Typed Const))
           | Cs (Select (Typed Const))
           | CiC (Icmp Const)
           | CfC (Fcmp Const)
           | CsV (ShuffleVector (Typed Const))
           | CeV (ExtractValue (Typed Const))
           | CiV (InsertValue (Typed Const))
           | CeE (ExtractElem (Typed Const))
           | CiE (InsertElem (Typed Const))
           | CmC MetaConst
           | Cnull
           deriving (Eq,Ord,Show)
                    
data Prefix = Prefix (Typed Const) deriving (Eq, Ord, Show)
data Prologue = Prologue (Typed Const) deriving (Eq, Ord, Show)

data MdVar = MdVar String deriving (Eq,Ord,Show)
data MdNode = MdNode String deriving (Eq,Ord,Show)
data MetaConst = MdConst Const
               | MdString DqString
               | McMn MdNode
               | McMv MdVar
               | MdRef LocalId
               deriving (Eq,Ord,Show)
                        
data GetResult = GetResult (Typed Value) String deriving (Eq, Ord, Show)


data Expr = EgEp (GetElemPtr (Typed Value))
          | EiC (Icmp Value)
          | EfC (Fcmp Value)
          | Eb (BinExpr Value) 
          | Ec (Conversion (Typed Value))
          | Es (Select (Typed Value))
          deriving (Eq,Ord,Show)
                   

-- | Memory Access and Addressing Operations <http://llvm.org/releases/3.5.0/docs/LangRef.html#memory-access-and-addressing-operations>
data MemOp = Alloca (IsOrIsNot InAllocaAttr) Type (Maybe (Typed Value)) (Maybe Alignment)
           | Load (IsOrIsNot Volatile) (Typed Pointer) (Maybe Alignment) (Maybe Nontemporal) (Maybe InvariantLoad) (Maybe Nonnull)
           | LoadAtomic Atomicity (IsOrIsNot Volatile) (Typed Pointer) (Maybe Alignment) 
           | Store (IsOrIsNot Volatile) (Typed Value) (Typed Pointer) (Maybe Alignment) (Maybe Nontemporal)
           | StoreAtomic Atomicity (IsOrIsNot Volatile) (Typed Value) (Typed Pointer) (Maybe Alignment)
           | Fence (IsOrIsNot SingleThread) AtomicMemoryOrdering
           | CmpXchg (IsOrIsNot Weak) (IsOrIsNot Volatile) (Typed Pointer) (Typed Value) (Typed Value) (IsOrIsNot SingleThread) AtomicMemoryOrdering AtomicMemoryOrdering
           | AtomicRmw (IsOrIsNot Volatile) AtomicOp (Typed Pointer) (Typed Value) (IsOrIsNot SingleThread) AtomicMemoryOrdering deriving (Eq,Ord,Show)


data Pointer = Pointer Value deriving (Eq, Ord, Show)
-- data TypedPointer = TypedPointer Type Pointer deriving (Eq, Ord, Show)
                         
data FunName = FunNameGlobal GlobalOrLocalId
             | FunNameString String
               deriving (Eq,Ord,Show)
             
data CallSite = CsFun (Maybe CallConv) [ParamAttr] Type FunName [ActualParam] [FunAttr]
              | CsAsm Type (Maybe SideEffect) (Maybe AlignStack) AsmDialect DqString DqString [ActualParam] [FunAttr] 
              | CsConversion [ParamAttr] Type (Conversion (Typed Const)) [ActualParam] [FunAttr]
              deriving (Eq,Ord,Show)
                       
data Clause = Catch (Typed Value)
            | Filter (Typed Const)
            | Cco (Conversion (Typed Value))
            deriving (Eq,Ord,Show)

data PersFn = PersFnId GlobalOrLocalId
            | PersFnCast (Conversion (Type, GlobalOrLocalId))
            | PersFnUndef
            | PersFnNull
            | PersFnConst Const
            deriving (Eq, Ord, Show)
                     
                           
data Rhs = RmO MemOp
         | Re Expr
         | Call TailCall CallSite
         | ReE (ExtractElem (Typed Value))
         | RiE (InsertElem (Typed Value))
         | RsV (ShuffleVector (Typed Value))
         | ReV (ExtractValue (Typed Value)) 
         | RiV (InsertValue (Typed Value))
         | VaArg (Typed Value) Type
         | LandingPad Type Type PersFn (Maybe Cleanup) [Clause] 
         deriving (Eq,Ord,Show)
              
data Dbg = Dbg MdVar MetaConst deriving (Eq,Show)

data PhiInst = PhiInst (Maybe GlobalOrLocalId) Type 
               [(Value, PercentLabel)] deriving (Eq,Show)
    
data PhiInstWithDbg = PhiInstWithDbg PhiInst [Dbg]
                      deriving (Eq, Show)
                               
data ComputingInst = ComputingInst (Maybe GlobalOrLocalId) Rhs
                     deriving (Eq,Show)
                              
data ComputingInstWithDbg = ComputingInstWithDbg ComputingInst [Dbg]
                            deriving (Eq,Show)
                               
-- | Terminator Instructions <http://llvm.org/releases/3.0/docs/LangRef.html#terminators>      
data TerminatorInst = 
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_ret>
    Return [(Typed Value)]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_br>
    | Br TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_br>
    | Cbr Value TargetLabel TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_switch>
    | Switch (Typed Value) TargetLabel [((Typed Value), TargetLabel)]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_indirectbr>
    | IndirectBr (Typed Value) [TargetLabel]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_invoke>
    | Invoke (Maybe GlobalOrLocalId) CallSite TargetLabel TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_unwind>
    | Unwind
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_resume>
    | Resume (Typed Value)
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_unreachable>
    | Unreachable
      deriving (Eq,Show)
                               
data TerminatorInstWithDbg = TerminatorInstWithDbg TerminatorInst [Dbg] 
                             deriving (Eq,Show)
                                      
data ActualParam = ActualParam Type [ParamAttr] (Maybe Alignment) Value [ParamAttr]
                 deriving (Eq,Ord,Show)
                          
                                            
data Value = VgOl GlobalOrLocalId
           | Ve Expr
           | Vc Const
           deriving (Eq,Ord,Show)

{-
data Typed v = Typed (Maybe Type) v deriving (Eq, Ord, Show)
-}

data Typed v where
  TypedData :: Type -> v -> Typed v 
  UntypedNull :: Typed Const

instance Eq v => Eq (Typed v) where
  (==) (TypedData t1 v1) (TypedData t2 v2) = (t1 == t2) && (v1 == v2)
  (==) UntypedNull UntypedNull = undefined
  (==) _ _ = False
  
instance Show v => Show (Typed v) where  
  show (TypedData t1 v1) = "TypedData " ++ (show t1) ++ " " ++ (show v1)
  show UntypedNull = "UntypedNull"
  
instance Ord v => Ord (Typed v) where  
  compare (TypedData t1 v1) (TypedData t2 v2) = let e1 = compare t1 t2
                                                in if e1 == EQ then compare v1 v2
                                                   else e1                                                        
  compare UntypedNull UntypedNull = undefined
  compare UntypedNull _ = LT
-- data TypedValue = TypedValue Type Value deriving (Eq,Ord,Show)
{-
data TypedConst = TypedConst Type Const 
                | TypedConstNull 
                deriving (Eq,Ord,Show)
-}
        
                                                
data Aliasee = AtV (Typed Value)
             | Ac (Conversion (Typed Const))
             | AgEp (GetElemPtr (Typed Const))
             deriving (Eq,Show)
                        
data FunctionPrototype = FunctionPrototype
                         (Maybe Linkage)
                         (Maybe Visibility)
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


data Toplevel = ToplevelTriple DqString
              | ToplevelDataLayout DataLayout
              | ToplevelAlias GlobalId (Maybe Visibility) (Maybe DllStorageClass) (Maybe ThreadLocalStorage) AddrNaming (Maybe Linkage) Aliasee
              | ToplevelDbgInit String Integer
              | ToplevelStandaloneMd String (Typed Value)
              | ToplevelNamedMd MdVar [MdNode]
              | ToplevelDeclare FunctionPrototype
              | ToplevelDefine FunctionPrototype [Block]
              | ToplevelGlobal 
                (Maybe GlobalId)
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
              | ToplevelTypeDef LocalId Type
              | ToplevelDepLibs [DqString]
              | ToplevelUnamedType Integer Type
              | ToplevelModuleAsm DqString
              | ToplevelAttribute Integer [FunAttr]
              | ToplevelComdat DollarId SelectionKind
              deriving (Eq,Show)
                       
data Block = Block BlockLabel [PhiInstWithDbg] [ComputingInstWithDbg] TerminatorInstWithDbg deriving (Eq,Show)

blockLabel :: Block -> BlockLabel
blockLabel (Block v _ _ _) = v

data Module = Module [Toplevel] deriving (Eq,Show)


