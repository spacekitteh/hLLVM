{-# LANGUAGE GADTs #-}
module Llvm.Data.CoreIr
       ( module Llvm.Data.CoreIr
       , module Llvm.Data.Shared
       , Label
       ) where
import Llvm.Data.Shared
import Compiler.Hoopl (Label)

-- | We have to keep the original label kinds to map Hoopl labels back to labels that llvm-as likes
data LabelId = LabelString Label
             | LabelNumber Label
             | LabelDqString Label -- a string enclosed by double quotes
             | LabelDqNumber Label -- a number enclosed by double quotes
             deriving (Eq, Ord, Show)

hooplLabelOf :: LabelId -> Label
hooplLabelOf (LabelString x) = x
hooplLabelOf (LabelDqString x) = x
hooplLabelOf (LabelNumber x) = x
hooplLabelOf (LabelDqNumber x) = x

data BlockLabel = BlockLabel LabelId deriving (Eq, Ord, Show)
data PercentLabel = PercentLabel LabelId deriving (Eq, Ord, Show)
data TargetLabel = TargetLabel PercentLabel deriving (Eq,Ord,Show)


data NoWrap =
  -- | No Signed Wrap
  Nsw
  -- | No Unsigned Wrap
  | Nuw
  -- | No Signed and Unsigned Wrap
  | Nsuw deriving (Eq,Ord,Show)

data Exact = Exact deriving (Eq,Ord,Show)

data IbinExpr v = Add (Maybe NoWrap) Type v v
                | Sub (Maybe NoWrap) Type v v
                | Mul (Maybe NoWrap) Type v v
                | Udiv (Maybe Exact) Type v v
                | Sdiv (Maybe Exact) Type v v
                | Urem Type v v
                | Srem Type v v
                | Shl (Maybe NoWrap) Type v v
                | Lshr (Maybe Exact) Type v v
                | Ashr (Maybe Exact) Type v v
                | And Type v v
                | Or Type v v
                | Xor Type v v
                deriving (Eq,Ord,Show)

operandOfIbinExpr :: IbinExpr v -> (v, v)
operandOfIbinExpr e = case e of
  Add _ _ v1 v2 -> (v1, v2)
  Sub _ _ v1 v2 -> (v1, v2)
  Mul _ _ v1 v2 -> (v1, v2)
  Udiv _ _ v1 v2 -> (v1, v2)
  Sdiv _ _ v1 v2 -> (v1, v2)
  Urem _ v1 v2 -> (v1, v2)
  Srem _ v1 v2 -> (v1, v2)
  Shl _ _ v1 v2 -> (v1, v2)
  Lshr _ _ v1 v2 -> (v1, v2)
  Ashr _ _ v1 v2 -> (v1, v2)
  And  _ v1 v2 -> (v1, v2)
  Or  _ v1 v2 -> (v1, v2)
  Xor _ v1 v2 -> (v1, v2)

data FbinExpr v = Fadd FastMathFlags Type v v
                | Fsub FastMathFlags Type v v
                | Fmul FastMathFlags Type v v
                | Fdiv FastMathFlags Type v v
                | Frem FastMathFlags Type v v
                deriving (Eq,Ord,Show)

data BinExpr v = Ie (IbinExpr v)
               | Fe (FbinExpr v)
               deriving (Eq, Ord, Show)

operandOfFbinExpr :: FbinExpr v -> (v, v)
operandOfFbinExpr e = case e of
  Fadd _ _ v1 v2 -> (v1, v2)
  Fsub _ _ v1 v2 -> (v1, v2)
  Fmul _ _ v1 v2 -> (v1, v2)
  Fdiv _ _ v1 v2 -> (v1, v2)
  Frem _ _ v1 v2 -> (v1, v2)

operandOfBinExpr :: BinExpr v -> (v, v)
operandOfBinExpr (Ie x) = operandOfIbinExpr x
operandOfBinExpr (Fe x) = operandOfFbinExpr x

operatorOfIbinExpr :: IbinExpr v -> String
operatorOfIbinExpr e = case e of
  Add _ _ _ _ -> "add"
  Sub _ _ _ _ -> "sub"
  Mul _ _ _ _ -> "mul"
  Udiv _ _ _ _ -> "udiv"
  Sdiv _ _ _ _ -> "sdiv"
  Urem _ _ _ -> "urem"
  Srem _ _ _ -> "srem"
  Shl _ _ _ _ -> "shl"
  Lshr _ _ _ _ -> "lshr"
  Ashr _ _ _ _ -> "ashr"
  And  _ _ _ -> "and"
  Or  _ _ _ -> "or"
  Xor _ _ _ -> "xor"

operatorOfFbinExpr :: FbinExpr v -> String
operatorOfFbinExpr e = case e of
  Fadd _ _ _ _ -> "fadd"
  Fsub _ _ _ _ -> "fsub"
  Fmul _ _ _ _ -> "fmul"
  Fdiv _ _ _ _ -> "fdiv"
  Frem _ _ _ _ -> "frem"


operatorOfBinExpr :: BinExpr v -> String
operatorOfBinExpr (Ie e) = operatorOfIbinExpr e
operatorOfBinExpr (Fe e) = operatorOfFbinExpr e


typeOfIbinExpr :: IbinExpr v -> Type
typeOfIbinExpr e = case e of
  Add _ t _ _ -> t
  Sub _ t _ _ -> t
  Mul _ t _ _ -> t
  Udiv _ t _ _ -> t
  Sdiv _ t _ _ -> t
  Urem t _ _ -> t
  Srem t _ _ -> t
  Shl _ t _ _ -> t
  Lshr _ t _ _ -> t
  Ashr _ t _ _ -> t
  And  t _ _ -> t
  Or  t _ _ -> t
  Xor t _ _ -> t

typeOfFbinExpr :: FbinExpr v -> Type
typeOfFbinExpr e = case e of
  Fadd _ t _ _ -> t
  Fsub _ t _ _ -> t
  Fmul _ t _ _ -> t
  Fdiv _ t _ _ -> t
  Frem _ t _ _ -> t

typeOfBinExpr :: BinExpr v -> Type
typeOfBinExpr (Ie e) = typeOfIbinExpr e
typeOfBinExpr (Fe e) = typeOfFbinExpr e


data Conversion v = Conversion ConvertOp v Type deriving (Eq,Ord,Show)
-- | getelemptr addr:u1 takenaddr indices:u1
data GetElemPtr v = GetElemPtr (IsOrIsNot InBounds) v [v] deriving (Eq,Ord,Show)
data Select v = Select v v v deriving (Eq,Ord,Show)
data Icmp v = Icmp IcmpOp Type v v deriving (Eq,Ord,Show)
data Fcmp v = Fcmp FcmpOp Type v v deriving (Eq,Ord,Show)
data ShuffleVector v = ShuffleVector v v v deriving (Eq,Ord,Show)
data ExtractValue v = ExtractValue v [String] deriving (Eq,Ord,Show)
data InsertValue v = InsertValue v v [String] deriving (Eq,Ord,Show)
data ExtractElem v = ExtractElem v v deriving (Eq,Ord,Show)
data InsertElem v = InsertElem v v v deriving (Eq,Ord,Show)


typeOfIcmp :: Icmp v -> Type
typeOfIcmp (Icmp _ t _ _) = t

typeOfFcmp :: Fcmp v -> Type
typeOfFcmp (Fcmp _ t _ _) = t


data ComplexConstant = Cstruct Packing [(Typed Const)]
                     | Cvector [(Typed Const)]
                     | CvectorN Integer (Typed Const) -- repeat the same const N times
                     | Carray [(Typed Const)]
                     | CarrayN Integer (Typed Const) -- repeat the same const N times
                     deriving (Eq, Ord, Show)

data Const = Ccp SimpleConstant
           | Cca ComplexConstant
           | CmL LocalId
           | Cl LabelId
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
           deriving (Eq,Ord,Show)

data MdVar = MdVar String deriving (Eq,Ord,Show)
data MdNode = MdNode String deriving (Eq,Ord,Show)
data MetaConst = MdConst Const
               | MdString DqString
               | McMn MdNode
               | McMv MdVar
               | MdRef LocalId
               deriving (Eq,Ord,Show)

data Expr = EgEp (GetElemPtr (Typed Value))
          | EiC (Icmp Value)
          | EfC (Fcmp Value)
          | Eb (BinExpr Value)
          | Ec (Conversion (Typed Value))
          | Es (Select (Typed Value))
          -- | Internal value to make the optimization easier
          | Ev (Typed Value)
          deriving (Eq,Ord,Show)


typeOfExpr :: Expr -> Type
typeOfExpr (Ev (TypedData t _)) = t
typeOfExpr x = error ("unexpected parameter " ++ (show x) ++ " is passed to typeOfExpr.")

{-   (element type, element number, align) -}
data MemOp =
  -- | allocate m t s:u1 align
  Allocate (IsOrIsNot InAllocaAttr) Type (Maybe (Typed Value)) (Maybe Alignment)
  -- | load a addr:u1u2 align
  | Load (IsOrIsNot Volatile) (Typed Pointer) (Maybe Alignment) (Maybe Nontemporal) (Maybe InvariantLoad) (Maybe Nonnull)
  | LoadAtomic Atomicity (IsOrIsNot Volatile) (Typed Pointer) (Maybe Alignment)
  -- | store v:u1 addr:u1d2 align
  | Store (IsOrIsNot Volatile) (Typed Value) (Typed Pointer) (Maybe Alignment) (Maybe Nontemporal)
  | StoreAtomic Atomicity (IsOrIsNot Volatile) (Typed Value) (Typed Pointer) (Maybe Alignment)
  -- | fence
  | Fence (IsOrIsNot SingleThread) AtomicMemoryOrdering
  -- | cmpxchg v1:u1u2d2 v2:u1 v3:u1
  | CmpXchg (IsOrIsNot Weak) (IsOrIsNot Volatile) (Typed Pointer) (Typed Value) (Typed Value) (IsOrIsNot SingleThread) AtomicMemoryOrdering AtomicMemoryOrdering
  -- | atomicrmw v1:u1u2d2 v2:u1
  | AtomicRmw (IsOrIsNot Volatile) AtomicOp (Typed Pointer) (Typed Value) (IsOrIsNot SingleThread) AtomicMemoryOrdering
  deriving (Eq,Ord,Show)

data FunName =
  -- | fn:u1
  FunNameGlobal GlobalOrLocalId
  | FunNameString String
    deriving (Eq,Ord,Show)

data CallSite = CsFun (Maybe CallConv) [ParamAttr] Type FunName [ActualParam] [FunAttr]
              | CsAsm Type (Maybe SideEffect) (Maybe AlignStack) AsmDialect DqString DqString [ActualParam] [FunAttr]
              | CsConversion [ParamAttr] Type (Conversion (Typed Const)) [ActualParam] [FunAttr]
              deriving (Eq,Ord,Show)

data Clause = Catch (Typed Value) -- u1
            | Filter (Typed Const) -- u1
            | Cco (Conversion (Typed Value))
            deriving (Eq,Ord,Show)

data PersFn = PersFnId GlobalOrLocalId -- u1
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

-- | PhiInst (d1) [u1]
data PhiInst = PhiInst (Maybe GlobalOrLocalId) Type [(Value, PercentLabel)] deriving (Eq,Show)

data PhiInstWithDbg = PhiInstWithDbg PhiInst [Dbg] deriving (Eq,Show)

-- | ComputingInst (d1) Rhs
data ComputingInst = ComputingInst (Maybe GlobalOrLocalId) Rhs deriving (Eq,Show)

data ComputingInstWithDbg = ComputingInstWithDbg ComputingInst [Dbg]
                          deriving (Eq,Show)

data TerminatorInst = Unreachable
                    | Return [(Typed Value)] -- u1
                    | Br TargetLabel
                    | Cbr Value TargetLabel TargetLabel -- u1
                    | IndirectBr (Typed Value) [TargetLabel]
                    | Switch (Typed Value) TargetLabel [((Typed Value), TargetLabel)]
                    -- | invoke d1 callsite t1 t2
                    | Invoke (Maybe GlobalOrLocalId) CallSite TargetLabel TargetLabel
                    | Resume (Typed Value)
                    | Unwind
                    deriving (Eq,Show)


targetOf :: TerminatorInst -> [TargetLabel]
targetOf (Unreachable) = []
targetOf (Return _) = []
targetOf (Br t) = [t]
targetOf (Cbr _ t f) = [t,f]
targetOf (IndirectBr _ ls) = ls
targetOf (Switch _ d cases) = d:(map snd cases)
targetOf (Invoke _ _ t1 t2) = [t1,t2]
targetOf (Resume _) = []
targetOf Unwind = []

data TerminatorInstWithDbg = TerminatorInstWithDbg TerminatorInst [Dbg] deriving (Eq,Show)

data ActualParam = ActualParam Type [ParamAttr] (Maybe Alignment) Value [ParamAttr] deriving (Eq,Ord,Show)


data Value = VgOl GlobalOrLocalId
           | Ve Expr
           | Vc Const
           -- | Internal value to make the optimization easier
           | Deref Pointer
           deriving (Eq,Ord,Show)

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

data Pointer = Pointer Value deriving (Eq, Ord, Show)

data Aliasee = AtV (Typed Value) -- u1
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

data Prefix = Prefix (Typed Const) deriving (Eq, Ord, Show)
data Prologue = Prologue (Typed Const) deriving (Eq, Ord, Show)

getTargetLabel :: TargetLabel -> Label
getTargetLabel (TargetLabel (PercentLabel l)) = toLabel l


toLabel :: LabelId -> Label
toLabel (LabelString l) = l
toLabel (LabelDqString l) = l
toLabel (LabelNumber l) = l
toLabel (LabelDqNumber l) = l

