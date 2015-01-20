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
data ComplexConstant = Cstruct Packing [TypedConst] 
                     | Carray [TypedConst]
                     | Cvector [TypedConst]
                       deriving (Eq,Ord,Show)
                         
-- | Constants <http://llvm.org/releases/3.5.0/docs/LangRef.html#constant-expressions>
data Const = Ccp SimpleConstant
           | Cca ComplexConstant
           | CmL LocalId
           | Cl LabelId
           -- | Addresses of Basic Block <http://llvm.org/releases/3.0/docs/LangRef.html#blockaddress>
           | CblockAddress GlobalId PercentLabel
           | Cb (BinExpr Const)
           | Cconv (Conversion TypedConst)
           | CgEp (GetElemPtr TypedConst)
           | Cs (Select TypedConst)
           | CiC (Icmp Const)
           | CfC (Fcmp Const)
           | CsV (ShuffleVector TypedConst)
           | CeV (ExtractValue TypedConst)
           | CiV (InsertValue TypedConst)
           | CeE (ExtractElem TypedConst)
           | CiE (InsertElem TypedConst)
           | CmC MetaConst
           deriving (Eq,Ord,Show)
                    
data Prefix = Prefix TypedConst deriving (Eq, Ord, Show)
data Prologue = Prologue TypedConst deriving (Eq, Ord, Show)

data MdVar = MdVar String deriving (Eq,Ord,Show)
data MdNode = MdNode String deriving (Eq,Ord,Show)
data MetaConst = MdConst Const
               | MdString DqString
               | McMn MdNode
               | McMv MdVar
               | MdRef LocalId
               deriving (Eq,Ord,Show)
                        
data GetResult = GetResult TypedValue String deriving (Eq, Ord, Show)


data Expr = EgEp (GetElemPtr TypedValue)
          | EiC (Icmp Value)
          | EfC (Fcmp Value)
          | Eb (BinExpr Value) 
          | Ec (Conversion TypedValue)
          | Es (Select TypedValue)
          deriving (Eq,Ord,Show)
                   

-- | Memory Access and Addressing Operations <http://llvm.org/releases/3.5.0/docs/LangRef.html#memory-access-and-addressing-operations>
data MemOp = Alloca (IsOrIsNot InAllocaAttr) Type (Maybe TypedValue) (Maybe Alignment)
           | Load (IsOrIsNot Volatile) TypedPointer (Maybe Alignment) (Maybe Nontemporal) (Maybe InvariantLoad) (Maybe Nonnull)
           | LoadAtomic Atomicity (IsOrIsNot Volatile) TypedPointer (Maybe Alignment) 
           | Store (IsOrIsNot Volatile) TypedValue TypedPointer (Maybe Alignment) (Maybe Nontemporal)
           | StoreAtomic Atomicity (IsOrIsNot Volatile) TypedValue TypedPointer (Maybe Alignment)
           | Fence (IsOrIsNot SingleThread) AtomicMemoryOrdering
           | CmpXchg (IsOrIsNot Weak) (IsOrIsNot Volatile) TypedPointer TypedValue TypedValue (IsOrIsNot SingleThread) AtomicMemoryOrdering AtomicMemoryOrdering
           | AtomicRmw (IsOrIsNot Volatile) AtomicOp TypedPointer TypedValue (IsOrIsNot SingleThread) AtomicMemoryOrdering deriving (Eq,Ord,Show)


data Pointer = Pointer Value deriving (Eq, Ord, Show)
data TypedPointer = TypedPointer Type Pointer deriving (Eq, Ord, Show)
                         
data FunName = FunNameGlobal GlobalOrLocalId
             | FunNameString String
               deriving (Eq,Ord,Show)
             
data CallSite = CsFun (Maybe CallConv) [ParamAttr] Type FunName [ActualParam] [FunAttr]
              | CsAsm Type (Maybe SideEffect) (Maybe AlignStack) AsmDialect DqString DqString [ActualParam] [FunAttr] 
              | CsConversion [ParamAttr] Type (Conversion TypedConst) [ActualParam] [FunAttr]
              deriving (Eq,Ord,Show)
                       
data Clause = Catch TypedValue
            | Filter TypedConst
            | Cco (Conversion TypedValue)
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
         | ReE (ExtractElem TypedValue)
         | RiE (InsertElem TypedValue)
         | RsV (ShuffleVector TypedValue)
         | ReV (ExtractValue TypedValue) 
         | RiV (InsertValue TypedValue)
         | VaArg TypedValue Type
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
    Return [TypedValue]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_br>
    | Br TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_br>
    | Cbr Value TargetLabel TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_switch>
    | Switch TypedValue TargetLabel [(TypedValue, TargetLabel)]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_indirectbr>
    | IndirectBr TypedValue [TargetLabel]
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_invoke>
    | Invoke (Maybe GlobalOrLocalId) CallSite TargetLabel TargetLabel
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_unwind>
    | Unwind
    -- | <http://llvm.org/releases/3.0/docs/LangRef.html#i_resume>
    | Resume TypedValue
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

data TypedValue = TypedValue Type Value deriving (Eq,Ord,Show)
data TypedConst = TypedConst Type Const 
                | TypedConstNull 
                deriving (Eq,Ord,Show)
        
                                                
data Aliasee = AtV TypedValue
             | Ac (Conversion TypedConst)
             | AgEp (GetElemPtr TypedConst)
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
              | ToplevelStandaloneMd String TypedValue
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


