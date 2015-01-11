module Llvm.VmCore.Ast 
       ( module Llvm.VmCore.Ast
       , module Llvm.VmCore.SharedEntity
       , module Llvm.VmCore.DataLayout
       ) where

import Llvm.VmCore.SharedEntity
import Llvm.VmCore.DataLayout


-- | quotation does not change a label value
-- | it's still unclear when a quoted verion is used  
-- | we keep the original format to make llvm-as happy 
data LabelId = LabelString Lstring
             | LabelQuoteString Lstring
             | LabelNumber Integer
             | LabelQuoteNumber Integer
             deriving (Eq,Ord,Show)

labelIdToLstring :: LabelId -> Lstring
labelIdToLstring (LabelString s) = s
labelIdToLstring (LabelQuoteString s) = s
labelIdToLstring (LabelNumber n) = Lstring $ show n
labelIdToLstring (LabelQuoteNumber n) = Lstring $ show n

data BlockLabel = ExplicitBlockLabel LabelId 
                | ImplicitBlockLabel (String, Int, Int)
                deriving (Eq, Ord, Show)
                         
data PercentLabel = PercentLabel LabelId deriving (Eq, Ord, Show)
data TargetLabel = TargetLabel PercentLabel deriving (Eq,Ord,Show)

data IbinaryOperator = Add | Sub | Mul | Udiv 
                     | Sdiv | Urem | Srem 
                     | Shl | Lshr | Ashr | And | Or | Xor
                     deriving (Eq,Ord,Show)

data FbinaryOperator = Fadd | Fsub | Fmul | Fdiv | Frem
                     deriving (Eq,Ord,Show)


data TrapFlag = Nuw | Nsw | Exact deriving (Eq,Ord,Show)

-- | Binary Operations <http://llvm.org/releases/3.0/docs/LangRef.html#binaryops>
data IbinExpr v = IbinExpr IbinaryOperator [TrapFlag] Type v v deriving (Eq,Ord,Show)
data FbinExpr v = FbinExpr FbinaryOperator FastMathFlags Type v v deriving (Eq,Ord,Show)
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
data ComplexConstant = Cstruct Bool [TypedConst] 
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
           -- | Ca (BinaryOperation Const)
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
               | MdString QuoteStr
               | McMn MdNode
               | McMv MdVar
               | MdRef LocalId
               deriving (Eq,Ord,Show)
                        
data GetResult = GetResult TypedValue String deriving (Eq, Ord, Show)


data Expr = EgEp (GetElemPtr TypedValue)
          -- | Ea (BinaryOperation Value)
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
           | Fence (IsOrIsNot SingleThread) FenceOrder
           | CmpXchg (IsOrIsNot Weak) (IsOrIsNot Volatile) TypedPointer TypedValue TypedValue (IsOrIsNot SingleThread) FenceOrder FenceOrder
           | AtomicRmw (IsOrIsNot Volatile) AtomicOp TypedPointer TypedValue (IsOrIsNot SingleThread) FenceOrder deriving (Eq,Ord,Show)


data Pointer = Pointer Value deriving (Eq, Ord, Show)
data TypedPointer = TypedPointer Type Pointer deriving (Eq, Ord, Show)
                         
data FunName = FunNameGlobal GlobalOrLocalId
             | FunNameString String
               deriving (Eq,Ord,Show)
             
data CallSite = CsFun (Maybe CallConv) [ParamAttr] Type FunName [ActualParam] [FunAttr]
              | CsAsm Type (Maybe SideEffect) (Maybe AlignStack) AsmDialect QuoteStr QuoteStr [ActualParam] [FunAttr] 
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
         | LandingPad Type Type PersFn Bool [Clause] 
         deriving (Eq,Ord,Show)
              
data Dbg = Dbg MdVar MetaConst deriving (Eq,Show)

data PhiInst = PhiInst (Maybe GlobalOrLocalId) Type 
               [(Value, PercentLabel)] deriving (Eq,Show)
    
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
                                      
data ActualParam = ActualParam { actualParamType :: Type
                               , actualParamPreAttrs :: [ParamAttr]
                               , actualParamAlign :: Maybe Alignment
                               , actualParamValue :: Value
                               , actualParamPostAttrs :: [ParamAttr]
                               } deriving (Eq,Ord,Show)
                          
                                            
data Value = VgOl GlobalOrLocalId
           | Ve Expr
           | Vc Const
           -- | Inline Assembler Expressions <http://llvm.org/releases/3.0/docs/LangRef.html#inlineasm>
--           | InlineAsm (Maybe SideEffect) (Maybe AlignStack) String String
           deriving (Eq,Ord,Show)

data TypedValue = TypedValue Type Value deriving (Eq,Ord,Show)
data TypedConst = TypedConst Type Const | TypedConstNull deriving (Eq,Ord,Show)
        
                                                
data Aliasee = AtV TypedValue
             | Ac (Conversion TypedConst)
             | AgEp (GetElemPtr TypedConst)
             deriving (Eq,Show)
                        
data FunctionPrototype = FunctionPrototype
                         { fhLinkage :: Maybe Linkage
                         , fhVisibility :: Maybe Visibility
                         , fhCCoonc :: Maybe CallConv
                         , fhAttr   :: [ParamAttr]
                         , fhRetType :: Type
                         , fhName    :: GlobalId
                         , fhParams  :: FormalParamList
                         , fhUnamed  :: Maybe AddrNaming
                         , fhAttr1   :: [FunAttr] -- Collection
                         , fhSection :: Maybe Section
                         , fhComdat :: Maybe Comdat
                         , fhAlign :: Maybe Alignment
                         , fhGc :: Maybe Gc
                         , fhPrefix :: Maybe Prefix
                         , fhPrologue :: Maybe Prologue
                         } 
                       deriving (Eq,Ord,Show)


data Toplevel = ToplevelTriple QuoteStr
              | ToplevelDataLayout DataLayout
              | ToplevelAlias GlobalId (Maybe Visibility) (Maybe DllStorage) (Maybe ThreadLocalStorage) AddrNaming (Maybe Linkage) Aliasee
              | ToplevelDbgInit String Integer
              | ToplevelStandaloneMd String TypedValue
              | ToplevelNamedMd MdVar [MdNode]
              | ToplevelDeclare FunctionPrototype
              | ToplevelDefine FunctionPrototype [Block]
              | ToplevelGlobal { toplevelGlobalLhs :: Maybe GlobalId
                               , toplevelGlobalLinkage :: Maybe Linkage
                               , toplevelGlobalVisibility :: Maybe Visibility
                               , toplevelGlobalDllStorage :: Maybe DllStorage
                               , toplevelGlobalThreadLocation :: Maybe ThreadLocalStorage
                               , toplevelGlobalUnamedAddr :: AddrNaming
                               , toplevelGlobalAddrSpace :: Maybe AddrSpace
                               , toplevelGlobalExternallyInitialized :: IsOrIsNot ExternallyInitialized
                               , toplevelGlobalGlobalType :: GlobalType
                               , toplevelGlobalType :: Type
                               , toplevelGlobalConst :: Maybe Const
                               , toplevelGlobalSection :: Maybe Section
                               , toplevelGlobalComdat :: Maybe Comdat
                               , toplevelGlobalAlign :: Maybe Alignment
                               }
              | ToplevelTypeDef LocalId Type
              | ToplevelDepLibs [QuoteStr]
              | ToplevelUnamedType Integer Type
              | ToplevelModuleAsm QuoteStr
              | ToplevelAttribute Integer [FunAttr]
              | ToplevelComdat DollarId SelectionKind
              deriving (Eq,Show)
                       

data Block = Block 
             { lbl:: BlockLabel, 
               phi:: [PhiInst], 
               comp:: [ComputingInstWithDbg], 
               term:: TerminatorInstWithDbg 
             } deriving (Eq,Show)

data Module = Module [Toplevel] deriving (Eq,Show)


