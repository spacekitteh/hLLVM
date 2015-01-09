{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-
  This module compute the use, def, and Addr of CoreIr
-}
module Llvm.Pass.Uda where
import Llvm.VmCore.CoreIr
import qualified Data.Set as Ds
import Data.Monoid
import Prelude (Show, Eq, Ord, fst, (.), ($), map, maybe, Maybe, (++), show, Bool(False), undefined)
#ifdef DEBUG
import Debug.Trace
#endif

data UDA = UDA { u1 :: Ds.Set GlobalOrLocalId
               , d1 :: Ds.Set GlobalOrLocalId
               , addr :: Ds.Set GlobalOrLocalId
               } deriving (Show, Eq, Ord)
        
instance Monoid UDA where
    mempty = UDA { u1 = Ds.empty
                 , d1 = Ds.empty
                 , addr = Ds.empty
                 }
    mappend x y = 
        UDA {  u1 = (u1 x) `Ds.union` (u1 y)
            ,  d1 = (d1 x) `Ds.union` (d1 y)
            ,  addr = (addr x) `Ds.union` (addr y)
            }
          

uDofRhs :: Rhs -> UDA
#ifdef DEBUG
uDofRhs rhs | trace ("uDofRhs " ++ (show rhs)) False = undefined
#endif
uDofRhs (RmO e) = uDofMemOp e
uDofRhs (Re e) = uDofExpr e
uDofRhs (Call _ cs) = uDofCallSite cs
uDofRhs (ReE e) = uDofExtractElem uDofTypedValue e
uDofRhs (RiE e) = uDofInsertElem uDofTypedValue e
uDofRhs (RsV e) = uDofShuffleVector uDofTypedValue e
uDofRhs (ReV e) = uDofExtractValue uDofTypedValue e
uDofRhs (RiV e) = uDofInsertValue uDofTypedValue e
uDofRhs (VaArg e _) = uDofTypedValue e
uDofRhs (LandingPad _ _ f _ cl) = (uDofPersFn f) `mappend` (mconcat $ map uDofClause cl)

uDofMemOp :: MemOp -> UDA
uDofMemOp (Allocate _ _ s _) = maybe mempty uDofTypedValue s
-- uDofMemOp (Free v) = uDofTypedValue v
uDofMemOp (Load _ ptr _ _ _ _) = uDofTypedPointer ptr
uDofMemOp (LoadAtomic _ _ ptr _) = uDofTypedPointer ptr
uDofMemOp (Store _ v a _ _) = (uDofTypedValue v) `mappend` (uDofTypedPointer a)
uDofMemOp (StoreAtomic _ _ v a _) = (uDofTypedValue v) `mappend` (uDofTypedPointer a)
uDofMemOp (Fence _ _) = mempty
uDofMemOp (CmpXchg _ _ ptr v1 v2 _ _ _) = (uDofTypedPointer ptr) 
                                          `mappend` (mconcat $ map uDofTypedValue [v1, v2])
uDofMemOp (AtomicRmw _ _ ptr v1 _ _) = (uDofTypedPointer ptr) `mappend` (uDofTypedValue v1)


uDofTypedValue :: TypedValue -> UDA
uDofTypedValue (TypedValue _ v) = uDofValue v

uDofTypedPointer :: TypedPointer -> UDA
uDofTypedPointer (TypedPointer _ ptr) = uDofPointer ptr

uDofPointer :: Pointer -> UDA
uDofPointer (Pointer g) = uDofValue g
                          
                 
uDofExtractElem :: (a -> UDA) -> ExtractElem a -> UDA
uDofExtractElem f (ExtractElem v1 v2) = mconcat $ map f [v1,v2] 

uDofInsertElem :: (a -> UDA) -> InsertElem a -> UDA
uDofInsertElem f (InsertElem v1 v2 v3) = mconcat $ map f [v1,v2,v3]

uDofShuffleVector :: (a -> UDA) -> ShuffleVector a -> UDA
uDofShuffleVector f (ShuffleVector v1 v2 v3) = mconcat $ map f [v1,v2,v3]

uDofExtractValue :: (a -> UDA) -> ExtractValue a -> UDA
uDofExtractValue f (ExtractValue v _) = f v


uDofInsertValue :: (a -> UDA) -> InsertValue a -> UDA
uDofInsertValue f (InsertValue v1 v2 _) = mconcat $ map f [v1,v2]

uDofClause :: Clause -> UDA
uDofClause (Catch v) = uDofTypedValue v
uDofClause (Filter c) = uDofTypedConst c
uDofClause (Cco c) = uDofConversion uDofTypedValue c
                 

uDofExpr :: Expr -> UDA
uDofExpr (EgEp e) = uDofGetElemPtr uDofTypedValue e
uDofExpr (EiC e) = uDofIcmp uDofValue e
uDofExpr (EfC e) = uDofFcmp uDofValue e
uDofExpr (Eb e) = uDofBinExpr uDofValue e
uDofExpr (Ec e) = uDofConversion uDofTypedValue e
uDofExpr (Es e) = uDofSelect uDofTypedValue e
uDofExpr (Ev e) = uDofTypedValue e


uDofGetElemPtr :: (a -> UDA) -> GetElemPtr a -> UDA
uDofGetElemPtr f (GetElemPtr _ ptr indices) =  (f ptr) `mappend` (mconcat $ map f indices)

uDofIcmp :: (a -> UDA) -> Icmp a -> UDA
uDofIcmp f (Icmp _ _ v1 v2) = mconcat $ map f [v1,v2]

uDofFcmp :: (a -> UDA) -> Fcmp a -> UDA
uDofFcmp f (Fcmp _ _ v1 v2) = mconcat $ map f [v1,v2]


uDofBinExpr :: (a -> UDA) -> BinExpr a -> UDA
uDofBinExpr f e = let (v1, v2) = operandOfBinExpr e
                  in mconcat $ map f [v1, v2]
                   

uDofConversion :: (a -> UDA) -> Conversion a -> UDA
uDofConversion f (Conversion _ v _) = f v

uDofSelect :: (a -> UDA) -> Select a -> UDA
uDofSelect f (Select v1 v2 v3) = mconcat $ map f [v1,v2,v3]


uDofFunName :: FunName -> UDA
uDofFunName (FunNameGlobal g) = mempty { u1 = Ds.singleton g }
uDofFunName (FunNameString _) = mempty 


uDofCallSite :: CallSite -> UDA
uDofCallSite (CallFun _ _ _ fn params _) = (uDofFunName fn) `mappend` (mconcat $ map uDofActualParam params)
                                           
uDofCallSite (CallAsm _ _ _ _ _ _ params _) = mconcat $ map uDofActualParam params
uDofCallSite (CallConversion _ _ c params _) = let s = uDofConversion uDofTypedConst c
                                               in s `mappend` (mconcat $ map uDofActualParam params)

uDofActualParam :: ActualParam -> UDA
uDofActualParam (ActualParam _ _ _ v _) = uDofValue v


uDofTypedConst :: TypedConst -> UDA
uDofTypedConst (TypedConst _ v) = uDofConst v
uDofTypedConst TypedConstNull = mempty
  
uDofConst :: Const -> UDA
uDofConst (Ccp s) = uDofSimpleConstant s
uDofConst (Cca c) = uDofComplexConstant c
uDofConst (CmL _) = mempty
uDofConst (Cl _) = mempty
uDofConst (CblockAddress _ _) = mempty
uDofConst (Cb _) = mempty
uDofConst (Cconv c) = uDofConversion uDofTypedConst c
uDofConst (CgEp g) = uDofGetElemPtr uDofTypedConst g
uDofConst (Cs s) = uDofSelect uDofTypedConst s
uDofConst (CiC x) = uDofIcmp uDofConst x
uDofConst (CfC x) = uDofFcmp uDofConst x
uDofConst (CsV x) = uDofShuffleVector uDofTypedConst x
uDofConst (CeV x) = uDofExtractValue uDofTypedConst x
uDofConst (CiV x) = uDofInsertValue uDofTypedConst x
uDofConst (CeE x) = uDofExtractElem uDofTypedConst x
uDofConst (CiE x) = uDofInsertElem uDofTypedConst x
uDofConst (CmC x) = uDofMetaConst x


uDofMetaConst :: MetaConst -> UDA
uDofMetaConst (MdConst c) = uDofConst c
uDofMetaConst (MdRef _) = mempty 
uDofMetaConst _ = mempty

uDofPersFn :: PersFn -> UDA
uDofPersFn (PersFnId g) = mempty { u1 = Ds.singleton g }
uDofPersFn (PersFnCast c) = uDofConversion (\(_,x) -> mempty {u1 = Ds.singleton x}) c
uDofPersFn PersFnUndef = mempty


uDofSimpleConstant :: SimpleConstant -> UDA
uDofSimpleConstant (CpGlobalAddr g) = mempty { addr = Ds.singleton $ GolG g }
uDofSimpleConstant _ = mempty


uDofComplexConstant :: ComplexConstant -> UDA
uDofComplexConstant (Cstruct _ l) = mconcat $ map uDofTypedConst l
uDofComplexConstant (Cvector l) = mconcat $ map uDofTypedConst l
uDofComplexConstant (Carray l) = mconcat $ map uDofTypedConst l


uDofValue :: Value -> UDA
uDofValue (VgOl g) = mempty { u1 = Ds.singleton g }
uDofValue (Ve e) = uDofExpr e
uDofValue (Vc c) = uDofConst c
uDofValue (InlineAsm _ _ _ _) = mempty
uDofValue (Deref x) = uDofPointer x



d1ofMaybeGlobalOrLocalId :: Maybe GlobalOrLocalId -> UDA
d1ofMaybeGlobalOrLocalId lhsOpt = mempty { d1 = maybe Ds.empty Ds.singleton lhsOpt }

uDofPhiInst :: PhiInst -> UDA
uDofPhiInst (PhiInst lhs _ pairs) = 
  (mempty { d1 = maybe Ds.empty Ds.singleton lhs}) `mappend` (mconcat $ map (uDofValue . fst) pairs)



uDofComputingInst :: ComputingInst -> UDA
uDofComputingInst (ComputingInst lhsOpt rhs) = 
    (d1ofMaybeGlobalOrLocalId lhsOpt) `mappend` (uDofRhs rhs)

uDofComputingInstWithDbg :: ComputingInstWithDbg -> UDA
uDofComputingInstWithDbg (ComputingInstWithDbg i _) = uDofComputingInst i


uDofTerminatorInst :: TerminatorInst -> UDA
uDofTerminatorInst (Return l) = mconcat $ map uDofTypedValue l
uDofTerminatorInst (Br _) = mempty
uDofTerminatorInst (Cbr c _ _) = uDofValue c
uDofTerminatorInst (IndirectBr c _) = uDofTypedValue c
uDofTerminatorInst (Switch c _ cases) = (uDofTypedValue c) `mappend` 
                                        (mconcat $ map (uDofTypedValue . fst) cases)
uDofTerminatorInst (Invoke lhsOpt cs _ _) = (d1ofMaybeGlobalOrLocalId lhsOpt) `mappend` (uDofCallSite cs)
uDofTerminatorInst (Resume v) = uDofTypedValue v
uDofTerminatorInst Unreachable = mempty
uDofTerminatorInst Unwind = mempty




uDofTerminatorInstWithDbg :: TerminatorInstWithDbg -> UDA
uDofTerminatorInstWithDbg (TerminatorInstWithDbg i _) = uDofTerminatorInst i


u1ofPinst :: PhiInst -> Ds.Set GlobalOrLocalId
u1ofPinst = u1 . uDofPhiInst

d1ofPinst :: PhiInst -> Ds.Set GlobalOrLocalId
d1ofPinst = d1 . uDofPhiInst


u1ofComputingInstWithDbg :: ComputingInstWithDbg -> Ds.Set GlobalOrLocalId
u1ofComputingInstWithDbg = u1 . uDofComputingInstWithDbg

d1ofComputingInstWithDbg :: ComputingInstWithDbg -> Ds.Set GlobalOrLocalId
d1ofComputingInstWithDbg = d1 . uDofComputingInstWithDbg

u1ofTerminatorInstWithDbg :: TerminatorInstWithDbg -> Ds.Set GlobalOrLocalId
u1ofTerminatorInstWithDbg = u1 . uDofTerminatorInstWithDbg

d1ofTerminatorInstWithDbg :: TerminatorInstWithDbg -> Ds.Set GlobalOrLocalId
d1ofTerminatorInstWithDbg = d1 . uDofTerminatorInstWithDbg



localIdSetOf :: Ds.Set GlobalOrLocalId -> Ds.Set LocalId
localIdSetOf s = Ds.fold (\e p -> case e of 
                                   GolG _ -> p
                                   GolL x -> Ds.insert x p
                         ) Ds.empty s
