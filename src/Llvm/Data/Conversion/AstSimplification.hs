{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Llvm.Data.Conversion.AstSimplification (simplify) where

import Llvm.Data.Ast
import qualified Data.Map as M
import qualified Data.Set as St
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Data.Maybe
import Llvm.Util.Monadic (maybeM)
import Debug.Trace
import Data.Word (Word32)

{-
  Assign explicit names to all implicit variables  
  Assign explicit labels to all implicit labels
-}
data MyState = MyState { _cnt :: Word32 -- Integer
                       , _renaming :: String
                       , _implicitLabelNums :: St.Set Word32 -- Integer
                       , _explicitLabelNums :: St.Set Word32 -- Integer
                       , _explicitLabelDqNums :: St.Set Word32 -- Integer                         
                       } 
               
type MS = S.State MyState

data LabelNumbers = LabelNumbers { implicitNumbers :: St.Set Word32 -- Integer
                                 , explicitNumbers :: St.Set Word32 -- Integer
                                 , explicitDqNumbers :: St.Set Word32 -- Integer
                                 } deriving (Eq, Ord, Show)
                                            
idPrefix :: String
idPrefix = "_hsllvm_implicit_ID"

implicitLbPrefix :: String
implicitLbPrefix = "_hsllvm_implicit_L"

explicitLbPrefix :: String
explicitLbPrefix = "_hsllvm_explicit_L"

------------------------------------------------------------------------
-- iteration 1
------------------------------------------------------------------------
iter1 :: [Toplevel] -> ([Toplevel], M.Map GlobalId LabelNumbers)
iter1 l = let (l1, m1) = unzip $ fmap rnToplevel1 l
          in (l1, foldl M.union M.empty m1)

rnToplevel1 :: Toplevel -> (Toplevel, M.Map GlobalId LabelNumbers)
rnToplevel1 (ToplevelDefine (TlDefine fp bs)) = 
  let ((fp2, bs2), mys) = S.runState (do { fpa <- rnDefFunctionPrototype fp
                                         ; bsa <- S.mapM rnBlock bs
                                         ; return (fpa, bsa)
                                         }
                                     ) (MyState 0 (show fp) St.empty St.empty St.empty)
      labelNumbers = LabelNumbers (_implicitLabelNums mys) (_explicitLabelNums mys) (_explicitLabelDqNums mys)
  in ((ToplevelDefine (TlDefine fp2 bs2)), M.insert (getGlobalId fp) labelNumbers M.empty)
rnToplevel1 x = (x, M.empty)


getGlobalId :: FunctionPrototype -> GlobalId
getGlobalId (FunctionPrototype _ _ _ _ _ _ fhName _ _ _ _ _ _ _ _ _) = fhName 

rnDefFunctionPrototype :: FunctionPrototype -> MS FunctionPrototype
rnDefFunctionPrototype fpt@(FunctionPrototype v0 v1 v2 v3 v4 v5 v6 fhParams v8 v9 v10 v11 v12 v13 fhPrefix fhPrologue) = 
  do { fp' <- rnDefFormalParamList fhParams
     ; prefix' <- rnPrefix fhPrefix
     ; prologue' <- rnPrologue fhPrologue 
     ; return $ FunctionPrototype v0 v1 v2 v3 v4 v5 v6 fp' v8 v9 v10 v11 v12 v13  prefix' prologue'
     }
  
  
  
rnDefFormalParamList :: FormalParamList -> MS FormalParamList  
rnDefFormalParamList (FormalParamList fps b fa) = do { fps' <- mapM rnDefFormalParam fps
                                                     ; return $ FormalParamList fps' b fa
                                                     }
                                               
rnDefFormalParam :: FormalParam -> MS FormalParam
rnDefFormalParam x = case x of
  (FormalParamData t pas1 align pid pas2) -> 
    do { pid' <- rnDefFparam pid
       ; return $ FormalParamData (normalType t) pas1 align pid' pas2
       }
  (FormalParamMeta e pid) -> 
    do { pid1 <- rnDefFparam pid
       ; e1 <- rnMetaKind e
       ; return $ FormalParamMeta e1 pid1
       }

rnDefFparam :: Fparam -> MS Fparam 
rnDefFparam (FimplicitParam) =
  do { x <- getNext
     ; S.liftM FexplicitParam (rnDefLocalId (LocalIdNum x))
     }
rnDefFparam (FexplicitParam x) = S.liftM FexplicitParam (rnDefLocalId x)
  
rnLabelId :: LabelId -> MS LabelId
rnLabelId x = return x



getNext :: MS Word32 -- Integer
getNext = do { MyState{..} <- S.get
             ; S.modify (\s@MyState {..} -> s { _cnt = _cnt + 1})
             ; return _cnt
             }

getFp :: MS String
getFp = do { MyState{..} <- S.get
           ; return _renaming
           }

rnDefLabel :: BlockLabel -> MS BlockLabel
rnDefLabel bl = case bl of
  ImplicitBlockLabel _ -> do { i <- getNext
                             ; S.modify (\s@MyState{..} ->  s { _implicitLabelNums  = St.insert i _implicitLabelNums })
                             ; ia <- return $ LabelString (implicitLbPrefix ++ show i)
                             ; return $ ExplicitBlockLabel ia
                             }
  ExplicitBlockLabel i -> S.liftM ExplicitBlockLabel (recordLabelNumber i)
  where 
    recordLabelNumber :: LabelId -> MS LabelId
    recordLabelNumber (LabelNumber i) = S.modify (\s@MyState {..} -> s { _explicitLabelNums = St.insert i _explicitLabelNums }) >> return (LabelString (explicitLbPrefix ++ show i))
    recordLabelNumber x@(LabelDqNumber i) = S.modify (\s@MyState {..} -> s { _explicitLabelDqNums = St.insert i _explicitLabelDqNums }) >> return x
    recordLabelNumber x = return x

rnBlock :: Block -> MS Block
rnBlock (Block lbl phi comp term) = do { lbl' <- rnDefLabel lbl
                                       ; phi' <- mapM rnPhiInstWithDbg phi
                                       ; comp' <- mapM rnComputingInstWithDbg comp
                                       ; term' <- rnTerminatorInstWithDbg term
                                       ; return (Block lbl' phi' comp' term')
                                       }


rnDefLocalId :: LocalId -> MS LocalId
#ifdef DEBUG
rnDefLocalId l | trace ("rnDefLocalId " ++ (show l)) False = undefined
#endif
rnDefLocalId (LocalIdNum i) = S.liftM (LocalIdAlphaNum) (return (idPrefix ++ show i))
rnDefLocalId x = return x


rnLocalId :: LocalId -> MS LocalId
rnLocalId (LocalIdNum i) = S.liftM (LocalIdAlphaNum) (return (idPrefix ++ show i))
rnLocalId x = return x  


rnLhs :: LocalId -> MS LocalId
rnLhs localId = checkImpCount localId >> rnDefLocalId localId

rnPhiInstWithDbg :: PhiInstWithDbg -> MS PhiInstWithDbg
rnPhiInstWithDbg (PhiInstWithDbg ins dbgs) = S.liftM (\x -> PhiInstWithDbg x dbgs) (rnPhi ins)

rnPhi :: PhiInst -> MS PhiInst
rnPhi (PhiInst lhs t ins) = do { lhs' <- maybeM rnLhs lhs
                               ; lhs'' <- getLhs True lhs'
                               ; ins' <- S.mapM g ins
                               ; return $ PhiInst lhs'' t ins'
                               }
  where g (v, p) = do { v' <- rnValue v
                      ; p' <- rnPercentLabel p
                      ; return (v', p')
                      }


rnComputingInstWithDbg :: ComputingInstWithDbg -> MS ComputingInstWithDbg
rnComputingInstWithDbg (ComputingInstWithDbg c l) = S.liftM (\x -> ComputingInstWithDbg x l) 
                                                    (rnComputingInst c)

rnComputingInst :: ComputingInst -> MS ComputingInst
#ifdef DEBUG
rnComputingInst x | trace ("rnComputingInst " ++ show (x)) False = undefined
#endif
rnComputingInst (ComputingInst lhs rhs) = do { lhs' <- maybeM rnLhs lhs
                                             ; (b, rhs') <- rnRhs rhs
                                             ; lhs'' <- getLhs b lhs'
                                             ; return (ComputingInst lhs'' rhs')
                                             }

getLhs :: Bool -> Maybe LocalId -> MS (Maybe LocalId)
getLhs False x = return x
getLhs True (Just x) = return $ Just x
getLhs True Nothing = do { m <- getNext
                         ; m' <- rnLocalId (LocalIdNum m)
                         ; return $ Just m'
                         }

rnGlobalOrLocalId :: GlobalOrLocalId -> MS GlobalOrLocalId
rnGlobalOrLocalId (GolL l) = S.liftM GolL (rnLocalId l)
rnGlobalOrLocalId x = return x

rnValue :: Value -> MS Value
rnValue (Val_local x) = S.liftM Val_local (rnLocalId x)
rnValue (Val_const x) = S.liftM Val_const (rnConst x)

rnTypedValue :: Typed Value -> MS (Typed Value)
rnTypedValue (Typed t v) = S.liftM (Typed $ normalType t) (rnValue v)

normalType :: Type -> Type
normalType (Tpointer et as) = Tpointer (normalType et) (normAddrSpace as)  
normalType (Tstruct pk l) = Tstruct pk (fmap normalType l)
normalType (Tarray n e) = Tarray n (normalType e)
normalType x = x
  
normAddrSpace :: AddrSpace -> AddrSpace  
normAddrSpace (AddrSpace 0) = AddrSpaceUnspecified
normAddrSpace x = x

checkImpCount :: LocalId -> MS ()
checkImpCount x@(LocalIdNum i) = 
  do { m <- getNext
     ; if (i == m) then return ()
       else do { y <- getFp
               ; error ("AstSimplify:expect " ++ (show m) ++ " and but found " ++ (show x) ++ " in " ++ y)
               }
     }
checkImpCount _ = return ()                
                                         
rnRhs :: Rhs -> MS (Bool, Rhs)
rnRhs lhs = case lhs of
              RmO x -> S.liftM (\(b,x) -> (b, RmO x)) (rnMemOp x)
              Re x -> S.liftM (\x -> (True, Re x)) (rnExpr x)
              Call b cs -> S.liftM (\(bb, x) -> (bb, Call b x)) (rnCallSite cs)
              {-
              (Asm tc t dia b1 b2 qs1 qs2 aps fas) -> 
                do { apsa <- mapM rnActualParam aps
                   ; let (rt, _) = splitCallReturnType t
                   ; return (not (rt == Tvoid), Asm tc t dia b1 b2 qs1 qs2 apsa fas)
                   }-}
              ReE e -> S.liftM (\x -> (True, ReE x)) (rnExtractElem rnTypedValue e)
              RiE e -> S.liftM (\x -> (True, RiE x)) (rnInsertElem rnTypedValue e)
              RsV e -> S.liftM (\x -> (True, RsV x)) (rnShuffleVector rnTypedValue e)
              ReV e -> S.liftM (\x -> (True, ReV x)) (rnExtractValue rnTypedValue e)
              RiV e -> S.liftM (\x -> (True, RiV x)) (rnInsertValue rnTypedValue e)
              RvA (VaArg tv t) -> S.liftM (\x -> (True, RvA $ VaArg x t)) (rnTypedValue tv)
              RlP (LandingPad rt ft ix cl c) -> return (True, lhs)
              
rnMemOp :: MemOp -> MS (Bool, MemOp)
rnMemOp (Alloca m t tv a) = do { tv' <- maybeM rnTypedValue tv
                               ; return (True, Alloca m t tv' a)
                               }
rnMemOp (Load a tp ma nt inv nl) = S.liftM (\x -> (True, Load a x ma nt inv nl)) (rnPointer rnTypedValue tp)
rnMemOp (LoadAtomic at a tp ma) = S.liftM (\x -> (True, LoadAtomic at a x ma)) (rnPointer rnTypedValue tp)
rnMemOp (Store a tv tp ma nt) = S.liftM2 (\x y -> (False, Store a x y ma nt))
                                (rnTypedValue tv) (rnPointer rnTypedValue tp)
rnMemOp (StoreAtomic at a tv tp ma) = S.liftM2 (\x y -> (False, StoreAtomic at a x y ma))
                                      (rnTypedValue tv) (rnPointer rnTypedValue tp)
rnMemOp (Fence b f) = return (False, Fence b f)
rnMemOp (CmpXchg wk b1 tp tv1 tv2 b2 mf ff) = 
  S.liftM3 (\x y z -> (True, CmpXchg wk b1 x y z b2 mf ff))
  (rnPointer rnTypedValue tp)
  (rnTypedValue tv1)
  (rnTypedValue tv2)
rnMemOp (AtomicRmw b1 ao tp tv b2 mf) = 
  S.liftM2 (\x y -> (True, AtomicRmw b1 ao x y b2 mf))
  (rnPointer rnTypedValue tp)
  (rnTypedValue tv)

rnPointer :: (a -> MS a) -> Pointer a -> MS (Pointer a)
rnPointer f (Pointer v) = S.liftM Pointer (f v) 

rnExpr :: Expr -> MS Expr
rnExpr (EgEp e) = S.liftM EgEp (rnGetElemPtr rnTypedValue e)
rnExpr (EiC e) = S.liftM EiC (rnIcmp rnValue e)
rnExpr (EfC e) = S.liftM EfC (rnFcmp rnValue e)
rnExpr (Eb e) = S.liftM Eb (rnBinExpr rnValue e)
rnExpr (Ec e) = S.liftM Ec (rnConversion rnTypedValue e)
rnExpr (Es e) = S.liftM Es (rnSelect rnTypedValue e)


rnGetElemPtr :: (Typed v -> MS (Typed v)) -> GetElementPtr v -> MS (GetElementPtr v)
rnGetElemPtr fv (GetElementPtr b v vs) = do { v' <- rnPointer fv v
                                            ; vs' <- mapM fv vs
                                            ; return $ GetElementPtr b v' vs'
                                            }


rnIcmp :: (a -> MS a) -> Icmp a -> MS (Icmp a)
rnIcmp f (Icmp o t v1 v2) = S.liftM2 (Icmp o t) (f v1) (f v2)

rnFcmp :: (a -> MS a) -> Fcmp a -> MS (Fcmp a)
rnFcmp f (Fcmp o t v1 v2) = S.liftM2 (Fcmp o t) (f v1) (f v2)

rnIbinExpr :: (a -> MS a) -> IbinExpr a -> MS (IbinExpr a)
rnIbinExpr f (IbinExpr o tf t v1 v2) = S.liftM2 (IbinExpr o tf t) (f v1) (f v2)

rnFbinExpr :: (a -> MS a) -> FbinExpr a -> MS (FbinExpr a)
rnFbinExpr f (FbinExpr o tf t v1 v2) = S.liftM2 (FbinExpr o tf t) (f v1) (f v2)

rnBinExpr :: (a -> MS a) -> BinExpr a -> MS (BinExpr a)
rnBinExpr f (Ie x) = S.liftM Ie (rnIbinExpr f x)
rnBinExpr f (Fe x) = S.liftM Fe (rnFbinExpr f x)

rnConversion :: (Typed a -> MS (Typed a)) -> Conversion a -> MS (Conversion a)
rnConversion f (Conversion o v t) = S.liftM (\x -> Conversion o x t) (f v)

rnSelect :: (Typed a -> MS (Typed a)) -> Select a -> MS (Select a)
rnSelect f (Select v1 v2 v3) = S.liftM3 Select (f v1) (f v2) (f v3)


rnFunName :: FunName -> MS FunName
rnFunName (FunNameGlobal g) = S.liftM FunNameGlobal (rnGlobalOrLocalId g)
rnFunName x = return x
  

rnCallSite :: CallSite -> MS (Bool, CallSite)
rnCallSite (CsFun c pa t fn aps fas) = 
  do { fna <- rnFunName fn
     ; x <- mapM rnActualParam aps
     ; let (rt, _) = splitCallReturnType t
     ; return (not (rt == Tvoid), CsFun c pa t fna x fas)
     }
rnCallSite (CsAsm t dia b1 b2 qs1 qs2 aps fas) = 
  do { apsa <- mapM rnActualParam aps
     ; let (rt, _) = splitCallReturnType t
     ; return (not (rt == Tvoid),  CsAsm t dia b1 b2 qs1 qs2 apsa fas)
     }
{-  
rnCallSite (CsConversion pas t c aps fas) = 
  do { apsa <- mapM rnActualParam aps
     ; let (rt, _) = splitCallReturnType t
     ; return (not (rt == Tvoid), CsConversion pas t c apsa fas)
     }
-}

rnActualParam :: ActualParam -> MS ActualParam
rnActualParam (ActualParamData t ps ma v pa) = S.liftM (\x -> ActualParamData t ps ma x pa) (rnValue v)
rnActualParam (ActualParamLabel t ps ma v pa) = S.liftM (\x -> ActualParamLabel t ps ma x pa) (rnPercentLabel v)
rnActualParam (ActualParamMeta mc) = S.liftM ActualParamMeta (rnMetaKindedConst mc)
                                        

rnExtractElem :: (Typed a -> MS (Typed a)) -> ExtractElement a -> MS (ExtractElement a)
rnExtractElem f (ExtractElement v1 v2) = S.liftM2 ExtractElement (f v1) (f v2)

rnInsertElem :: (Typed a -> MS (Typed a)) -> InsertElement a -> MS (InsertElement a)
rnInsertElem f (InsertElement v1 v2 v3) = S.liftM3 InsertElement (f v1) (f v2) (f v3)

rnShuffleVector :: (Typed a -> MS (Typed a)) -> ShuffleVector a -> MS (ShuffleVector a)
rnShuffleVector f (ShuffleVector v1 v2 v3) = S.liftM3 ShuffleVector (f v1) (f v2) (f v3)


rnExtractValue :: (Typed a -> MS (Typed a)) -> ExtractValue a -> MS (ExtractValue a)
rnExtractValue f (ExtractValue v s) = S.liftM (\x -> ExtractValue x s) (f v)


rnInsertValue :: (Typed a -> MS (Typed a)) -> InsertValue a -> MS (InsertValue a)
rnInsertValue f (InsertValue v1 v2 s) = S.liftM2 (\x y -> InsertValue x y s)
                                        (f v1) (f v2)
                                        
  
rnTerminatorInstWithDbg :: TerminatorInstWithDbg -> MS TerminatorInstWithDbg
rnTerminatorInstWithDbg (TerminatorInstWithDbg t ds) = S.liftM (\x -> TerminatorInstWithDbg x ds) (rnTerminatorInst t)


rnTerminatorInst :: TerminatorInst -> MS TerminatorInst
rnTerminatorInst (Return ls) = S.liftM Return (mapM rnTypedValue ls)
rnTerminatorInst (Br l) = S.liftM Br (rnTargetLabel l)
rnTerminatorInst (Cbr v tl fl) = S.liftM3 Cbr (rnValue v) (rnTargetLabel tl) (rnTargetLabel fl)
rnTerminatorInst (Switch t d cases) = S.liftM3 Switch (rnTypedValue t) (rnTargetLabel d) 
                                      (mapM (\(x,y) -> S.liftM2 (,)
                                                       (rnTypedValue x)
                                                       (rnTargetLabel y)) cases)

rnTerminatorInst (IndirectBr tv ls) = S.liftM2 IndirectBr
                                      (rnTypedValue tv) (mapM rnTargetLabel ls)
                                      
rnTerminatorInst (Invoke lhsOpt callSite tl fl) = do { lhsOpt' <- (maybeM rnLhs lhsOpt)
                                                     ; (b, cs) <- (rnCallSite callSite)
                                                     ; lhsOpt'' <- getLhs b lhsOpt'
                                                     ; tl' <- (rnTargetLabel tl)
                                                     ; fl' <- (rnTargetLabel fl)
                                                     ; return $ Invoke lhsOpt'' cs tl' fl'
                                                     }
rnTerminatorInst (Resume tv) = S.liftM Resume (rnTypedValue tv)                                                  
rnTerminatorInst x = return x


rnPercentLabel :: PercentLabel -> MS PercentLabel
rnPercentLabel (PercentLabel x) = S.liftM PercentLabel (rnLabelId x)

rnTargetLabel :: TargetLabel -> MS TargetLabel
rnTargetLabel (TargetLabel x) = S.liftM TargetLabel (rnPercentLabel x)


rnMetaConst :: MetaConst -> MS MetaConst
rnMetaConst (McRef l) = S.liftM McRef (rnLocalId l)
rnMetaConst (McStruct c) = S.liftM McStruct (S.mapM (mapMetaKindedConst rnMetaConst) c)
rnMetaConst mc@(McSimple sc) = return mc
rnMetaConst x = return x
 
rnMetaKind :: MetaKind -> MS MetaKind                
rnMetaKind mk = return mk

rnMetaKind2 :: MetaKind -> RD MetaKind
rnMetaKind2 mk = return mk

rnMetaKindedConst :: MetaKindedConst -> MS MetaKindedConst
rnMetaKindedConst x = case x of
  (MetaKindedConst mk mc) -> S.liftM (MetaKindedConst mk) (rnMetaConst mc)
  UnmetaKindedNull -> return UnmetaKindedNull

mapMetaKindedConst :: Monad m => (MetaConst -> m MetaConst) -> MetaKindedConst -> m MetaKindedConst
mapMetaKindedConst f x = case x of
  (MetaKindedConst mk mc) -> S.liftM (MetaKindedConst mk) (f mc)
  UnmetaKindedNull -> return UnmetaKindedNull

rnConst :: Const -> MS Const
rnConst (C_complex x) = S.liftM C_complex (rnComplexConstant x)
-- rnConst (C_localId x) = S.liftM C_localId (rnLocalId x)
rnConst (C_labelId l) = S.liftM C_labelId (rnLabelId l)
rnConst (C_blockAddress g l) = S.liftM (C_blockAddress g) (rnPercentLabel l)
rnConst (C_binexp bexpr) = S.liftM C_binexp (rnBinExpr rnConst bexpr)
rnConst (C_conv convert) = S.liftM C_conv (rnConversion rnTypedConst convert)
rnConst (C_gep getelm) = S.liftM C_gep (rnGetElemPtr rnTypedConst getelm)
rnConst (C_select select) = S.liftM C_select (rnSelect rnTypedConst select)
rnConst (C_icmp icmp) = S.liftM C_icmp (rnIcmp rnConst icmp)
rnConst (C_fcmp fcmp) = S.liftM C_fcmp (rnFcmp rnConst fcmp)
rnConst (C_shufflevector shuffle) = S.liftM C_shufflevector (rnShuffleVector rnTypedConst shuffle)
rnConst (C_extractvalue extract) = S.liftM C_extractvalue (rnExtractValue rnTypedConst extract)
rnConst (C_insertvalue insert) = S.liftM C_insertvalue (rnInsertValue rnTypedConst insert)
rnConst (C_extractelement extract) = S.liftM C_extractelement (rnExtractElem rnTypedConst extract)
rnConst (C_insertelement insert) = S.liftM C_insertelement (rnInsertElem rnTypedConst insert)
rnConst x = return x

rnComplexConstant :: ComplexConstant -> MS ComplexConstant
rnComplexConstant (Cstruct b l) = S.liftM (Cstruct b) (S.mapM (mapTypedConstOrNull rnTypedConst) l)
rnComplexConstant (Carray l) = S.liftM Carray (S.mapM (mapTypedConstOrNull rnTypedConst) l)
rnComplexConstant (Cvector l) = S.liftM Cvector (S.mapM (mapTypedConstOrNull rnTypedConst) l)

rnTypedConst :: Typed Const -> MS (Typed Const)                                  
rnTypedConst (Typed t c) = S.liftM (Typed t) (rnConst c)

rnPrefix :: Maybe Prefix -> MS (Maybe Prefix)
rnPrefix (Just (Prefix n)) = S.liftM (Just . Prefix) (mapTypedConstOrNull rnTypedConst n)
rnPrefix _ = return Nothing

rnPrologue :: Maybe Prologue -> MS (Maybe Prologue)
rnPrologue (Just (Prologue n)) = S.liftM (Just . Prologue) (mapTypedConstOrNull rnTypedConst n)
rnPrologue _ = return Nothing



------------------------------------------------------------------------
-- iteration 2
------------------------------------------------------------------------
type RD a = R.Reader (GlobalId, (M.Map GlobalId LabelNumbers)) a

iter2 :: M.Map GlobalId LabelNumbers -> [Toplevel] -> [Toplevel]
#ifdef DEBUG
iter2 m tl | trace ("iter2 " ++ show m) False = undefined
#endif
iter2 m tl = fmap (rnToplevel2 m) tl
  
rnToplevel2 :: M.Map GlobalId LabelNumbers -> Toplevel -> Toplevel
rnToplevel2 m (ToplevelGlobal (TlGlobal v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 const v12 v13 v14)) = 
  R.runReader (do { x <- maybe (return Nothing) (\x -> R.liftM Just (rnConst2 x)) const
                  ; return (ToplevelGlobal (TlGlobal v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 const v12 v13 v14))
                  }) (GlobalIdNum 0, m)
rnToplevel2 m (ToplevelDefine (TlDefine fp@(FunctionPrototype _ _ _ _ _ _ fhName _ _ _ _ _ _ _ _ _) bs)) = 
  let (fp2, bs2) = R.runReader (do { fp' <- rnDefFunctionPrototype2 fp
                                   ; bs' <- S.mapM rnBlock2 bs
                                   ; return (fp', bs')
                                   }) (fhName, m) -- 
  in (ToplevelDefine (TlDefine fp2 bs2))
rnToplevel2 _ x = x


rnBlock2 :: Block -> RD Block
rnBlock2 (Block lbl phi comp term) = do { phi' <- mapM rnPhiInstWithDbg2 phi
                                        ; comp' <- mapM rnComputingInstWithDbg2 comp
                                        ; term' <- rnTerminatorInstWithDbg2 term
                                        ; return (Block lbl phi' comp' term')
                                        }


rnPhiInstWithDbg2 :: PhiInstWithDbg -> RD PhiInstWithDbg
rnPhiInstWithDbg2 (PhiInstWithDbg ins dbgs) = S.liftM (\x -> PhiInstWithDbg x dbgs) (rnPhi2 ins)

rnPhi2 :: PhiInst -> RD PhiInst
rnPhi2 (PhiInst lhs t ins) = do { ins' <- S.mapM g ins
                                ; return $ PhiInst lhs t ins'
                                }
  where g (v, p) = do { v' <- rnValue2 v
                      ; p' <- rnMyPercentLabel2 p
                      ; return (v', p')
                      }

rnComputingInstWithDbg2 :: ComputingInstWithDbg -> RD ComputingInstWithDbg
rnComputingInstWithDbg2 (ComputingInstWithDbg c l) = 
  R.liftM (\x -> ComputingInstWithDbg x l) (rnComputingInst2 c)

rnComputingInst2 :: ComputingInst -> RD ComputingInst
#ifdef DEBUG
rnComputingInst2 x | trace ("rnComputingInst2 " ++ show (x)) False = undefined
#endif
rnComputingInst2 (ComputingInst lhs rhs) = do { rhsa <- rnRhs2 rhs
                                              ; return (ComputingInst lhs rhsa)
                                              }


rnRhs2 :: Rhs -> RD Rhs
rnRhs2 lhs = case lhs of
  RmO x -> S.liftM RmO (rnMemOp2 x)
  Re x -> S.liftM Re (rnExpr2 x)
  Call b cs -> S.liftM (Call b) (rnCallSite2 cs)
  {-
  (Asm tc t dia b1 b2 qs1 qs2 aps fas) -> 
    do { apsa <- mapM rnActualParam2 aps
       ; return (Asm tc t dia b1 b2 qs1 qs2 apsa fas)
       }-}
  ReE e -> S.liftM ReE (rnExtractElem2 rnTypedValue2 e)
  RiE e -> S.liftM RiE (rnInsertElem2 rnTypedValue2 e)
  RsV e -> S.liftM RsV (rnShuffleVector2 rnTypedValue2 e)
  ReV e -> S.liftM ReV (rnExtractValue2 rnTypedValue2 e)
  RiV e -> S.liftM RiV (rnInsertValue2 rnTypedValue2 e)
  RvA (VaArg tv t) -> S.liftM (\x -> RvA $ VaArg x t) (rnTypedValue2 tv)
  RlP (LandingPad rt ft ix cl c) -> return lhs


rnMemOp2 :: MemOp -> RD MemOp
rnMemOp2 (Alloca m t tv a) = do { tv' <- maybeM rnTypedValue2 tv
                                ; return (Alloca m t tv' a)
                                }
rnMemOp2 (Load a tp ma nt inv nl) = S.liftM (\x -> (Load a x ma nt inv nl)) (rnPointer2 rnTypedValue2 tp)
rnMemOp2 (LoadAtomic at a tp ma) = S.liftM (\x -> (LoadAtomic at a x ma)) (rnPointer2 rnTypedValue2 tp)
rnMemOp2 (Store a tv tp ma nt) = S.liftM2 (\x y -> (Store a x y ma nt))
                                 (rnTypedValue2 tv) (rnPointer2 rnTypedValue2 tp)
rnMemOp2 (StoreAtomic at a tv tp ma) = S.liftM2 (\x y -> (StoreAtomic at a x y ma))
                                       (rnTypedValue2 tv) (rnPointer2 rnTypedValue2 tp)
rnMemOp2 (Fence b f) = return (Fence b f)
rnMemOp2 (CmpXchg wk b1 tp tv1 tv2 b2 mf ff) = 
  S.liftM3 (\x y z -> CmpXchg wk b1 x y z b2 mf ff)
  (rnPointer2 rnTypedValue2 tp)
  (rnTypedValue2 tv1)
  (rnTypedValue2 tv2)
rnMemOp2 (AtomicRmw b1 ao tp tv b2 mf) = 
  S.liftM2 (\x y -> AtomicRmw b1 ao x y b2 mf)
  (rnPointer2 rnTypedValue2 tp)
  (rnTypedValue2 tv)

rnCallSite2 :: CallSite -> RD CallSite
rnCallSite2 (CsFun c pa t fn aps fas) = 
  do { x <- mapM rnActualParam2 aps
     ; return (CsFun c pa t fn x fas)
     }
rnCallSite2 (CsAsm t dia b1 b2 qs1 qs2 aps fas) = 
  do { apsa <- mapM rnActualParam2 aps
     ; return (CsAsm t dia b1 b2 qs1 qs2 apsa fas)
     }
{-  
rnCallSite2 (CsConversion pas t c aps fas) = 
  do { apsa <- mapM rnActualParam2 aps
     ; return (CsConversion pas t c apsa fas)
     }
-}
  
rnActualParam2 :: ActualParam -> RD ActualParam
rnActualParam2 (ActualParamData t ps ma v pa) = S.liftM (\x -> ActualParamData t ps ma x pa) (rnValue2 v)
rnActualParam2 (ActualParamLabel t ps ma v pa) = do { r <- R.ask
                                                    ; S.liftM (\x -> ActualParamLabel t ps ma x pa) (rnPercentLabel2 (fst r) v)
                                                    }
rnActualParam2 (ActualParamMeta mc) = S.liftM ActualParamMeta (rnMetaKindedConst2 mc)
                                        
  

rnTerminatorInstWithDbg2 :: TerminatorInstWithDbg -> RD TerminatorInstWithDbg
rnTerminatorInstWithDbg2 (TerminatorInstWithDbg t ds) = 
  S.liftM (\x -> TerminatorInstWithDbg x ds) (rnTerminatorInst2 t)
  
  
rnTerminatorInst2 :: TerminatorInst -> RD TerminatorInst
rnTerminatorInst2 x = case x of
  Return ls -> S.liftM Return (mapM rnTypedValue2 ls)
  Br l -> S.liftM Br (rnTargetLabel2 l)
  Cbr v tl fl -> S.liftM3 Cbr (rnValue2 v) (rnTargetLabel2 tl) (rnTargetLabel2 fl)
  Switch t d cases -> S.liftM3 Switch (rnTypedValue2 t) (rnTargetLabel2 d) 
                      (mapM (\(x,y) -> S.liftM2 (,)
                                       (rnTypedValue2 x)
                                       (rnTargetLabel2 y)) cases)
  IndirectBr tv ls -> S.liftM2 IndirectBr
                      (rnTypedValue2 tv) (mapM rnTargetLabel2 ls)
  Invoke lhsOpt callSite tl fl -> do { cs <- (rnCallSite2 callSite)
                                     ; tla <- (rnTargetLabel2 tl)
                                     ; fla <- (rnTargetLabel2 fl)
                                     ; return $ Invoke lhsOpt cs tla fla
                                     }
  Resume tv -> S.liftM Resume (rnTypedValue2 tv)
  _ -> return x  

rnTargetLabel2 :: TargetLabel -> RD TargetLabel
rnTargetLabel2 (TargetLabel x) = S.liftM TargetLabel (rnMyPercentLabel2 x)

rnDefFunctionPrototype2 :: FunctionPrototype -> RD FunctionPrototype
rnDefFunctionPrototype2 fpt = return fpt


rnConst2 :: Const -> RD Const
rnConst2 (C_complex x) = S.liftM C_complex (rnComplexConstant2 x)
-- rnConst2 (C_localId x) = S.liftM C_localId (rnLocalId2 x)
rnConst2 (C_labelId l) = do { rd <- R.ask
                            ; S.liftM C_labelId (rnLabelId2 (fst rd) l)
                            }
rnConst2 (C_blockAddress g l) = S.liftM (C_blockAddress g) (rnPercentLabel2 g l)
rnConst2 (C_binexp bexpr) = S.liftM C_binexp (rnBinExpr2 rnConst2 bexpr)
rnConst2 (C_conv convert) = S.liftM C_conv (rnConversion2 rnTypedConst2 convert)
rnConst2 (C_gep getelm) = S.liftM C_gep (rnGetElemPtr2 rnTypedConst2 getelm)
rnConst2 (C_select select) = S.liftM C_select (rnSelect2 rnTypedConst2 select)
rnConst2 (C_icmp icmp) = S.liftM C_icmp (rnIcmp2 rnConst2 icmp)
rnConst2 (C_fcmp fcmp) = S.liftM C_fcmp (rnFcmp2 rnConst2 fcmp)
rnConst2 (C_shufflevector shuffle) = S.liftM C_shufflevector (rnShuffleVector2 rnTypedConst2 shuffle)
rnConst2 (C_extractvalue extract) = S.liftM C_extractvalue (rnExtractValue2 rnTypedConst2 extract)
rnConst2 (C_insertvalue insert) = S.liftM C_insertvalue (rnInsertValue2 rnTypedConst2 insert)
rnConst2 (C_extractelement extract) = S.liftM C_extractelement (rnExtractElem2 rnTypedConst2 extract)
rnConst2 (C_insertelement insert) = S.liftM C_insertelement (rnInsertElem2 rnTypedConst2 insert)
rnConst2 x = return x



rnComplexConstant2 :: ComplexConstant -> RD ComplexConstant
rnComplexConstant2 (Cstruct b l) = S.liftM (Cstruct b) (S.mapM (mapTypedConstOrNull rnTypedConst2) l)
rnComplexConstant2 (Carray l) = S.liftM Carray (S.mapM (mapTypedConstOrNull rnTypedConst2) l)
rnComplexConstant2 (Cvector l) = S.liftM Cvector (S.mapM (mapTypedConstOrNull rnTypedConst2) l)
                                  
rnTypedConst2 :: Typed Const -> RD (Typed Const)                                  
rnTypedConst2 (Typed t c) = S.liftM (Typed t) (rnConst2 c)

mapTypedConstOrNull :: Monad m => (Typed Const -> m (Typed Const)) -> TypedConstOrNull -> m TypedConstOrNull
mapTypedConstOrNull f (TypedConst tc) = S.liftM TypedConst (f tc)
mapTypedConstOrNull f UntypedNull = return UntypedNull

rnMetaConst2 :: MetaConst -> RD MetaConst
rnMetaConst2 (McRef l) = R.liftM McRef (rnLocalId2 l)
rnMetaConst2 (McStruct l) = R.liftM McStruct (S.mapM (mapMetaKindedConst rnMetaConst2) l) -- Const (rnConst2 c)
rnMetaConst2 (McSimple sc) = return $ McSimple sc
rnMetaConst2 x = return x

rnMetaKindedConst2 :: MetaKindedConst -> RD MetaKindedConst
rnMetaKindedConst2 x = case x of
  (MetaKindedConst mk mc) -> R.liftM (MetaKindedConst mk) (rnMetaConst2 mc)
  UnmetaKindedNull -> return UnmetaKindedNull  

rnPercentLabel2 :: GlobalId -> PercentLabel -> RD PercentLabel
rnPercentLabel2 g (PercentLabel x) = R.liftM PercentLabel (rnLabelId2 g x)

rnMyPercentLabel2 :: PercentLabel -> RD PercentLabel 
rnMyPercentLabel2 x = do { r <- R.ask
                         ; rnPercentLabel2 (fst r) x
                         }

rnLabelId2 :: GlobalId -> LabelId -> RD LabelId
rnLabelId2 g l = case l of
  LabelNumber i -> do { b <- isImplicitNum g i
                      ; if b then return $ LabelString (implicitLbPrefix ++ show i)
                        else do { c <- isExplicitNum g i
                                ; if c then return $ LabelString (explicitLbPrefix ++ show i)
                                  else do { d <- isExplicitDqNum g i     
                                          ; if d then return $ LabelDqNumber i
                                            else error ("what happend??? " ++ show l)
                                          }
                                }
                      }
  LabelDqNumber i -> do { b <- isExplicitDqNum g i
                        ; if b then return l
                          else do { c <- isExplicitNum g i     
                                  ; if c then return $ LabelString (explicitLbPrefix ++ show i)
                                    else do { x <- R.ask
                                            ; error ("what happend??? " ++ show l ++ " " ++ show x)
                                            }
                                  }
                        }
  _ -> return l

isImplicitNum :: GlobalId -> Word32 {-Integer-} -> RD Bool
isImplicitNum g i = do { (_,r) <- R.ask
                       ; case M.lookup g r of
                         Nothing -> return False
                         Just st -> return $ St.member i (implicitNumbers st) 
                       }
                    
isExplicitNum :: GlobalId -> Word32 {-Integer-} -> RD Bool
isExplicitNum g i = do { (_,r) <- R.ask
                       ; case M.lookup g r of
                         Nothing -> return False
                         Just st -> return $ St.member i (explicitNumbers st) 
                       }

isExplicitDqNum :: GlobalId -> Word32 {-Integer-} -> RD Bool
isExplicitDqNum g i = do { (_,r) <- R.ask
                         ; case M.lookup g r of
                           Nothing -> return False
                           Just st -> return $ St.member i (explicitDqNumbers st) 
                         }

rnPointer2 :: (a -> RD a) -> Pointer a -> RD (Pointer a)
rnPointer2 f (Pointer v) = S.liftM Pointer (f v)

rnExpr2 :: Expr -> RD Expr
rnExpr2 (EgEp e) = S.liftM EgEp (rnGetElemPtr2 rnTypedValue2 e)
rnExpr2 (EiC e) = S.liftM EiC (rnIcmp2 rnValue2 e)
rnExpr2 (EfC e) = S.liftM EfC (rnFcmp2 rnValue2 e)
rnExpr2 (Eb e) = S.liftM Eb (rnBinExpr2 rnValue2 e)
rnExpr2 (Ec e) = S.liftM Ec (rnConversion2 rnTypedValue2 e)
rnExpr2 (Es e) = S.liftM Es (rnSelect2 rnTypedValue2 e)


rnGetElemPtr2 :: (Typed v -> RD (Typed v)) -> GetElementPtr v -> RD (GetElementPtr v)
rnGetElemPtr2 fv (GetElementPtr b v vs) = do { v' <- rnPointer2 fv v
                                             ; vs' <- mapM fv vs
                                             ; return $ GetElementPtr b v' vs'
                                             }

rnIcmp2 :: (a -> RD a) -> Icmp a -> RD (Icmp a)
rnIcmp2 f (Icmp o t v1 v2) = S.liftM2 (Icmp o t) (f v1) (f v2)

rnFcmp2 :: (a -> RD a) -> Fcmp a -> RD (Fcmp a)
rnFcmp2 f (Fcmp o t v1 v2) = S.liftM2 (Fcmp o t) (f v1) (f v2)

rnIbinExpr2 :: (a -> RD a) -> IbinExpr a -> RD (IbinExpr a)
rnIbinExpr2 f (IbinExpr o tf t v1 v2) = S.liftM2 (IbinExpr o tf t) (f v1) (f v2)

rnFbinExpr2 :: (a -> RD a) -> FbinExpr a -> RD (FbinExpr a)
rnFbinExpr2 f (FbinExpr o tf t v1 v2) = S.liftM2 (FbinExpr o tf t) (f v1) (f v2)

rnBinExpr2 :: (a -> RD a) -> BinExpr a -> RD (BinExpr a)
rnBinExpr2 f (Ie x) = S.liftM Ie (rnIbinExpr2 f x)
rnBinExpr2 f (Fe x) = S.liftM Fe (rnFbinExpr2 f x)

rnConversion2 :: (Typed a -> RD (Typed a)) -> Conversion a -> RD (Conversion a)
rnConversion2 f (Conversion o v t) = S.liftM (\x -> Conversion o x t) (f v)

rnSelect2 :: (Typed a -> RD (Typed a)) -> Select a -> RD (Select a)
rnSelect2 f (Select v1 v2 v3) = S.liftM3 Select (f v1) (f v2) (f v3)

rnValue2 :: Value -> RD Value
rnValue2 (Val_local x) = S.liftM Val_local (rnLocalId2 x)
rnValue2 (Val_const x) = S.liftM Val_const (rnConst2 x)


rnTypedValue2 :: Typed Value -> RD (Typed Value)
rnTypedValue2 (Typed t v) = S.liftM (Typed t) (rnValue2 v)

rnLocalId2 :: LocalId -> RD LocalId
rnLocalId2 x = return x;

rnExtractElem2 :: (Typed a -> RD (Typed a)) -> ExtractElement a -> RD (ExtractElement a)
rnExtractElem2 f (ExtractElement v1 v2) = S.liftM2 ExtractElement (f v1) (f v2)

rnInsertElem2 :: (Typed a -> RD (Typed a)) -> InsertElement a -> RD (InsertElement a)
rnInsertElem2 f (InsertElement v1 v2 v3) = S.liftM3 InsertElement (f v1) (f v2) (f v3)

rnShuffleVector2 :: (Typed a -> RD (Typed a)) -> ShuffleVector a -> RD (ShuffleVector a)
rnShuffleVector2 f (ShuffleVector v1 v2 v3) = S.liftM3 ShuffleVector (f v1) (f v2) (f v3)


rnExtractValue2 :: (Typed a -> RD (Typed a)) -> ExtractValue a -> RD (ExtractValue a)
rnExtractValue2 f (ExtractValue v s) = S.liftM (\x -> ExtractValue x s) (f v)


rnInsertValue2 :: (Typed a -> RD (Typed a)) -> InsertValue a -> RD (InsertValue a)
rnInsertValue2 f (InsertValue v1 v2 s) = S.liftM2 (\x y -> InsertValue x y s)
                                         (f v1) (f v2)


simplify :: Module -> Module
simplify (Module ml) = let (sl, ml1) = iter1 ml
                       in Module (iter2 ml1 sl)
