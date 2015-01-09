{-# OPTIONS_GHC -cpp #-}
module Llvm.VmCore.AstCanonicalization (canonicalize) where

import Llvm.VmCore.Ast
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Data.Maybe
import Llvm.VmCore.Converter
import Debug.Trace

data MyState = MyState { _cnt :: Integer
                       , _renaming :: String
                       } 
               
defaultMyState n f = MyState n f
               
data FRef s a = FRef { get :: s -> a               
                     , set :: a -> s -> s
                     }
                
modify :: FRef s a -> (a -> a) -> (s -> s)
modify ref f s = set ref (f (get ref s)) s
                
                 

cnt :: FRef MyState Integer                
cnt = FRef { get = \(MyState c _) -> c
           , set = \v (MyState _ r) -> MyState v r
           }
      
renaming :: FRef MyState String
renaming = FRef { get = \(MyState _ r) -> r
                , set = \v (MyState c _) -> MyState c v
                }           
           
           

type MS = S.State MyState

idPrefix = "localId"
lbPrefix = "localLb"

canonicalize :: Module -> Module
canonicalize (Module l) = Module (fmap rnToplevel l)

rnToplevel :: Toplevel -> Toplevel
rnToplevel (ToplevelDefine fp bs) = let (fp2, bs2) = S.evalState (do { fp' <- rnDefFunctionPrototype fp
                                                                     ; bs' <- S.mapM rnBlock bs
                                                                     ; return (fp', bs')
                                                                     }) (defaultMyState 0 (show fp))
                                    in (ToplevelDefine fp2 bs2)
rnToplevel x = x



rnDefFunctionPrototype :: FunctionPrototype -> MS FunctionPrototype
rnDefFunctionPrototype (FunctionPrototype link vis cconv fas t g fp fnd fa sec cmd align gc prefix prologue) = 
  do { fp' <- rnDefFormalParamList fp
     ; prefix' <- rnPrefix prefix
     ; prologue' <- rnPrologue prologue
     ; return $ FunctionPrototype link vis cconv fas t g fp' fnd fa sec cmd align gc prefix' prologue'
     }
  
  
  
rnDefFormalParamList :: FormalParamList -> MS FormalParamList  
rnDefFormalParamList (FormalParamList fps b fa) = do { fps' <- mapM rnDefFormalParam fps
                                                     ; return $ FormalParamList fps' b fa
                                                     }
                                               
rnDefFormalParam :: FormalParam -> MS FormalParam
rnDefFormalParam (FormalParam t pas1 align pid pas2) = do { pid' <- rnDefFparam pid
                                                          ; return $ FormalParam t pas1 align pid' pas2
                                                          }

rnDefFparam :: Fparam -> MS Fparam 
rnDefFparam (FimplicitParam) = do { x <- getNext
                                  ; x' <- (rnDefLocalId (LocalIdNum x))
                                  ; return $ FexplicitParam x'
                                  }
rnDefFparam (FexplicitParam x) = S.liftM FexplicitParam (rnDefLocalId x)
  

rnLabelId :: LabelId -> MS LabelId
rnLabelId (LabelNumber i) = return $ LabelString $ Lstring (lbPrefix ++ show i)
rnLabelId (LabelQuoteNumber i) = return $ LabelQuoteString $ Lstring (lbPrefix ++ show i)
rnLabelId x@(LabelQuoteString (Lstring s)) = case reads s :: [(Integer, String)] of
                                               [(5, "")] -> return $ LabelQuoteString $ Lstring (lbPrefix ++ s)
                                               _ -> return x
rnLabelId x = return x


getNext :: MS Integer
getNext = do { s <- S.get
             ; let x = get cnt s
             ; S.put (set cnt (x+1) s)
             ; return x
             }
          
getFp :: MS String          
getFp = do { s <- S.get
           ; let x = get renaming s
           ; return x
           }

rnDefLabel :: BlockLabel -> MS BlockLabel
#ifdef DEBUG
rnDefLabel bl | trace ("rnLabel " ++ (show bl)) False = undefined
#endif
rnDefLabel (ImplicitBlockLabel) = do { i <- getNext
                                     ; i' <- rnLabelId (LabelNumber i)
                                     ; return $ ExplicitBlockLabel i'
                                     }
rnDefLabel (ExplicitBlockLabel i) = S.liftM ExplicitBlockLabel (rnLabelId i)



rnBlock :: Block -> MS Block
rnBlock (Block lbl phi comp term) = do { lbl' <- rnDefLabel lbl
                                       ; phi' <- mapM rnPhi phi
                                       ; comp' <- mapM rnComputingInstWithDbg comp
                                       ; term' <- rnTerminatorInstWithDbg term
                                       ; return (Block lbl' phi' comp' term')
                                       }


{-
rnDefId :: Integer -> MS Integer
rnDefId i = do { ms <- S.get
               ; let n = get cnt ms
                     f = get renaming ms
               ; let (g0, ms0) = if i > n then (n, MyState (n+1) (M.insert i n f)) 
                                 else if i == n then (i, MyState (n+1) f)
                                      else error (" i:" ++ (show i) ++ " <  n:" ++ (show n))
               ; S.put ms0
               ; return g0
               }
            
rnId :: Integer -> MS Integer            
rnId i = do { ms <- S.get
            ; let f = get renaming ms
                  v' = case M.lookup i f of
                        Just i' -> i'
                        Nothing -> i
            ; return v'
            }
-}

rnDefLocalId :: LocalId -> MS LocalId
#ifdef DEBUG
rnDefLocalId l | trace ("rnDefLocalId " ++ (show l)) False = undefined
#endif
rnDefLocalId (LocalIdNum i) = S.liftM (LocalIdAlphaNum . Lstring) (return (idPrefix ++ show i))
rnDefLocalId x = return x


rnLhs :: GlobalOrLocalId -> MS GlobalOrLocalId
-- rnLhs g | trace ("rnLhs is called with " ++ (show g)) False = undefined
rnLhs (GolL localId) = S.liftM GolL (rnDefLocalId localId)
rnLhs x = return x


rnPhi :: PhiInst -> MS PhiInst
rnPhi (PhiInst lhs t ins) = do { maybe (return ()) checkImpCount lhs
                               ; lhs' <- maybeM rnLhs lhs
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
-- rnComputingInst x | trace ("rnComputingInst " ++ show (x)) False = undefined
rnComputingInst (ComputingInst lhs rhs) = do { maybe (return ()) checkImpCount lhs
                                             ; lhs' <- maybeM rnLhs lhs
                                             ; (b, rhs') <- rnRhs rhs
                                             ; lhs'' <- getLhs b lhs'
                                             ; return (ComputingInst lhs'' rhs')
                                             }

rnLocalId :: LocalId -> MS LocalId
rnLocalId (LocalIdNum i) = S.liftM (LocalIdAlphaNum . Lstring) (return (idPrefix ++ show i))
rnLocalId x = return x  


getLhs :: Bool -> Maybe GlobalOrLocalId -> MS (Maybe GlobalOrLocalId)
getLhs False x = return x
getLhs True (Just x) = return $ Just x
getLhs True Nothing = do { m <- getNext
                         ; m' <- rnLocalId (LocalIdNum m)
                         ; return $ Just $ GolL m'
                         }

rnGlobalOrLocalId :: GlobalOrLocalId -> MS GlobalOrLocalId
rnGlobalOrLocalId (GolL l) = S.liftM GolL (rnLocalId l)
rnGlobalOrLocalId x = return x

rnValue :: Value -> MS Value
rnValue (VgOl x) = S.liftM VgOl (rnGlobalOrLocalId x)
rnValue (Vc x) = S.liftM Vc (rnConst x)
rnValue (Ve x) = S.liftM Ve (rnExpr x)
rnValue x = return x

                                         
rnTypedValue :: TypedValue -> MS TypedValue
rnTypedValue (TypedValue t v) = S.liftM (TypedValue t) (rnValue v)
                                         

checkImpCount :: GlobalOrLocalId -> MS ()
checkImpCount x@(GolL (LocalIdNum i)) = do { m <- getNext
                                           ; if (i == m) then
                                               return ()
                                             else 
                                               do { y <- getFp
                                                  ; error ("expect " ++ (show m) ++ " and but found " ++ (show x) ++ " in " ++ y)
                                                  }
                                           }
checkImpCount _ = return ()                
                                         
rnRhs :: Rhs -> MS (Bool, Rhs)
rnRhs lhs = case lhs of
              RmO x -> S.liftM (\(b,x) -> (b, RmO x)) (rnMemOp x)
              Re x -> S.liftM (\x -> (True, Re x)) (rnExpr x)
              Call b cs -> S.liftM (\(bb, x) -> (bb, Call b x)) (rnCallSite cs)
              ReE e -> S.liftM (\x -> (True, ReE x)) (rnExtractElem rnTypedValue e)
              RiE e -> S.liftM (\x -> (True, RiE x)) (rnInsertElem rnTypedValue e)
              RsV e -> S.liftM (\x -> (True, RsV x)) (rnShuffleVector rnTypedValue e)
              ReV e -> S.liftM (\x -> (True, ReV x)) (rnExtractValue rnTypedValue e)
              RiV e -> S.liftM (\x -> (True, RiV x)) (rnInsertValue rnTypedValue e)
              VaArg tv t -> S.liftM (\x -> (True, VaArg x t)) (rnTypedValue tv)
              LandingPad rt ft ix cl c -> return (True, lhs)
              
rnMemOp :: MemOp -> MS (Bool, MemOp)
rnMemOp (Alloca m t tv a) = do { tv' <- maybeM rnTypedValue tv
                               ; return (True, Alloca m t tv' a)
                               }
-- rnMemOp (Free tv) = S.liftM (\x -> (False, Free x)) (rnTypedValue tv)
rnMemOp (Load a tp ma nt inv nl) = S.liftM (\x -> (True, Load a x ma nt inv nl)) (rnTypedPointer tp)
rnMemOp (LoadAtomic at a tp ma) = S.liftM (\x -> (True, LoadAtomic at a x ma)) (rnTypedPointer tp)
rnMemOp (Store a tv tp ma nt) = S.liftM2 (\x y -> (False, Store a x y ma nt))
                                (rnTypedValue tv) (rnTypedPointer tp)
rnMemOp (StoreAtomic at a tv tp ma) = S.liftM2 (\x y -> (False, StoreAtomic at a x y ma))
                                      (rnTypedValue tv) (rnTypedPointer tp)
rnMemOp (Fence b f) = return (False, Fence b f)
rnMemOp (CmpXchg wk b1 tp tv1 tv2 b2 mf ff) = 
  S.liftM3 (\x y z -> (False, CmpXchg wk b1 x y z b2 mf ff))
  (rnTypedPointer tp)
  (rnTypedValue tv1)
  (rnTypedValue tv2)
rnMemOp (AtomicRmw b1 ao tp tv b2 mf) = 
  S.liftM2 (\x y -> (False, AtomicRmw b1 ao x y b2 mf))
  (rnTypedPointer tp)
  (rnTypedValue tv)

rnTypedPointer :: TypedPointer -> MS TypedPointer
rnTypedPointer (TypedPointer t p) = S.liftM (TypedPointer t) (rnPointer p)

rnPointer :: Pointer -> MS Pointer
rnPointer (Pointer v) = S.liftM Pointer (rnValue v)

rnExpr :: Expr -> MS Expr
rnExpr (EgEp e) = S.liftM EgEp (rnGetElemPtr rnTypedValue e)
rnExpr (EiC e) = S.liftM EiC (rnIcmp rnValue e)
rnExpr (EfC e) = S.liftM EfC (rnFcmp rnValue e)
rnExpr (Eb e) = S.liftM Eb (rnBinExpr rnValue e)
rnExpr (Ec e) = S.liftM Ec (rnConversion rnTypedValue e)
rnExpr (Es e) = S.liftM Es (rnSelect rnTypedValue e)


rnGetElemPtr :: (a -> MS a) -> GetElemPtr a -> MS (GetElemPtr a)
rnGetElemPtr f (GetElemPtr b v vs) = do { v' <- f v
                                        ; vs' <- mapM f vs
                                        ; return $ GetElemPtr b v' vs'
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

rnConversion :: (a -> MS a) -> Conversion a -> MS (Conversion a)
rnConversion f (Conversion o v t) = S.liftM (\x -> Conversion o x t) (f v)

rnSelect :: (a -> MS a) -> Select a -> MS (Select a)
rnSelect f (Select v1 v2 v3) = S.liftM3 Select (f v1) (f v2) (f v3)


rnFunName :: FunName -> MS FunName
rnFunName (FunNameGlobal g) = S.liftM FunNameGlobal (rnGlobalOrLocalId g)
rnFunName x = return x
  

rnCallSite :: CallSite -> MS (Bool, CallSite)
rnCallSite (CallFun c pa t fn aps fas) = do { fn' <- rnFunName fn
                                            ; x <- mapM rnActualParam aps
                                            ; return (not $ isVoidType t, CallFun c pa t fn' x fas)
                                            }
rnCallSite (CallAsm t dia b1 b2 qs1 qs2 aps fas) = 
  S.liftM 
  (\x -> (not $ isVoidType t, CallAsm t dia b1 b2 qs1 qs2 x fas))
  (mapM rnActualParam aps)
rnCallSite (CallConversion pas t c aps fas) = S.liftM
                                              (\x -> (not $ isVoidType t, CallConversion pas t c x fas))
                                              (mapM rnActualParam aps)

rnActualParam :: ActualParam -> MS ActualParam
rnActualParam (ActualParam t ps ma v pa) = S.liftM (\x -> ActualParam t ps ma x pa)
                                           (rnValue v)
                                           
                                           
rnExtractElem :: (a -> MS a) -> ExtractElem a -> MS (ExtractElem a)
rnExtractElem f (ExtractElem v1 v2) = S.liftM2 ExtractElem (f v1) (f v2)

rnInsertElem :: (a -> MS a) -> InsertElem a -> MS (InsertElem a)
rnInsertElem f (InsertElem v1 v2 v3) = S.liftM3 InsertElem (f v1) (f v2) (f v3)

rnShuffleVector :: (a -> MS a) -> ShuffleVector a -> MS (ShuffleVector a)
rnShuffleVector f (ShuffleVector v1 v2 v3) = S.liftM3 ShuffleVector (f v1) (f v2) (f v3)


rnExtractValue :: (a -> MS a) -> ExtractValue a -> MS (ExtractValue a)
rnExtractValue f (ExtractValue v s) = S.liftM (\x -> ExtractValue x s) (f v)


rnInsertValue :: (a -> MS a) -> InsertValue a -> MS (InsertValue a)
rnInsertValue f (InsertValue v1 v2 s) = S.liftM2 (\x y -> InsertValue x y s)
                                        (f v1) (f v2)
                                        
  
rnTerminatorInstWithDbg :: TerminatorInstWithDbg -> MS TerminatorInstWithDbg
rnTerminatorInstWithDbg (TerminatorInstWithDbg t ds) = S.liftM (\x -> TerminatorInstWithDbg x ds) (rnTerminatorInst t)


rnTerminatorInst :: TerminatorInst -> MS TerminatorInst
rnTerminatorInst (Return ls) = S.liftM Return (mapM rnTypedValue ls)
rnTerminatorInst (Br l) = S.liftM Br (rnTargetLabel l)
rnTerminatorInst (Cbr v tl fl) = S.liftM3 Cbr (rnValue v) (rnTargetLabel tl) (rnTargetLabel fl)
rnTerminatorInst (Switch t d cases) = S.liftM3 Switch
                                      (rnTypedValue t) (rnTargetLabel d) (mapM (\(x,y) -> S.liftM2 (,) 
                                                                                          (rnTypedValue x) 
                                                                                          (rnTargetLabel y)
                                                                               ) cases)
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
rnMetaConst (MdRef l) = S.liftM MdRef (rnLocalId l)
rnMetaConst (MdConst c) = S.liftM MdConst (rnConst c)
rnMetaConst x = return x


rnConst :: Const -> MS Const
rnConst (Cca x) = S.liftM Cca (rnComplexConstant x)
rnConst (CmL x) = S.liftM CmL (rnLocalId x)
rnConst (Cl l) = S.liftM Cl (rnLabelId l)
rnConst (CblockAddress g l) = S.liftM (CblockAddress g) (rnPercentLabel l)
rnConst (Cb bexpr) = S.liftM Cb (rnBinExpr rnConst bexpr)
rnConst (Cconv convert) = S.liftM Cconv (rnConversion rnTypedConst convert)
rnConst (CgEp getelm) = S.liftM CgEp (rnGetElemPtr rnTypedConst getelm)
rnConst (Cs select) = S.liftM Cs (rnSelect rnTypedConst select)
rnConst (CiC icmp) = S.liftM CiC (rnIcmp rnConst icmp)
rnConst (CfC fcmp) = S.liftM CfC (rnFcmp rnConst fcmp)
rnConst (CsV shuffle) = S.liftM CsV (rnShuffleVector rnTypedConst shuffle)
rnConst (CeV extract) = S.liftM CeV (rnExtractValue rnTypedConst extract)
rnConst (CiV insert) = S.liftM CiV (rnInsertValue rnTypedConst insert)
rnConst (CeE extract) = S.liftM CeE (rnExtractElem rnTypedConst extract)
rnConst (CiE insert) = S.liftM CiE (rnInsertElem rnTypedConst insert)
rnConst (CmC x) = S.liftM CmC (rnMetaConst x)
rnConst x = return x



rnComplexConstant :: ComplexConstant -> MS ComplexConstant
rnComplexConstant (Cstruct b l) = S.liftM (Cstruct b) (S.mapM rnTypedConst l)
rnComplexConstant (Carray l) = S.liftM Carray (S.mapM rnTypedConst l)
rnComplexConstant (Cvector l) = S.liftM Cvector (S.mapM rnTypedConst l)
                                  
rnTypedConst :: TypedConst -> MS TypedConst                                  
rnTypedConst (TypedConst t c) = S.liftM (TypedConst t) (rnConst c)
rnTypedConst TypedConstNull = return $ TypedConstNull


rnPrefix :: Maybe Prefix -> MS (Maybe Prefix)
rnPrefix (Just (Prefix n)) = S.liftM (Just . Prefix) (rnTypedConst n)
rnPrefix _ = return Nothing

rnPrologue :: Maybe Prologue -> MS (Maybe Prologue)
rnPrologue (Just (Prologue n)) = S.liftM (Just . Prologue) (rnTypedConst n)
rnPrologue _ = return Nothing