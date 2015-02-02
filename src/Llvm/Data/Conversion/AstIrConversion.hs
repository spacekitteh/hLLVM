{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Llvm.Data.Conversion.AstIrConversion(astToIr, irToAst) where

import qualified Compiler.Hoopl as H
import qualified Control.Monad as Md
import qualified Data.Map as M
import qualified Llvm.Data.Ast as A
import qualified Llvm.Data.Ir as I
import Llvm.Data.Conversion.LabelMapM
import Llvm.Util.Monadic (maybeM, pairM)


class Conversion l1 l2 | l1 -> l2 where
  convert :: l1 -> l2

type MyLabelMapM = LabelMapM H.SimpleUniqueMonad


{- Ast to Ir conversion -}

-- the real differences between Ast and Ir
-- 1. Ir uses Unique values as labels while Ast can use any strings as labels
-- 2. All unreachable code are removed in Ir
instance Conversion (A.LabelId) (MyLabelMapM I.LabelId) where
  convert l@(A.LabelString _) = Md.liftM I.LabelString (labelFor $ A.labelIdToLstring l)
  convert l@(A.LabelDqString _) = Md.liftM I.LabelDqString (labelFor $ A.labelIdToLstring l)
  convert l@(A.LabelNumber _) = Md.liftM I.LabelNumber (labelFor $ A.labelIdToLstring l)
  convert l@(A.LabelDqNumber _) = Md.liftM I.LabelDqNumber (labelFor $ A.labelIdToLstring l)

instance Conversion A.PercentLabel (MyLabelMapM I.PercentLabel) where
  convert (A.PercentLabel l) = Md.liftM I.PercentLabel (convert l)

instance Conversion A.TargetLabel (MyLabelMapM I.TargetLabel) where
  convert (A.TargetLabel tl) = Md.liftM I.TargetLabel (convert tl)

instance Conversion A.BlockLabel (MyLabelMapM I.BlockLabel) where
  convert (A.ImplicitBlockLabel p) = error $ "ImplicitBlockLabel @" ++ show p ++ " should be normalized away in AstSimplification, and should not be leaked to Ast2Ir."
  convert (A.ExplicitBlockLabel b) = Md.liftM I.BlockLabel (convert b)

{-
instance Conversion u v => Conversion (A.Typed u) (MyLabelMapM (I.Typed v)) where
  convert (A.TypedData t x) = Md.liftM (I.TypedData t) (convert x)
-}

instance Conversion (A.Typed A.Const) (MyLabelMapM (I.Typed I.Const)) where
  convert (A.TypedData t c) = Md.liftM (I.TypedData t) (convert c)
  convert A.UntypedNull = return I.UntypedNull

instance Conversion (A.Typed A.Value) (MyLabelMapM (I.Typed I.Value)) where
  convert (A.TypedData t v) = Md.liftM (I.TypedData t) (convert v)

instance Conversion (A.Typed A.Pointer) (MyLabelMapM (I.Typed I.Pointer)) where
  convert (A.TypedData t v) = Md.liftM (I.TypedData t) (convert v)

instance Conversion A.ComplexConstant (MyLabelMapM I.ComplexConstant) where
  convert (A.Cstruct b fs) = Md.liftM (I.Cstruct b) (mapM convert fs)
  convert (A.Cvector fs) = Md.liftM I.Cvector (mapM convert fs)
  convert (A.Carray fs) = Md.liftM I.Carray (mapM convert fs)

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.IbinExpr v1) (MyLabelMapM (I.IbinExpr v2)) where
  convert (A.IbinExpr op cs t u1 u2) = Md.liftM2 ((convertIop op cs) t) (convert u1) (convert u2)
      where
        convertIop op cs = case op of
          A.Add -> I.Add (getnowrap cs)
          A.Sub -> I.Sub (getnowrap cs)
          A.Mul -> I.Mul (getnowrap cs)
          A.Udiv -> I.Udiv (getexact cs)
          A.Sdiv -> I.Sdiv (getexact cs)
          A.Shl -> I.Shl (getnowrap cs)
          A.Lshr -> I.Lshr (getexact cs)
          A.Ashr -> I.Ashr (getexact cs)
          A.Urem -> I.Urem
          A.Srem -> I.Srem
          A.And -> I.And
          A.Or -> I.Or
          A.Xor -> I.Xor
        getnowrap x = case x of
          [A.Nsw] -> Just I.Nsw
          [A.Nuw] -> Just I.Nuw
          [A.Nsw,A.Nuw] -> Just I.Nsuw
          [A.Nuw,A.Nsw] -> Just I.Nsuw
          [] -> Nothing
          _ -> error ("irrefutable error1 " ++ show cs)
        getexact x = case x of
          [A.Exact] -> Just I.Exact
          [] -> Nothing
          _ -> error "irrefutable error2"

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.FbinExpr v1) (MyLabelMapM (I.FbinExpr v2)) where
  convert (A.FbinExpr op cs t u1 u2) = Md.liftM2 ((convertFop op) cs t) (convert u1) (convert u2)
    where
      convertFop op = case op of
        A.Fadd -> I.Fadd
        A.Fsub -> I.Fsub
        A.Fmul -> I.Fmul
        A.Fdiv -> I.Fdiv
        A.Frem -> I.Frem

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.BinExpr v1) (MyLabelMapM (I.BinExpr v2)) where
  convert (A.Ie e) = Md.liftM I.Ie (convert e)
  convert (A.Fe e) = Md.liftM I.Fe (convert e)


instance Conversion v1 (MyLabelMapM v2) => Conversion (A.Conversion v1) (MyLabelMapM (I.Conversion v2)) where
  convert (A.Conversion op u t) = convert u >>= \u' -> return $ I.Conversion op u' t

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.GetElemPtr v1) (MyLabelMapM (I.GetElemPtr v2)) where
  convert (A.GetElemPtr b u us) = Md.liftM2 (I.GetElemPtr b) (convert u) (mapM convert us)

instance (Conversion v1 (MyLabelMapM v2)) => Conversion (A.Select v1) (MyLabelMapM (I.Select v2)) where
  convert (A.Select u1 u2 u3) = Md.liftM3 I.Select (convert u1) (convert u2) (convert u3)

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.Icmp v1) (MyLabelMapM (I.Icmp v2)) where
  convert (A.Icmp op t u1 u2) = Md.liftM2 (I.Icmp op t) (convert u1) (convert u2)
    
instance Conversion v1 (MyLabelMapM v2) => Conversion (A.Fcmp v1) (MyLabelMapM (I.Fcmp v2)) where
  convert (A.Fcmp op t u1 u2) = Md.liftM2 (I.Fcmp op t) (convert u1) (convert u2)

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.ShuffleVector v1) (MyLabelMapM (I.ShuffleVector v2)) where
  convert (A.ShuffleVector u1 u2 u3) = Md.liftM3 I.ShuffleVector (convert u1) (convert u2) (convert u3)

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.ExtractValue v1) (MyLabelMapM (I.ExtractValue v2)) where
  convert (A.ExtractValue u s) = convert u >>= \u' -> return $ I.ExtractValue u' s

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.InsertValue v1) (MyLabelMapM (I.InsertValue v2)) where
  convert (A.InsertValue u1 u2 s) = do { u1' <- convert u1
                                       ; u2' <- convert u2
                                       ; return $ I.InsertValue u1' u2' s
                                       }

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.ExtractElem v1) (MyLabelMapM (I.ExtractElem v2)) where
  convert (A.ExtractElem u1 u2) = Md.liftM2 I.ExtractElem (convert u1) (convert u2)

instance Conversion v1 (MyLabelMapM v2) => Conversion (A.InsertElem v1) (MyLabelMapM (I.InsertElem v2)) where
  convert (A.InsertElem u1 u2 u3) = Md.liftM3 I.InsertElem (convert u1) (convert u2) (convert u3)

instance Conversion A.Const (MyLabelMapM I.Const) where
    convert x =
        case x of
          A.Ccp a -> return $ I.Ccp a
          A.Cca a -> Md.liftM I.Cca (convert a)
          A.CmL a -> return $ I.CmL a
          A.Cl a -> Md.liftM I.Cl (convert a)
          A.CblockAddress g a -> do { a' <- convert a
                                    ; return $ I.CblockAddress g a'
                                    }
          A.Cb a -> Md.liftM I.Cb (convert a)
          A.Cconv a -> Md.liftM I.Cconv (convert a)
          A.CgEp a -> Md.liftM I.CgEp (convert a)
          A.Cs a -> Md.liftM I.Cs (convert a)
          A.CiC a -> Md.liftM I.CiC (convert a)
          A.CfC a -> Md.liftM I.CfC (convert a)
          A.CsV a -> Md.liftM I.CsV (convert a)
          A.CeV a -> Md.liftM I.CeV (convert a)
          A.CiV a -> Md.liftM I.CiV (convert a)
          A.CeE a -> Md.liftM I.CeE (convert a)
          A.CiE a -> Md.liftM I.CiE (convert a)
          A.CmC a -> Md.liftM I.CmC (convert a)

instance Conversion A.MdVar (MyLabelMapM I.MdVar) where
  convert (A.MdVar s) = return $ I.MdVar s

instance Conversion A.MdNode (MyLabelMapM I.MdNode) where
  convert (A.MdNode s) = return $ I.MdNode s

instance Conversion A.MetaConst (MyLabelMapM I.MetaConst) where
  convert (A.MdConst c) = convert c >>= return . I.MdConst
  convert (A.MdString s) = return $ I.MdString s
  convert (A.McMn n) = convert n >>= return . I.McMn
  convert (A.McMv n) = convert n >>= return . I.McMv
  convert (A.MdRef i) = return $ I.MdRef i


instance Conversion A.Expr (MyLabelMapM I.Expr) where
  convert (A.EgEp c) = Md.liftM I.EgEp (convert c)
  convert (A.EiC a) = Md.liftM I.EiC (convert a)
  convert (A.EfC a) = Md.liftM I.EfC (convert a)
  convert (A.Eb a) = Md.liftM I.Eb (convert a)
  convert (A.Ec a) = Md.liftM I.Ec (convert a)
  convert (A.Es a) = Md.liftM I.Es (convert a)

instance Conversion A.MemOp (MyLabelMapM I.MemOp) where
    convert (A.Alloca mar t mtv ma) = maybeM convert mtv >>= \x -> return $ I.Allocate mar t x ma
    convert (A.Load atom tv aa nonterm inv nonul) = convert tv >>= \tv' -> return $ I.Load atom tv' aa nonterm inv nonul
    convert (A.LoadAtomic at v tv aa) = convert tv >>= \tv' -> return $ I.LoadAtomic at v tv' aa
    convert (A.Store atom tv1 tv2 aa nt) = do { tv1' <- convert tv1
                                              ; tv2' <- convert tv2
                                              ; return $ I.Store atom tv1' tv2' aa nt
                                              }
    convert (A.StoreAtomic atom v tv1 tv2 aa) = do { tv1' <- convert tv1
                                                   ; tv2' <- convert tv2
                                                   ; return $ I.StoreAtomic atom v tv1' tv2' aa
                                                   }

    convert (A.CmpXchg wk b1 tv1 tv2 tv3 b2 mf ff) = do { tv1' <- convert tv1
                                                        ; tv2' <- convert tv2
                                                        ; tv3' <- convert tv3
                                                        ; return $ I.CmpXchg wk b1 tv1' tv2' tv3' b2 mf ff
                                                        }
    convert (A.AtomicRmw b1 op tv1 tv2 b2 mf) = do { tv1' <- convert tv1
                                                   ; tv2' <- convert tv2
                                                   ; return $ I.AtomicRmw b1 op tv1' tv2' b2 mf
                                                   }
    convert (A.Fence b fo) = return $ I.Fence b fo



instance Conversion A.FunName (MyLabelMapM I.FunName) where
    convert (A.FunNameGlobal g) = return $ I.FunNameGlobal g
    convert (A.FunNameString s) = return $ I.FunNameString s

instance Conversion A.Value (MyLabelMapM I.Value) where
    convert (A.VgOl a) = return $ I.VgOl a
    convert (A.Ve a) = Md.liftM I.Ve (convert a)
    convert (A.Vc a) = Md.liftM I.Vc (convert a)

instance Conversion A.Pointer (MyLabelMapM I.Pointer) where
    convert (A.Pointer a) = Md.liftM I.Pointer (convert a)


instance Conversion A.CallSite (MyLabelMapM I.CallSite) where
    convert  (A.CsFun cc pa t fn aps fa) = do { fn' <- convert fn
                                              ; aps' <- mapM convert aps
                                              ; return $ I.CsFun cc pa t fn' aps' fa
                                              }
    convert (A.CsAsm t dia b1 b2 qs1 qs2 as fa) = do { as' <- mapM convert as
                                                     ; return $ I.CsAsm t dia b1 b2 qs1 qs2 as' fa
                                                     }
    convert (A.CsConversion pa t cv as fa) = do { cv' <- convert cv
                                               ; as' <- mapM convert as
                                               ; return $ I.CsConversion pa t cv' as' fa
                                               }

instance Conversion A.Clause (MyLabelMapM I.Clause) where
    convert (A.Catch tv) = Md.liftM I.Catch (convert tv)
    convert (A.Filter tc) = Md.liftM I.Filter (convert tc)
    convert (A.Cco tc) = Md.liftM I.Cco (convert tc)


instance Conversion (A.Type, A.GlobalOrLocalId) (MyLabelMapM (I.Type, I.GlobalOrLocalId)) where
    convert (t, g) = return (t, g)

instance Conversion A.PersFn (MyLabelMapM I.PersFn) where
    convert (A.PersFnId s) = return $ I.PersFnId s
    convert (A.PersFnCast c) = Md.liftM I.PersFnCast (convert c)
    convert (A.PersFnUndef) = return $ I.PersFnUndef
    convert (A.PersFnNull) = return $ I.PersFnNull
    convert (A.PersFnConst c) = Md.liftM I.PersFnConst (convert c)


instance Conversion A.Rhs (MyLabelMapM I.Rhs) where
    convert (A.RmO c) = Md.liftM I.RmO (convert c)
    convert (A.Re e) = Md.liftM I.Re (convert e)
    convert (A.Call b cs) = Md.liftM (I.Call b) (convert cs)
    convert (A.VaArg tv t) = do { tv' <- convert tv
                                ; return $ I.VaArg tv' t
                                }
    convert (A.LandingPad t1 t2 pf b cs) = do { pf' <- convert pf
                                              ; cs' <- mapM convert cs
                                              ; return $ I.LandingPad t1 t2 pf' b cs'
                                              }
    convert (A.ReE a) = Md.liftM I.ReE (convert a)
    convert (A.RiE a) = Md.liftM I.RiE (convert a)
    convert (A.RsV a) = Md.liftM I.RsV (convert a)
    convert (A.ReV a) = Md.liftM I.ReV (convert a)
    convert (A.RiV a) = Md.liftM I.RiV (convert a)


instance Conversion A.ActualParam (MyLabelMapM I.ActualParam) where
    convert (A.ActualParam t pa1 ma v pa2) = convert v >>= \v' -> return $ I.ActualParam t pa1 ma v' pa2

instance Conversion A.Aliasee (MyLabelMapM I.Aliasee) where
    convert (A.AtV tv) = Md.liftM I.AtV (convert tv)
    convert (A.Ac a) = Md.liftM I.Ac (convert a)
    convert (A.AgEp a) = Md.liftM I.AgEp (convert a)

instance Conversion A.Prefix (MyLabelMapM I.Prefix) where
  convert (A.Prefix n) = Md.liftM I.Prefix (convert n)

instance Conversion A.Prologue (MyLabelMapM I.Prologue) where
  convert (A.Prologue n) = Md.liftM I.Prologue (convert n)

instance Conversion a (MyLabelMapM b) => Conversion (Maybe a) (MyLabelMapM (Maybe b)) where
  convert (Just x) = Md.liftM Just (convert x)
  convert Nothing = return Nothing

instance Conversion A.FunctionPrototype (MyLabelMapM I.FunctionPrototype) where
    convert (A.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13 f14) =
      do { f13' <- convert f13
         ; f14' <- convert f14
         ; return $ I.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13' f14'
         }

instance Conversion A.PhiInst (MyLabelMapM I.PhiInst) where
    convert (A.PhiInst mg t branches) = do { branches' <- mapM (pairM convert convert) branches
                                           ; return $ I.PhiInst mg t
                                               (fmap (\x -> (fst x, snd x)) branches')
                                           }

instance Conversion A.ComputingInst (MyLabelMapM I.ComputingInst) where
    convert (A.ComputingInst mg rhs) = Md.liftM (I.ComputingInst mg) (convert rhs)

instance Conversion A.TerminatorInst (MyLabelMapM I.TerminatorInst) where
  convert (A.Return tvs) = Md.liftM I.Return (mapM convert tvs)
  convert (A.Br t) = Md.liftM I.Br (convert t)
  convert (A.Cbr cnd t f) = Md.liftM3 I.Cbr (convert cnd) (convert t) (convert f)
  convert (A.IndirectBr cnd bs) = Md.liftM2 I.IndirectBr (convert cnd) (mapM convert bs)
  convert (A.Switch cnd d cases) = Md.liftM3 I.Switch (convert cnd) (convert d) (mapM (pairM convert convert) cases)
  convert (A.Invoke mg cs t f) = Md.liftM3 (I.Invoke mg) (convert cs) (convert t) (convert f)
  convert (A.Resume tv) = Md.liftM I.Resume (convert tv)
  convert A.Unreachable = return I.Unreachable
  convert A.Unwind = return I.Unwind

instance Conversion A.Dbg (MyLabelMapM I.Dbg) where
  convert (A.Dbg mv mc) = Md.liftM2 I.Dbg (convert mv) (convert mc)

instance Conversion A.PhiInstWithDbg (MyLabelMapM I.PhiInstWithDbg) where
  convert (A.PhiInstWithDbg ins dbgs) = Md.liftM2 I.PhiInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion A.ComputingInstWithDbg (MyLabelMapM I.ComputingInstWithDbg) where
  convert (A.ComputingInstWithDbg ins dbgs) = Md.liftM2 I.ComputingInstWithDbg (convert ins) (mapM convert dbgs)
    
instance Conversion A.TerminatorInstWithDbg (MyLabelMapM I.TerminatorInstWithDbg) where
  convert (A.TerminatorInstWithDbg term dbgs) = Md.liftM2 I.TerminatorInstWithDbg (convert term) (mapM convert dbgs)


toSingleNodeGraph :: A.Block -> MyLabelMapM (H.Graph I.Node H.C H.C)
-- toSingleNodeGraph b | trace ("toSingleNodeGraph " ++ toLlvm b) False = undefined
toSingleNodeGraph (A.Block f  phi ms l) =
  do { f'  <- toFirst f
     ; phi' <- mapM toPhi phi
     ; ms' <- mapM toMid ms
     ; l'  <- toLast l
     ; return $ H.mkFirst f' H.<*> H.mkMiddles phi' H.<*> H.mkMiddles ms' H.<*> H.mkLast l'
     }

toFirst :: A.BlockLabel -> MyLabelMapM (I.Node H.C H.O)
toFirst x = Md.liftM I.Nlabel (convert x)

toPhi :: A.PhiInstWithDbg -> MyLabelMapM (I.Node H.O H.O)
toPhi phi = Md.liftM I.Pinst (convert phi)

toMid :: A.ComputingInstWithDbg -> MyLabelMapM (I.Node H.O H.O)
toMid inst = Md.liftM I.Cinst (convert inst)

toLast :: A.TerminatorInstWithDbg -> MyLabelMapM (I.Node H.O H.C)
toLast inst = Md.liftM I.Tinst (convert inst)

-- | the head must be the entry block
getEntryAndAlist :: [A.Block] -> MyLabelMapM (H.Label, [A.Lstring])
getEntryAndAlist [] = error "Parsed procedures should not be empty"
getEntryAndAlist bs =
  do { l <- convert $ A.blockLabel $ head bs
     ; let l' = case l of
              I.BlockLabel x -> I.toLabel x
     ; let ord = map (\b -> case A.blockLabel b of
                             A.ImplicitBlockLabel p -> error $ "irrefutable implicitblock " ++ show p ++ " should be normalized in AstSimplify" -- A.labelIdToString x
                             A.ExplicitBlockLabel x -> A.labelIdToLstring x
                     ) bs
     ; return (l', ord)
     }

toGraph :: [A.Block] -> MyLabelMapM (H.Graph I.Node H.C H.C)
toGraph bs =
  {-
    It's more likely that only reachable blocks are pulled out and used to create
    a graph, the unreachable blocks are left.
  -}
  do { g <- foldl (Md.liftM2 (H.|*><*|)) (return H.emptyClosedGraph) (map toSingleNodeGraph bs)
     ; getBody g
     }

getBody :: forall n. H.Graph n H.C H.C -> MyLabelMapM (H.Graph n H.C H.C)
getBody graph = LabelMapM f
  where f m = return (m, graph)


blockToGraph :: A.FunctionPrototype -> [A.Block] -> MyLabelMapM (H.Label, H.Graph I.Node H.C H.C)
blockToGraph fn blocks =
  do { (entry, labels) <- getEntryAndAlist blocks
     ; body <- toGraph blocks
     ; addAlist fn labels
     ; appendH2A
     ; return (entry, body)
     }

toplevel2Ir :: A.Toplevel -> MyLabelMapM I.Toplevel
toplevel2Ir (A.ToplevelTriple q) = return $ I.ToplevelTriple q
toplevel2Ir (A.ToplevelDataLayout q) = return $ I.ToplevelDataLayout q
toplevel2Ir (A.ToplevelAlias g v dll tlm na l a) = convert a >>= return . (I.ToplevelAlias g v dll tlm na l)
toplevel2Ir (A.ToplevelDbgInit s i) = return $ I.ToplevelDbgInit s i
toplevel2Ir (A.ToplevelStandaloneMd s tv) = convert tv >>= return . (I.ToplevelStandaloneMd s)
toplevel2Ir (A.ToplevelNamedMd m ns) = do { m' <- convert m
                                          ; ns' <- mapM convert ns
                                          ; return $ I.ToplevelNamedMd m' ns'
                                          }
toplevel2Ir (A.ToplevelDeclare f) = convert f >>= return . I.ToplevelDeclare
toplevel2Ir (A.ToplevelDefine f b) =
    do { f' <- convert f
       ; (e, g) <- blockToGraph f b
       ; return $ I.ToplevelDefine f' e g
       }

toplevel2Ir (A.ToplevelGlobal a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) =
  do { a10' <- maybeM convert a10
     ; return $ I.ToplevelGlobal a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10' a11 a12 a13
     }

toplevel2Ir (A.ToplevelTypeDef lid t) = return $ I.ToplevelTypeDef lid t
toplevel2Ir (A.ToplevelDepLibs qs) = return $ I.ToplevelDepLibs qs
toplevel2Ir (A.ToplevelUnamedType i t) = return $ I.ToplevelUnamedType i t
toplevel2Ir (A.ToplevelModuleAsm q) = return $ I.ToplevelModuleAsm q
toplevel2Ir (A.ToplevelAttribute n l) = return $ I.ToplevelAttribute n l
toplevel2Ir (A.ToplevelComdat l s) = return $ I.ToplevelComdat l s

astToIr :: A.Module -> H.SimpleUniqueMonad (IdLabelMap, I.Module)
astToIr (A.Module ts) = runLabelMapM emptyIdLabelMap $ Md.liftM I.Module (mapM toplevel2Ir ts)


{- Ir to Ast conversion -}

instance Conversion I.LabelId (MyLabelMapM A.LabelId) where
  convert (I.LabelString l) = Md.liftM A.LabelString (labelIdFor l)
  convert (I.LabelDqString l) = Md.liftM A.LabelDqString (labelIdFor l)
  convert (I.LabelNumber l) = do { A.Lstring s <- labelIdFor l
                                 ; let n = read s :: Integer
                                 ; return $ A.LabelNumber n
                                 }
  convert (I.LabelDqNumber l) = do { A.Lstring s <- labelIdFor l
                                   ; let n = read s :: Integer
                                   ; return $ A.LabelDqNumber n
                                   }

instance Conversion I.PercentLabel (MyLabelMapM A.PercentLabel) where
  convert (I.PercentLabel l) = Md.liftM A.PercentLabel (convert l)

instance Conversion I.TargetLabel (MyLabelMapM A.TargetLabel) where
  convert (I.TargetLabel tl) = Md.liftM A.TargetLabel (convert tl)

instance Conversion I.BlockLabel (MyLabelMapM A.BlockLabel) where
  convert (I.BlockLabel b) = Md.liftM A.ExplicitBlockLabel (convert b)

{-
instance Conversion u v => Conversion (I.Typed u) (MyLabelMapM (A.Typed v)) where
  convert (I.TypedData t x) = Md.liftM (A.TypedData t) (convert x)
-}


instance Conversion (I.Typed I.Const) (MyLabelMapM (A.Typed A.Const)) where
  convert (I.TypedData t c) = Md.liftM (A.TypedData t) (convert c)
  convert I.UntypedNull = return A.UntypedNull

instance Conversion (I.Typed I.Value) (MyLabelMapM (A.Typed A.Value)) where
  convert (I.TypedData t v) = Md.liftM (A.TypedData t) (convert v)

instance Conversion (I.Typed I.Pointer) (MyLabelMapM (A.Typed A.Pointer)) where
  convert (I.TypedData t v) = Md.liftM (A.TypedData t) (convert v)

instance Conversion I.Pointer (MyLabelMapM A.Pointer) where
  convert (I.Pointer v) = Md.liftM A.Pointer (convert v)

instance Conversion I.ComplexConstant (MyLabelMapM A.ComplexConstant) where
  convert (I.Cstruct b fs) = Md.liftM (A.Cstruct b) (mapM convert fs)
  convert (I.Cvector fs) = Md.liftM A.Cvector (mapM convert fs)
  convert (I.Carray fs) = Md.liftM A.Carray (mapM convert fs)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.IbinExpr v1) (MyLabelMapM (A.IbinExpr v2)) where
    convert e = let (u1, u2) = I.operandOfIbinExpr e
                    t = I.typeOfIbinExpr e
                in
                  do { u1' <- convert u1
                     ; u2' <- convert u2
                     ; let f = case e of
                             I.Add nw _ _ _ -> A.IbinExpr A.Add (cnowrap nw)
                             I.Sub nw _ _ _ -> A.IbinExpr A.Sub (cnowrap nw)
                             I.Mul nw _ _ _ -> A.IbinExpr A.Mul (cnowrap nw)
                             I.Udiv nw _ _ _ -> A.IbinExpr A.Udiv (cexact nw)
                             I.Sdiv nw _ _ _ -> A.IbinExpr A.Sdiv (cexact nw)
                             I.Urem _ _ _ -> A.IbinExpr A.Urem []
                             I.Srem _ _ _ -> A.IbinExpr A.Srem []
                             I.Shl nw _ _ _ -> A.IbinExpr A.Shl (cnowrap nw)
                             I.Lshr nw _ _ _ -> A.IbinExpr A.Lshr (cexact nw)
                             I.Ashr nw _ _ _ -> A.IbinExpr A.Ashr (cexact nw)
                             I.And _ _ _ -> A.IbinExpr A.And []
                             I.Or _ _ _ -> A.IbinExpr A.Or []
                             I.Xor _ _ _ -> A.IbinExpr A.Xor []
                     ; return $ f t u1' u2'
                     }
                    where  cnowrap = maybe [] (\x -> case x of
                                                  I.Nsw -> [A.Nsw]
                                                  I.Nuw -> [A.Nuw]
                                                  I.Nsuw -> [A.Nsw, A.Nuw]
                                              )
                           cexact = maybe [] (\_ -> [A.Exact])


instance Conversion v1 (MyLabelMapM v2) => Conversion (I.FbinExpr v1) (MyLabelMapM (A.FbinExpr v2)) where
    convert e = let (u1, u2) = I.operandOfFbinExpr e
                    t = I.typeOfFbinExpr e
                in
                  do { u1' <- convert u1
                     ; u2' <- convert u2
                     ; let f = case e of
                             I.Fadd fg _ _ _ -> A.FbinExpr A.Fadd fg
                             I.Fsub fg _ _ _ -> A.FbinExpr A.Fsub fg
                             I.Fmul fg _ _ _ -> A.FbinExpr A.Fmul fg
                             I.Fdiv fg _ _ _ -> A.FbinExpr A.Fdiv fg
                             I.Frem fg _ _ _ -> A.FbinExpr A.Frem fg
                     ; return $ f t u1' u2'
                     }


instance Conversion v1 (MyLabelMapM v2) => Conversion (I.BinExpr v1) (MyLabelMapM (A.BinExpr v2)) where
  convert (I.Ie e) = Md.liftM A.Ie (convert e)
  convert (I.Fe e) = Md.liftM A.Fe (convert e)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.Conversion v1) (MyLabelMapM (A.Conversion v2)) where
  convert (I.Conversion op u t) = convert u >>= \u' -> return $ A.Conversion op u' t

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.GetElemPtr v1) (MyLabelMapM (A.GetElemPtr v2)) where
  convert (I.GetElemPtr b u us) = do { u' <- convert u
                                     ; us' <- mapM convert (fmap u1 us)
                                     ; return $ A.GetElemPtr b u' us'
                                     }
    where u1 x = x

instance (Conversion v1 (MyLabelMapM v2)) => Conversion (I.Select v1) (MyLabelMapM (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.Icmp v1) (MyLabelMapM (A.Icmp v2)) where
  convert (I.Icmp op t u1 u2) = Md.liftM2 (A.Icmp op t) (convert u1) (convert u2)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.Fcmp v1) (MyLabelMapM (A.Fcmp v2)) where
  convert (I.Fcmp op t u1 u2) = Md.liftM2 (A.Fcmp op t) (convert u1) (convert u2)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.ShuffleVector v1) (MyLabelMapM (A.ShuffleVector v2)) where
  convert (I.ShuffleVector u1 u2 u3) = Md.liftM3 A.ShuffleVector (convert u1) (convert u2) (convert u3)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.ExtractValue v1) (MyLabelMapM (A.ExtractValue v2)) where
  convert (I.ExtractValue u s) = convert u >>= \u' -> return $ A.ExtractValue u' s


instance Conversion v1 (MyLabelMapM v2) => Conversion (I.InsertValue v1) (MyLabelMapM (A.InsertValue v2)) where
  convert (I.InsertValue u1 u2 s) = do { u1' <- convert u1
                                       ; u2' <- convert u2
                                       ; return $ A.InsertValue u1' u2' s
                                       }

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.ExtractElem v1) (MyLabelMapM (A.ExtractElem v2)) where
  convert (I.ExtractElem u1 u2) = Md.liftM2 A.ExtractElem (convert u1) (convert u2)

instance Conversion v1 (MyLabelMapM v2) => Conversion (I.InsertElem v1) (MyLabelMapM (A.InsertElem v2)) where
  convert (I.InsertElem u1 u2 u3) = Md.liftM3 A.InsertElem (convert u1) (convert u2) (convert u3)

instance Conversion I.Const (MyLabelMapM A.Const) where
    convert x =
        case x of
          I.Ccp a -> return $ A.Ccp a
          I.Cca a -> Md.liftM A.Cca (convert a)
          I.CmL a -> return $ A.CmL a
          I.Cl a -> Md.liftM A.Cl (convert a)
          I.CblockAddress g a -> do { a' <- convert a
                                   ; return $ A.CblockAddress g a'
                                   }
          I.Cb a -> Md.liftM A.Cb (convert a)
          I.Cconv a -> Md.liftM A.Cconv (convert a)
          I.CgEp a -> Md.liftM A.CgEp (convert a)
          I.Cs a -> Md.liftM A.Cs (convert a)
          I.CiC a -> Md.liftM A.CiC (convert a)
          I.CfC a -> Md.liftM A.CfC (convert a)
          I.CsV a -> Md.liftM A.CsV (convert a)
          I.CeV a -> Md.liftM A.CeV (convert a)
          I.CiV a -> Md.liftM A.CiV (convert a)
          I.CeE a -> Md.liftM A.CeE (convert a)
          I.CiE a -> Md.liftM A.CiE (convert a)
          I.CmC a -> Md.liftM A.CmC (convert a)

instance Conversion I.MdVar (MyLabelMapM A.MdVar) where
    convert (I.MdVar s) = return $ A.MdVar s

instance Conversion I.MdNode (MyLabelMapM A.MdNode) where
    convert (I.MdNode s) = return $ A.MdNode s

instance Conversion I.MetaConst (MyLabelMapM A.MetaConst) where
    convert (I.MdConst c) = convert c >>= return . A.MdConst
    convert (I.MdString s) = return $ A.MdString s
    convert (I.McMn n) = convert n >>= return . A.McMn
    convert (I.McMv n) = convert n >>= return . A.McMv
    convert (I.MdRef i) = return $ A.MdRef i


instance Conversion I.Expr (MyLabelMapM A.Expr) where
    convert (I.EgEp c) = Md.liftM A.EgEp (convert c)
    convert (I.EiC a) = Md.liftM A.EiC (convert a)
    convert (I.EfC a) = Md.liftM A.EfC (convert a)
    convert (I.Eb a) = Md.liftM A.Eb (convert a)
    convert (I.Ec a) = Md.liftM A.Ec (convert a)
    convert (I.Es a) = Md.liftM A.Es (convert a)
    convert (I.Ev tv) = do { tv1@(A.TypedData t' _) <- convert tv
                           ; return $ A.Ec $ A.Conversion A.Bitcast tv1 t'
                           }




instance Conversion I.MemOp (MyLabelMapM A.MemOp) where
    convert (I.Allocate mar t mtv ma) = maybeM convert mtv >>= \x -> return $ A.Alloca mar t x ma
    convert (I.Load atom tv aa nonterm invr nonull) = convert tv >>= \tv' -> return $ A.Load atom tv' aa nonterm invr nonull
    convert (I.LoadAtomic atom v tv aa) = convert tv >>= \tv' -> return $ A.LoadAtomic atom v tv' aa
    convert (I.Store atom tv1 tv2 aa nonterm) = do { tv1' <- convert tv1
                                                   ; tv2' <- convert tv2
                                                   ; return $ A.Store atom tv1' tv2' aa nonterm
                                                   }
    convert (I.StoreAtomic atom v tv1 tv2 aa) = do { tv1' <- convert tv1
                                                   ; tv2' <- convert tv2
                                                   ; return $ A.StoreAtomic atom v tv1' tv2' aa
                                                   }
    convert (I.CmpXchg wk b1 tv1 tv2 tv3 b2 sord ford) = do { tv1' <- convert tv1
                                                            ; tv2' <- convert tv2
                                                            ; tv3' <- convert tv3
                                                            ; return $ A.CmpXchg wk b1 tv1' tv2' tv3' b2 sord ford
                                                            }
    convert (I.AtomicRmw b1 op tv1 tv2 b2 mf) = do { tv1' <- convert tv1
                                                   ; tv2' <- convert tv2
                                                   ; return $ A.AtomicRmw b1 op tv1' tv2' b2 mf
                                                   }
    convert (I.Fence b fo) = return $ A.Fence b fo



instance Conversion I.FunName (MyLabelMapM A.FunName) where
    convert (I.FunNameGlobal g) = return $ A.FunNameGlobal g
    convert (I.FunNameString s) = return $ A.FunNameString s

instance Conversion I.Value (MyLabelMapM A.Value) where
    convert (I.VgOl a) = return $ A.VgOl a
    convert (I.Ve a) = Md.liftM A.Ve (convert a)
    convert (I.Vc a) = Md.liftM A.Vc (convert a)
    convert (I.Deref _) = error "I.Deref should be removed in optimization"


instance Conversion I.CallSite (MyLabelMapM A.CallSite) where
    convert  (I.CsFun cc pa t fn aps fa) = do { fn' <- convert fn
                                              ; aps' <- mapM convert aps
                                              ; return $ A.CsFun cc pa t fn' aps' fa
                                              }
    convert (I.CsAsm t dia b1 b2 qs1 qs2 as fa) = do { as' <- mapM convert as
                                                     ; return $ A.CsAsm t dia b1 b2 qs1 qs2 as' fa
                                                     }
    convert (I.CsConversion pa t cv as fa) = do { cv' <- convert cv
                                                ; as' <- mapM convert as
                                                ; return $ A.CsConversion pa t cv' as' fa
                                                }

instance Conversion I.Clause (MyLabelMapM A.Clause) where
    convert (I.Catch tv) = convert tv >>= \tv' -> return $ A.Catch tv'
    convert (I.Filter tc) = convert tc >>= \tc' -> return $ A.Filter tc'
    convert (I.Cco tc) = convert tc >>= return . A.Cco

{-
instance Conversion (I.Type, I.GlobalOrLocalId) (MyLabelMapM (A.Type, A.GlobalOrLocalId)) where
    convert (t, g) = return (t, g)
-}

instance Conversion I.PersFn (MyLabelMapM A.PersFn) where
    convert (I.PersFnId s) = return $ A.PersFnId $ s
    convert (I.PersFnCast c) = convert c >>= return . A.PersFnCast
    convert (I.PersFnUndef) = return $ A.PersFnUndef
    convert (I.PersFnNull) = return $ A.PersFnNull
    convert (I.PersFnConst c) = Md.liftM A.PersFnConst (convert c)


instance Conversion I.Rhs (MyLabelMapM A.Rhs) where
    convert (I.RmO c) = convert c >>= return . A.RmO
    convert (I.Re e) = convert e >>= return . A.Re
    convert (I.Call b cs) = Md.liftM (A.Call b) (convert cs)
    convert (I.VaArg tv t) = do { tv' <- convert tv
                                ; return $ A.VaArg tv' t
                                }
    convert (I.LandingPad t1 t2 pf b cs) = do { pf' <- convert pf
                                              ; cs' <- mapM convert cs
                                              ; return $ A.LandingPad t1 t2 pf' b cs'
                                              }
    convert (I.ReE a) = Md.liftM A.ReE (convert a)
    convert (I.RiE a) = Md.liftM A.RiE (convert a)
    convert (I.RsV a) = Md.liftM A.RsV (convert a)
    convert (I.ReV a) = Md.liftM A.ReV (convert a)
    convert (I.RiV a) = Md.liftM A.RiV (convert a)

instance Conversion I.ActualParam (MyLabelMapM A.ActualParam) where
    convert (I.ActualParam t pa1 ma v pa2) = convert v >>= \v' -> return $ A.ActualParam t pa1 ma v' pa2

instance Conversion I.Aliasee (MyLabelMapM A.Aliasee) where
    convert (I.AtV tv) = Md.liftM A.AtV (convert tv)
    convert (I.Ac a) = Md.liftM A.Ac (convert a)
    convert (I.AgEp a) = Md.liftM A.AgEp (convert a)

{-
instance Conversion a (MyLabelMapM b) => Conversion (Maybe a) (MyLabelMapM (Maybe b)) where
  convert (Just a) = Md.liftM Just (convert a)
  convert Nothing = return Nothing
-}

instance Conversion I.Prefix (MyLabelMapM A.Prefix) where
  convert (I.Prefix n) = Md.liftM A.Prefix (convert n)

instance Conversion I.Prologue (MyLabelMapM A.Prologue) where
  convert (I.Prologue n) = Md.liftM A.Prologue (convert n)

instance Conversion I.FunctionPrototype (MyLabelMapM A.FunctionPrototype) where
    convert (I.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13 f14) =
      do { f13' <- convert f13
         ; f14' <- convert f14
         ; return $ A.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13' f14'
         }

instance Conversion I.PhiInst (MyLabelMapM A.PhiInst) where
  convert (I.PhiInst mg t branches) = Md.liftM (A.PhiInst mg t) (mapM (pairM convert convert) branches)

instance Conversion I.ComputingInst (MyLabelMapM A.ComputingInst) where
  convert (I.ComputingInst mg rhs) = Md.liftM (A.ComputingInst mg) (convert rhs)

instance Conversion I.TerminatorInst (MyLabelMapM A.TerminatorInst) where
  convert (I.Return tvs) = Md.liftM A.Return (mapM convert tvs)
  convert (I.Br t) = Md.liftM A.Br (convert t)
  convert (I.Cbr cnd t f) = Md.liftM3 A.Cbr (convert cnd) (convert t) (convert f)
  convert (I.IndirectBr cnd bs) = Md.liftM2 A.IndirectBr (convert cnd) (mapM convert bs)
  convert (I.Switch cnd d cases) = Md.liftM3 A.Switch (convert cnd) (convert d) (mapM (pairM convert convert) cases)
  convert (I.Invoke mg cs t f) = Md.liftM3 (A.Invoke mg) (convert cs) (convert t) (convert f)
  convert (I.Resume tv) = Md.liftM A.Resume (convert tv)
  convert I.Unreachable = return A.Unreachable
  convert I.Unwind = return A.Unwind

instance Conversion I.Dbg (MyLabelMapM A.Dbg) where
    convert (I.Dbg mv mc) = Md.liftM2 A.Dbg (convert mv) (convert mc)

instance Conversion I.PhiInstWithDbg (MyLabelMapM A.PhiInstWithDbg) where
  convert (I.PhiInstWithDbg ins dbgs) = Md.liftM2 A.PhiInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion I.ComputingInstWithDbg (MyLabelMapM A.ComputingInstWithDbg) where
    convert (I.ComputingInstWithDbg ins dbgs) = Md.liftM2 A.ComputingInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion I.TerminatorInstWithDbg (MyLabelMapM A.TerminatorInstWithDbg) where
    convert (I.TerminatorInstWithDbg term dbgs) = Md.liftM2 A.TerminatorInstWithDbg (convert term) (mapM convert dbgs)
    
    
type Pblock = (A.BlockLabel, [A.PhiInstWithDbg], [A.ComputingInstWithDbg])

getLabelId :: A.BlockLabel -> A.Lstring
getLabelId (A.ImplicitBlockLabel _) = error "ImplicitBlockLabel should be normalized"
getLabelId (A.ExplicitBlockLabel l) = A.labelIdToLstring l

convertNode :: I.Node e x -> MyLabelMapM (M.Map A.Lstring A.Block, Maybe Pblock)
               -> MyLabelMapM (M.Map A.Lstring A.Block, Maybe Pblock)
convertNode (I.Nlabel a) p = do { (bs, Nothing) <- p
                                ; a' <- convert a
                                ; return (bs, Just (a', [], []))
                                }
convertNode (I.Pinst a) p = do { (bs, Just (pb, phis, [])) <- p
                               ; a' <- convert a
                               ; return (bs, Just (pb, a':phis, []))
                               }
convertNode (I.Cinst a) p = do { (bs, Just (pb, phis, cs)) <- p
                               ; a' <- convert a
                               ; return (bs, Just (pb, phis, a':cs))
                               }
convertNode (I.Tinst a) p = do { (bs, pb) <- p
                               ; a' <- convert a
                               ; case pb of
                                 Nothing -> error "irrefutable"
                                 Just (l, phis, cs) ->
                                   return (M.insert (getLabelId l)
                                           (A.Block l (reverse phis) (reverse cs) a') bs,
                                           Nothing)
                               }

graphToBlocks :: H.Graph I.Node H.C H.C -> MyLabelMapM (M.Map A.Lstring A.Block)
graphToBlocks g = do { (bs, Nothing) <- H.foldGraphNodes convertNode g (return (M.empty, Nothing))
                     ; return bs
                     }

toplevel2Ast :: I.Toplevel -> MyLabelMapM A.Toplevel
toplevel2Ast (I.ToplevelTriple q) = return $ A.ToplevelTriple q
toplevel2Ast (I.ToplevelDataLayout q) = return $ A.ToplevelDataLayout q
toplevel2Ast (I.ToplevelAlias g v dll tlm na l a) = Md.liftM (A.ToplevelAlias g v dll tlm na l) (convert a)
toplevel2Ast (I.ToplevelDbgInit s i) = return $ A.ToplevelDbgInit s i
toplevel2Ast (I.ToplevelStandaloneMd s tv) = Md.liftM (A.ToplevelStandaloneMd s) (convert tv)
toplevel2Ast (I.ToplevelNamedMd m ns) = Md.liftM2 A.ToplevelNamedMd (convert m) (mapM convert ns)
toplevel2Ast (I.ToplevelDeclare f) = Md.liftM A.ToplevelDeclare (convert f)

toplevel2Ast (I.ToplevelDefine f _ g) =
  do { bm <- graphToBlocks g
     ; f' <- convert f
     ; bs' <- getAlist f'
     ; let bs'' = reverse (foldl (\a b -> maybe a (\e -> e:a) (M.lookup b bm)) [] bs')
     ; return $ A.ToplevelDefine f' bs''
     }

toplevel2Ast (I.ToplevelGlobal a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) =
  do { a10' <- maybeM convert a10
     ; return $ A.ToplevelGlobal a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10' a11 a12 a13
     }

toplevel2Ast (I.ToplevelTypeDef lid t) = return $ A.ToplevelTypeDef lid t
toplevel2Ast (I.ToplevelDepLibs qs) = return $ A.ToplevelDepLibs qs
toplevel2Ast (I.ToplevelUnamedType i t) = return $ A.ToplevelUnamedType i t
toplevel2Ast (I.ToplevelModuleAsm q) = return $ A.ToplevelModuleAsm q
toplevel2Ast (I.ToplevelComdat l s) = return $ A.ToplevelComdat l s
toplevel2Ast (I.ToplevelAttribute n c) = return $ A.ToplevelAttribute n c
-- toplevel2Ast x = error $ "unhandled toplevel " ++ show x

irToAst :: IdLabelMap -> I.Module -> H.SimpleUniqueMonad A.Module
irToAst iLm (I.Module ts) = Md.liftM snd (runLabelMapM iLm $ Md.liftM A.Module (mapM toplevel2Ast ts))

