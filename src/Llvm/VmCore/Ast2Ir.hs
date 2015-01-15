{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Llvm.VmCore.Ast2Ir where
import qualified Compiler.Hoopl as H
import qualified Control.Monad as Md
import qualified Data.Map as M
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
import Llvm.VmCore.Converter 
import Llvm.VmCore.LabelMapM 

type MyLabelMapM = LabelMapM H.SimpleUniqueMonad

-- the real differences between Ast and Ir 
-- 1. Ir uses Unique values as labels while Ast can use any strings as labels
-- 2. All unreachable code are removed in Ir
instance Converter (A.LabelId) (MyLabelMapM I.LabelId) where
  convert l@(A.LabelString _) = Md.liftM I.LabelString (labelFor $ A.labelIdToLstring l)
  convert l@(A.LabelDqString _) = Md.liftM I.LabelDqString (labelFor $ A.labelIdToLstring l)
  convert l@(A.LabelNumber _) = Md.liftM I.LabelNumber (labelFor $ A.labelIdToLstring l)
  convert l@(A.LabelQuoteNumber _) = Md.liftM I.LabelQuoteNumber (labelFor $ A.labelIdToLstring l)

instance Converter A.PercentLabel (MyLabelMapM I.PercentLabel) where
    convert (A.PercentLabel l) = convert l >>= return . I.PercentLabel
                               
instance Converter A.TargetLabel (MyLabelMapM I.TargetLabel) where
    convert (A.TargetLabel tl) = convert tl >>= return . I.TargetLabel

instance Converter A.BlockLabel (MyLabelMapM I.BlockLabel) where
    convert (A.ImplicitBlockLabel p) = error $ "ImplicitBlockLabel @" ++ show p ++ " should be normalized in AstSimplify, and should not be leaked to Ast2Ir."
    convert (A.ExplicitBlockLabel b) = convert b >>= return . I.BlockLabel
  
instance Converter A.TypedConst (MyLabelMapM I.TypedConst) where
    convert (A.TypedConst t c) = convert c >>= return . (I.TypedConst t)
    convert A.TypedConstNull = return I.TypedConstNull

instance Converter A.TypedValue (MyLabelMapM I.TypedValue) where
    convert (A.TypedValue t v) = convert v >>= return . (I.TypedValue t)

instance Converter A.TypedPointer (MyLabelMapM I.TypedPointer) where
    convert (A.TypedPointer t v) = convert v >>= return . (I.TypedPointer t)

instance Converter A.ComplexConstant (MyLabelMapM I.ComplexConstant) where
    convert (A.Cstruct b fs) = Md.liftM (I.Cstruct b) (mapM convert fs)
    convert (A.Cvector fs) = mapM convert fs >>= return . I.Cvector
    convert (A.Carray fs) = mapM convert fs >>= return . I.Carray

instance Converter v1 (MyLabelMapM v2) => Converter (A.IbinExpr v1) (MyLabelMapM (I.IbinExpr v2)) where   
    convert (A.IbinExpr op cs t u1 u2) = do { u1' <- convert u1
                                            ; u2' <- convert u2
                                            ; return $ (convertIop op cs) t u1' u2'
                                            }
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

instance Converter v1 (MyLabelMapM v2) => Converter (A.FbinExpr v1) (MyLabelMapM (I.FbinExpr v2)) where   
    convert (A.FbinExpr op cs t u1 u2) = do { u1' <- convert u1
                                            ; u2' <- convert u2
                                            ; return $ (convertFop op) cs t u1' u2'
                                            }
      where 
        convertFop op = case op of
          A.Fadd -> I.Fadd 
          A.Fsub -> I.Fsub
          A.Fmul -> I.Fmul
          A.Fdiv -> I.Fdiv
          A.Frem -> I.Frem

instance Converter v1 (MyLabelMapM v2) => Converter (A.BinExpr v1) (MyLabelMapM (I.BinExpr v2)) where   
    convert (A.Ie e) = Md.liftM I.Ie (convert e)
    convert (A.Fe e) = Md.liftM I.Fe (convert e)
                                                           

instance Converter v1 (MyLabelMapM v2) => Converter (A.Conversion v1) (MyLabelMapM (I.Conversion v2)) where    
    convert (A.Conversion op u t) = convert u >>= \u' -> return $ I.Conversion op u' t
                                                  

instance Converter v1 (MyLabelMapM v2) => Converter (A.GetElemPtr v1) (MyLabelMapM (I.GetElemPtr v2)) where
    convert (A.GetElemPtr b u us) = do { u' <- convert u
                                       ; us' <- mapM convert us
                                       ; return $ I.GetElemPtr b u' us'
                                       }
                                
instance (Converter v1 (MyLabelMapM v2)) => Converter (A.Select v1) (MyLabelMapM (I.Select v2)) where
    convert (A.Select u1 u2 u3) = do { u1' <- convert u1
                                     ; u2' <- convert u2
                                     ; u3' <- convert u3
                                     ; return $ I.Select u1' u2' u3'
                                     } 
                              
instance Converter v1 (MyLabelMapM v2) => Converter (A.Icmp v1) (MyLabelMapM (I.Icmp v2)) where
    convert (A.Icmp op t u1 u2) = do { u1' <- convert u1
                                     ; u2' <- convert u2
                                     ; return $ I.Icmp op t u1' u2'
                                     } 
                              
instance Converter v1 (MyLabelMapM v2) => Converter (A.Fcmp v1) (MyLabelMapM (I.Fcmp v2)) where
    convert (A.Fcmp op t u1 u2) = do { u1' <- convert u1
                                     ; u2' <- convert u2
                                     ; return $ I.Fcmp op t u1' u2'
                                     }
                              
instance Converter v1 (MyLabelMapM v2) => Converter (A.ShuffleVector v1) (MyLabelMapM (I.ShuffleVector v2)) where 
    convert (A.ShuffleVector u1 u2 u3) = do { u1' <- convert u1
                                            ; u2' <- convert u2
                                            ; u3' <- convert u3
                                            ; return $ I.ShuffleVector u1' u2' u3'
                                            }
                                     
instance Converter v1 (MyLabelMapM v2) => Converter (A.ExtractValue v1) (MyLabelMapM (I.ExtractValue v2)) where
    convert (A.ExtractValue u s) = convert u >>= \u' -> return $ I.ExtractValue u' s
  

instance Converter v1 (MyLabelMapM v2) => Converter (A.InsertValue v1) (MyLabelMapM (I.InsertValue v2)) where  
    convert (A.InsertValue u1 u2 s) = do { u1' <- convert u1
                                         ; u2' <- convert u2
                                         ; return $ I.InsertValue u1' u2' s
                                         }
                                  
instance Converter v1 (MyLabelMapM v2) => Converter (A.ExtractElem v1) (MyLabelMapM (I.ExtractElem v2)) where
    convert (A.ExtractElem u1 u2) = do { u1' <- convert u1
                                       ; u2' <- convert u2
                                       ; return $ I.ExtractElem u1' u2'
                                       }
                                

instance Converter v1 (MyLabelMapM v2) => Converter (A.InsertElem v1) (MyLabelMapM (I.InsertElem v2)) where 
    convert (A.InsertElem u1 u2 u3) = do { u1' <- convert u1
                                         ; u2' <- convert u2
                                         ; u3' <- convert u3
                                         ; return $ I.InsertElem u1' u2' u3'
                                         }
                                

instance Converter A.Const (MyLabelMapM I.Const) where
    convert x = 
        case x of
          A.Ccp a -> return $ I.Ccp a
          A.Cca a -> Md.liftM I.Cca (convert a)
          A.CmL a -> return $ I.CmL a
          A.Cl a -> Md.liftM I.Cl (convert a)
          A.CblockAddress g a -> do { a' <- convert a
                                   ; return $ I.CblockAddress g a'
                                   }
--          A.Ca a -> Md.liftM I.Ca (convert a)
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

instance Converter A.MdVar (MyLabelMapM I.MdVar) where
    convert (A.MdVar s) = return $ I.MdVar s

instance Converter A.MdNode (MyLabelMapM I.MdNode) where
    convert (A.MdNode s) = return $ I.MdNode s

instance Converter A.MetaConst (MyLabelMapM I.MetaConst) where
    convert (A.MdConst c) = convert c >>= return . I.MdConst
    convert (A.MdString s) = return $ I.MdString s
    convert (A.McMn n) = convert n >>= return . I.McMn
    convert (A.McMv n) = convert n >>= return . I.McMv
    convert (A.MdRef i) = return $ I.MdRef i


instance Converter A.Expr (MyLabelMapM I.Expr) where
    convert (A.EgEp c) = Md.liftM I.EgEp (convert c)
--    convert (A.Ea a) =  Md.liftM I.Ea (convert a)
    convert (A.EiC a) = Md.liftM I.EiC (convert a)
    convert (A.EfC a) = Md.liftM I.EfC (convert a)
    convert (A.Eb a) = Md.liftM I.Eb (convert a)
    convert (A.Ec a) = Md.liftM I.Ec (convert a)
    convert (A.Es a) = Md.liftM I.Es (convert a)



instance Converter A.MemOp (MyLabelMapM I.MemOp) where
    convert (A.Alloca mar t mtv ma) = maybeM convert mtv >>= \x -> return $ I.Allocate mar t x ma
--    convert (A.Free tv) = convert tv >>= \tv' -> return $ I.Free tv'
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



instance Converter A.FunName (MyLabelMapM I.FunName) where
    convert (A.FunNameGlobal g) = return $ I.FunNameGlobal g
    convert (A.FunNameString s) = return $ I.FunNameString s

instance Converter A.Value (MyLabelMapM I.Value) where
    convert (A.VgOl a) = return $ I.VgOl a
    convert (A.Ve a) = Md.liftM I.Ve (convert a)
    convert (A.Vc a) = Md.liftM I.Vc (convert a)

instance Converter A.Pointer (MyLabelMapM I.Pointer) where
    convert (A.Pointer a) = convert a >>= return . I.Pointer
      
   
instance Converter A.CallSite (MyLabelMapM I.CallSite) where
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
                                        
instance Converter A.Clause (MyLabelMapM I.Clause) where
    convert (A.Catch tv) = convert tv >>= \tv' -> return $ I.Catch tv'
    convert (A.Filter tc) = convert tc >>= \tc' -> return $ I.Filter tc'
    convert (A.Cco tc) = convert tc >>= return . I.Cco
  

instance Converter (A.Type, A.GlobalOrLocalId) (MyLabelMapM (I.Type, I.GlobalOrLocalId)) where
    convert (t, g) = return (t, g)

instance Converter A.PersFn (MyLabelMapM I.PersFn) where
    convert (A.PersFnId s) = return $ I.PersFnId s
    convert (A.PersFnCast c) = convert c >>= return . I.PersFnCast 
    convert (A.PersFnUndef) = return $ I.PersFnUndef
    convert (A.PersFnNull) = return $ I.PersFnNull
    convert (A.PersFnConst c) = Md.liftM I.PersFnConst (convert c)


instance Converter A.Rhs (MyLabelMapM I.Rhs) where
    convert (A.RmO c) = convert c >>= return . I.RmO
    convert (A.Re e) = convert e >>= return . I.Re
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
    

instance Converter A.ActualParam (MyLabelMapM I.ActualParam) where
    convert (A.ActualParam t pa1 ma v pa2) = convert v >>= \v' -> return $ I.ActualParam t pa1 ma v' pa2
                    
instance Converter A.Aliasee (MyLabelMapM I.Aliasee) where
    convert (A.AtV tv) = convert tv >>= return . I.AtV 
    convert (A.Ac a) = convert a >>= \a' -> return $ I.Ac a'
    convert (A.AgEp a) = Md.liftM I.AgEp (convert a)

instance Converter A.Prefix (MyLabelMapM I.Prefix) where
  convert (A.Prefix n) = Md.liftM I.Prefix (convert n)
  
instance Converter A.Prologue (MyLabelMapM I.Prologue) where  
  convert (A.Prologue n) = Md.liftM I.Prologue (convert n)

instance Converter a (MyLabelMapM b) => Converter (Maybe a) (MyLabelMapM (Maybe b)) where
  convert (Just x) = Md.liftM Just (convert x)
  convert Nothing = return Nothing

instance Converter A.FunctionPrototype (MyLabelMapM I.FunctionPrototype) where
    convert (A.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13 f14) = 
      do { f13' <- convert f13
         ; f14' <- convert f14
         ; return $ I.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13' f14'
         }

instance Converter A.PhiInst (MyLabelMapM I.PhiInst) where
    convert (A.PhiInst mg t branches) = do { branches' <- mapM (pairM convert convert) branches
                                           ; return $ I.PhiInst mg t 
                                               (fmap (\x -> (fst x, snd x)) branches')
                                           }

instance Converter A.ComputingInst (MyLabelMapM I.ComputingInst) where
    convert (A.ComputingInst mg rhs) = do { rhs' <- convert rhs
                                          ; return $ I.ComputingInst mg rhs'
                                          }

instance Converter A.TerminatorInst (MyLabelMapM I.TerminatorInst) where
    convert (A.Return tvs) = (mapM convert tvs) >>= return . I.Return 
    convert (A.Br t) = Md.liftM I.Br (convert t)
    convert (A.Cbr cnd t f) = do { cnd' <- convert cnd
                                 ; t' <- convert t
                                 ; f' <- convert f
                                 ; return $ I.Cbr cnd' t' f'
                                 } 
    convert (A.IndirectBr cnd bs) = do { cnd' <- convert cnd
                                       ; bs' <- mapM convert bs
                                       ; return $ I.IndirectBr cnd' bs'
                                       } 
    convert (A.Switch cnd d cases) = do { cnd' <- convert cnd
                                        ; d' <- convert d
                                        ; cases' <- mapM (pairM convert convert) cases  
                                        ; return $ I.Switch cnd' d' (fmap (\x -> (fst x, snd x)) cases')
                                        } 
    convert (A.Invoke mg cs t f) = do { cs' <- convert cs
                                      ; t' <- convert t
                                      ; f' <- convert f
                                      ; return $ I.Invoke mg cs' t' f'
                                      }
    convert (A.Resume tv) = convert tv >>= \tv' -> return $ I.Resume tv'
    convert A.Unreachable = return I.Unreachable
    convert A.Unwind = return I.Unwind


instance Converter A.Dbg (MyLabelMapM I.Dbg) where
    convert (A.Dbg mv mc) = do { mv' <- convert mv
                               ; mc' <- convert mc
                               ; return $ I.Dbg mv' mc'
                               }

instance Converter A.ComputingInstWithDbg (MyLabelMapM I.ComputingInstWithDbg) where
    convert (A.ComputingInstWithDbg ins dbgs) = do { ins' <- convert ins
                                                   ; dbgs' <- mapM convert dbgs
                                                   ; return $ I.ComputingInstWithDbg ins' dbgs'
                                                   } 

instance Converter A.TerminatorInstWithDbg (MyLabelMapM I.TerminatorInstWithDbg) where
    convert (A.TerminatorInstWithDbg term dbgs) = do { term' <- convert term
                                                     ; dbgs' <- mapM convert dbgs
                                                     ; return $ I.TerminatorInstWithDbg term' dbgs'
                                                     } 


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
toFirst x = convert x >>= return . I.Nlabel

toPhi :: A.PhiInst -> MyLabelMapM (I.Node H.O H.O)
toPhi phi = convert phi >>= return . I.Pinst
  
toMid :: A.ComputingInstWithDbg -> MyLabelMapM (I.Node H.O H.O)
toMid inst = convert inst >>= return . I.Cinst 

toLast :: A.TerminatorInstWithDbg -> MyLabelMapM (I.Node H.O H.C)
toLast inst = convert inst >>= return . I.Tinst  

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
