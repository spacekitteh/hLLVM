{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Llvm.VmCore.Ir2Ast where
import qualified Compiler.Hoopl as H
import qualified Control.Monad as Md
import qualified Data.Map as M
import Llvm.VmCore.CoreIr
import qualified Llvm.VmCore.Ast as A
import qualified Llvm.VmCore.Ir as I
import Llvm.VmCore.Converter 
import Llvm.VmCore.LabelMap

instance Converter I.LabelId (LabelMapM A.LabelId) where
  convert (I.LabelString l) = Md.liftM A.LabelString (labelIdFor l)
  convert (I.LabelQuoteString l) = Md.liftM A.LabelQuoteString (labelIdFor l)
  convert (I.LabelNumber l) = do { A.Lstring s <- labelIdFor l
                                 ; let n = read s :: Integer
                                 ; return $ A.LabelNumber n
                                 }
  convert (I.LabelQuoteNumber l) = do { A.Lstring s <- labelIdFor l
                                      ; let n = read s :: Integer
                                      ; return $ A.LabelQuoteNumber n
                                      }
  


instance Converter I.PercentLabel (LabelMapM A.PercentLabel) where
    convert (I.PercentLabel l) = convert l >>= return . A.PercentLabel
                               
instance Converter I.TargetLabel (LabelMapM A.TargetLabel) where
    convert (I.TargetLabel tl) = convert tl >>= return . A.TargetLabel


instance Converter I.BlockLabel (LabelMapM A.BlockLabel) where
    convert (I.BlockLabel b) = convert b >>= return . A.ExplicitBlockLabel
  
instance Converter I.TypedConst (LabelMapM A.TypedConst) where
    convert (I.TypedConst t c) = convert c >>= return . (A.TypedConst t)
    convert I.TypedConstNull = return A.TypedConstNull


instance Converter I.TypedValue (LabelMapM A.TypedValue) where
    convert (I.TypedValue t v) = convert v >>= return . (A.TypedValue t)

instance Converter I.TypedPointer (LabelMapM A.TypedPointer) where
    convert (I.TypedPointer t v) = convert v >>= return . (A.TypedPointer t)

instance Converter I.Pointer (LabelMapM A.Pointer) where
    convert (I.Pointer v) = convert v >>= return . A.Pointer 


instance Converter I.ComplexConstant (LabelMapM A.ComplexConstant) where
    convert (I.Cstruct b fs) = Md.liftM (A.Cstruct b) (mapM convert fs)
    convert (I.Cvector fs) = mapM convert fs >>= return . A.Cvector
    convert (I.Carray fs) = mapM convert fs >>= return . A.Carray
    



instance Converter v1 (LabelMapM v2) => Converter (I.BinExpr v1) (LabelMapM (A.BinExpr v2)) where   
    convert e = let (u1, u2) = operandOfBinExpr e
                    t = typeOfBinExpr e
                in 
                  do { u1' <- convert u1
                     ; u2' <- convert u2
                     ; let f = case e of 
                                 I.Add nw _ _ _ -> A.BinExpr A.Add (cnowrap nw) 
                                 I.Sub nw _ _ _ -> A.BinExpr A.Sub (cnowrap nw) 
                                 I.Mul nw _ _ _ -> A.BinExpr A.Mul (cnowrap nw) 
                                 I.Udiv nw _ _ _ -> A.BinExpr A.Udiv (cexact nw)
                                 I.Sdiv nw _ _ _ -> A.BinExpr A.Sdiv (cexact nw)
                                 I.Urem _ _ _ -> A.BinExpr A.Urem []
                                 I.Srem _ _ _ -> A.BinExpr A.Srem []
                                 I.Fadd _ _ _ -> A.BinExpr A.Fadd []
                                 I.Fsub _ _ _ -> A.BinExpr A.Fsub []
                                 I.Fmul _ _ _ -> A.BinExpr A.Fmul []
                                 I.Fdiv _ _ _ -> A.BinExpr A.Fdiv []
                                 I.Frem _ _ _ -> A.BinExpr A.Frem []
                                 I.Shl nw _ _ _ -> A.BinExpr A.Shl (cnowrap nw) 
                                 I.Lshr nw _ _ _ -> A.BinExpr A.Lshr (cexact nw)
                                 I.Ashr nw _ _ _ -> A.BinExpr A.Ashr (cexact nw)
                                 I.And _ _ _ -> A.BinExpr A.And []
                                 I.Or _ _ _ -> A.BinExpr A.Or []
                                 I.Xor _ _ _ -> A.BinExpr A.Xor []
                     ; return $ f t u1' u2'
                     }
                    where  cnowrap = maybe [] (\x -> case x of 
                                                      I.Nsw -> [A.Nsw]
                                                      I.Nuw -> [A.Nuw]
                                                      I.Nsuw -> [A.Nsw, A.Nuw]
                                              ) 
                           cexact = maybe [] (\x -> [A.Exact])
                                   
                                             

{-
instance Converter v1 (LabelMapM v2) => Converter (I.Bitwise v1) (LabelMapM (A.Bitwise v2)) where
    convert (I.Bitwise op cs t u1 u2) = do { u1' <- convert u1
                                                         ; u2' <- convert u2
                                                         ; return $ A.Bitwise op cs t u1' u2'
                                                         }
-}
                                    
instance Converter v1 (LabelMapM v2) => Converter (I.Conversion v1) (LabelMapM (A.Conversion v2)) where    
    convert (I.Conversion op u t) = convert u >>= \u' -> return $ A.Conversion op u' t
                                                  

instance Converter v1 (LabelMapM v2) => Converter (I.GetElemPtr v1) (LabelMapM (A.GetElemPtr v2)) where
    convert (I.GetElemPtr b u us) = do { u' <- convert u
                                                     ; us' <- mapM convert (fmap u1 us)
                                                     ; return $ A.GetElemPtr b u' us'
                                                     }
      where u1 x = x
                                
instance (Converter v1 (LabelMapM v2)) => Converter (I.Select v1) (LabelMapM (A.Select v2)) where
    convert (I.Select u1 u2 u3) = do { u1' <- convert u1
                                                          ; u2' <- convert u2
                                                          ; u3' <- convert u3
                                                          ; return $ A.Select u1' u2' u3'
                                                          } 
                              
instance Converter v1 (LabelMapM v2) => Converter (I.Icmp v1) (LabelMapM (A.Icmp v2)) where
    convert (I.Icmp op t u1 u2) = do { u1' <- convert u1
                                                   ; u2' <- convert u2
                                                   ; return $ A.Icmp op t u1' u2'
                                                   } 
                              
instance Converter v1 (LabelMapM v2) => Converter (I.Fcmp v1) (LabelMapM (A.Fcmp v2)) where
    convert (I.Fcmp op t u1 u2) = do { u1' <- convert u1
                                                   ; u2' <- convert u2
                                                   ; return $ A.Fcmp op t u1' u2'
                                                   }
                              
instance Converter v1 (LabelMapM v2) => Converter (I.ShuffleVector v1) (LabelMapM (A.ShuffleVector v2)) where 
    convert (I.ShuffleVector u1 u2 u3) = do { u1' <- convert u1
                                                                 ; u2' <- convert u2
                                                                 ; u3' <- convert u3
                                                                 ; return $ A.ShuffleVector u1' u2' u3'
                                                                 }
                                     
instance Converter v1 (LabelMapM v2) => Converter (I.ExtractValue v1) (LabelMapM (A.ExtractValue v2)) where
    convert (I.ExtractValue u s) = convert u >>= \u' -> return $ A.ExtractValue u' s
  

instance Converter v1 (LabelMapM v2) => Converter (I.InsertValue v1) (LabelMapM (A.InsertValue v2)) where  
    convert (I.InsertValue u1 u2 s) = do { u1' <- convert u1
                                                       ; u2' <- convert u2
                                                       ; return $ A.InsertValue u1' u2' s
                                                       }
                                  
instance Converter v1 (LabelMapM v2) => Converter (I.ExtractElem v1) (LabelMapM (A.ExtractElem v2)) where
    convert (I.ExtractElem u1 u2) = do { u1' <- convert u1
                                                     ; u2' <- convert u2
                                                     ; return $ A.ExtractElem u1' u2'
                                                     }
                                

instance Converter v1 (LabelMapM v2) => Converter (I.InsertElem v1) (LabelMapM (A.InsertElem v2)) where 
    convert (I.InsertElem u1 u2 u3) = do { u1' <- convert u1
                                                              ; u2' <- convert u2
                                                              ; u3' <- convert u3
                                                              ; return $ A.InsertElem u1' u2' u3'
                                                              }

instance Converter I.Const (LabelMapM A.Const) where
    convert x = 
        case x of
          I.Ccp a -> return $ A.Ccp a
          I.Cca a -> Md.liftM A.Cca (convert a)
          I.CmL a -> return $ A.CmL a
          I.Cl a -> Md.liftM A.Cl (convert a)
          I.CblockAddress g a -> do { a' <- convert a
                                   ; return $ A.CblockAddress g a'
                                   }
--          I.Ca a -> Md.liftM A.Ca (convert a)
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

instance Converter I.MdVar (LabelMapM A.MdVar) where
    convert (I.MdVar s) = return $ A.MdVar s

instance Converter I.MdNode (LabelMapM A.MdNode) where
    convert (I.MdNode s) = return $ A.MdNode s

instance Converter I.MetaConst (LabelMapM A.MetaConst) where
    convert (I.MdConst c) = convert c >>= return . A.MdConst
    convert (I.MdString s) = return $ A.MdString s
    convert (I.McMn n) = convert n >>= return . A.McMn
    convert (I.McMv n) = convert n >>= return . A.McMv
    convert (I.MdRef i) = return $ A.MdRef i


instance Converter I.Expr (LabelMapM A.Expr) where
    convert (I.EgEp c) = Md.liftM A.EgEp (convert c)
--    convert (I.Ea a) =  Md.liftM A.Ea (convert a)
    convert (I.EiC a) = Md.liftM A.EiC (convert a)
    convert (I.EfC a) = Md.liftM A.EfC (convert a)
    convert (I.Eb a) = Md.liftM A.Eb (convert a)
    convert (I.Ec a) = Md.liftM A.Ec (convert a)
    convert (I.Es a) = Md.liftM A.Es (convert a)
    convert (I.Ev tv) = do { (A.TypedValue t' v') <- convert tv
                           ; return $ A.Ec $ A.Conversion A.Bitcast (A.TypedValue t' v') t'
                           }
                                                                            
                       


instance Converter I.MemOp (LabelMapM A.MemOp) where
    convert (I.Allocate mar t mtv ma) = maybeM convert mtv >>= \x -> return $ A.Allocate mar t x ma
    convert (I.Free tv) = convert tv >>= return . A.Free
    convert (I.Load atom tv aa) = convert tv >>= \tv' -> return $ A.Load atom tv' aa
    convert (I.Store atom tv1 tv2 aa) = do { tv1' <- convert tv1
                                           ; tv2' <- convert tv2
                                           ; return $ A.Store atom tv1' tv2' aa
                                           }
    convert (I.CmpXchg b1 tv1 tv2 tv3 b2 mf) = do { tv1' <- convert tv1
                                                  ; tv2' <- convert tv2
                                                  ; tv3' <- convert tv3
                                                  ; return $ A.CmpXchg b1 tv1' tv2' tv3' b2 mf
                                                  }
    convert (I.AtomicRmw b1 op tv1 tv2 b2 mf) = do { tv1' <- convert tv1
                                                   ; tv2' <- convert tv2
                                                   ; return $ A.AtomicRmw b1 op tv1' tv2' b2 mf
                                                   }
    convert (I.Fence b fo) = return $ A.Fence b fo



instance Converter I.FunName (LabelMapM A.FunName) where
    convert (I.FunNameGlobal g) = return $ A.FunNameGlobal g
    convert (I.FunNameString s) = return $ A.FunNameString s

instance Converter I.Value (LabelMapM A.Value) where
    convert (I.VgOl a) = return $ A.VgOl a
    convert (I.Ve a) = Md.liftM A.Ve (convert a)
    convert (I.Vc a) = Md.liftM A.Vc (convert a)
    convert (I.InlineAsm a1 a2 a3 a4) = return $ A.InlineAsm a1 a2 a3 a4
      
   
instance Converter I.CallSite (LabelMapM A.CallSite) where
    convert  (I.CallFun cc pa t fn aps fa) = do { fn' <- convert fn
                                                ; aps' <- mapM convert aps
                                                ; return $ A.CallFun cc pa t fn' aps' fa
                                                } 
    convert (I.CallAsm t b1 b2 qs1 qs2 as fa) = do { as' <- mapM convert as
                                                   ; return $ A.CallAsm t b1 b2 qs1 qs2 as' fa
                                                   }
    convert (I.CallConversion pa t cv as fa) = do { cv' <- convert cv
                                               ; as' <- mapM convert as
                                               ; return $ A.CallConversion pa t cv' as' fa
                                               } 
                                        
instance Converter I.Clause (LabelMapM A.Clause) where
    convert (I.Catch tv) = convert tv >>= \tv' -> return $ A.Catch tv'
    convert (I.Filter tc) = convert tc >>= \tc' -> return $ A.Filter tc'
    convert (I.Cco tc) = convert tc >>= return . A.Cco

instance Converter (I.Type, I.GlobalOrLocalId) (LabelMapM (A.Type, A.GlobalOrLocalId)) where
    convert (t, g) = return (t, g)

instance Converter I.PersFn (LabelMapM A.PersFn) where
    convert (I.PersFnId s) = return $ A.PersFnId $ s
    convert (I.PersFnCast c) = convert c >>= return . A.PersFnCast 
    convert (I.PersFnUndef) = return $ A.PersFnUndef


instance Converter I.Rhs (LabelMapM A.Rhs) where
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
    

instance Converter I.ActualParam (LabelMapM A.ActualParam) where
    convert (I.ActualParam t pa1 ma v pa2) = convert v >>= \v' -> return $ A.ActualParam t pa1 ma v' pa2
                    
instance Converter I.Aliasee (LabelMapM A.Aliasee) where
    convert (I.AtV tv) = convert tv >>= return . A.AtV 
    convert (I.Ac a) = convert a >>= \a' -> return $ A.Ac a'
    convert (I.AgEp a) = Md.liftM A.AgEp (convert a)


instance Converter I.FunctionPrototype (LabelMapM A.FunctionPrototype) where
    convert (I.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11) = 
        return $ A.FunctionPrototype f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11


instance Converter I.PhiInst (LabelMapM A.PhiInst) where
    convert (I.PhiInst mg t branches) = do { branches' <- mapM (pairM convert convert) branches
                                           ; return $ A.PhiInst mg t branches'
                                           }

instance Converter I.ComputingInst (LabelMapM A.ComputingInst) where
    convert (I.ComputingInst mg rhs) = do { rhs' <- convert rhs
                                          ; return $ A.ComputingInst mg rhs'
                                          }

instance Converter I.TerminatorInst (LabelMapM A.TerminatorInst) where
    convert (I.Return tvs) = (mapM convert tvs) >>= return . A.Return
    convert (I.Br t) = Md.liftM A.Br (convert t)
    convert (I.Cbr cnd t f) = do { cnd' <- convert cnd
                                        ; t' <- convert t
                                        ; f' <- convert f
                                        ; return $ A.Cbr cnd' t' f'
                                        } 
    convert (I.IndirectBr cnd bs) = do { cnd' <- convert cnd
                                              ; bs' <- mapM convert bs
                                              ; return $ A.IndirectBr cnd' bs'
                                              } 
    convert (I.Switch cnd d cases) = do { cnd' <- convert cnd
                                               ; d' <- convert d
                                               ; cases' <- mapM (pairM convert convert) cases
                                               ; return $ A.Switch cnd' d' cases'
                                               } 
    convert (I.Invoke mg cs t f) = do { cs' <- convert cs
                                      ; t' <- convert t
                                      ; f' <- convert f
                                      ; return $ A.Invoke mg cs' t' f'
                                      }
    convert (I.Resume tv) = convert tv >>= return . A.Resume
    convert I.Unreachable = return A.Unreachable


instance Converter I.Dbg (LabelMapM A.Dbg) where
    convert (I.Dbg mv mc) = do { mv' <- convert mv
                               ; mc' <- convert mc
                               ; return $ A.Dbg mv' mc'
                               }

instance Converter I.ComputingInstWithDbg (LabelMapM A.ComputingInstWithDbg) where
    convert (I.ComputingInstWithDbg ins dbgs) = do { ins' <- convert ins
                                                   ; dbgs' <- mapM convert dbgs
                                                   ; return $ A.ComputingInstWithDbg ins' dbgs'
                                                   } 

instance Converter I.TerminatorInstWithDbg (LabelMapM A.TerminatorInstWithDbg) where
    convert (I.TerminatorInstWithDbg term dbgs) = do { term' <- convert term
                                                     ; dbgs' <- mapM convert dbgs
                                                     ; return $ A.TerminatorInstWithDbg term' dbgs'
                                                     } 

type Pblock = (A.BlockLabel, [A.PhiInst], [A.ComputingInstWithDbg])

getLabelId :: A.BlockLabel -> A.Lstring -- abelId
-- getLabelId (A.ImplicitBlockLabel l) = A.labelIdToString l
getLabelId (A.ExplicitBlockLabel l) = A.labelIdToString l
              
convertNode :: I.Node e x -> LabelMapM (M.Map A.Lstring A.Block, Maybe Pblock) 
               -> LabelMapM (M.Map A.Lstring A.Block, Maybe Pblock)
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
                                 Just (l, phis, cs) -> return (M.insert (getLabelId l) (A.Block l
                                                                           (reverse phis) (reverse cs) a') bs, 
                                                               Nothing)
                               }

graphToBlocks :: H.Graph I.Node H.C H.C -> LabelMapM (M.Map A.Lstring A.Block)
graphToBlocks g = do { (bs, Nothing) <- H.foldGraphNodes convertNode g (return (M.empty, Nothing))
                     ; return bs
                     }

run :: IdLabelMap -> LabelMapM a -> I.M a
run m (LabelMapM f) = do { (_, a') <- f m 
                         ; return a'
                         }


toplevel2Ast :: I.Toplevel -> LabelMapM A.Toplevel
toplevel2Ast (I.ToplevelTarget k q) = return $ A.ToplevelTarget k q
toplevel2Ast (I.ToplevelAlias g v l a) = convert a >>= return . (A.ToplevelAlias g v l)
toplevel2Ast (I.ToplevelDbgInit s i) = return $ A.ToplevelDbgInit s i
toplevel2Ast (I.ToplevelStandaloneMd s tv) = convert tv >>= return . (A.ToplevelStandaloneMd s)
toplevel2Ast (I.ToplevelNamedMd m ns) = do { m' <- convert m
                                           ; ns' <- mapM convert ns
                                           ; return $ A.ToplevelNamedMd m' ns'
                                           }
toplevel2Ast (I.ToplevelDeclare f) = convert f >>= return . A.ToplevelDeclare


toplevel2Ast (I.ToplevelDefine f e g) = 
  do { bm <- graphToBlocks g
     ; f' <- convert f
     ; bs' <- getAlist f'
     ; let bs'' = reverse (foldl (\a b -> maybe a (\e -> e:a) (M.lookup b bm)) [] bs')
     ; return $ A.ToplevelDefine f' bs''
     }


toplevel2Ast (I.ToplevelGlobal a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11) = 
  do { a9' <- maybeM convert a9
     ; return $ A.ToplevelGlobal a1 a2 a3 a4 a5 a6 a7 a8 a9' a10 a11
     }
  
toplevel2Ast (I.ToplevelTypeDef lid t) = return $ A.ToplevelTypeDef lid t
toplevel2Ast (I.ToplevelDepLibs qs) = return $ A.ToplevelDepLibs qs
toplevel2Ast (I.ToplevelUnamedType i t) = return $ A.ToplevelUnamedType i t
toplevel2Ast (I.ToplevelModuleAsm q) = return $ A.ToplevelModuleAsm q



irToAst :: IdLabelMap -> I.Module -> I.M A.Module
irToAst m (I.Module ts) = run m $ Md.liftM A.Module (mapM toplevel2Ast ts)
