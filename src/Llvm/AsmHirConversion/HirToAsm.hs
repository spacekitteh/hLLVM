{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Llvm.AsmHirConversion.HirToAsm(hirToAsm) where

#define FLC  (FileLoc $(srcLoc))

import qualified Compiler.Hoopl as H
import qualified Control.Monad as Md
import qualified Data.Map as M
import qualified Llvm.Asm.Data as A
import qualified Llvm.Hir.Data as I
import Llvm.Hir.Composer (ptr0)
import Llvm.Hir.Cast
import Llvm.Util.Monadic (maybeM, pairM)
import Llvm.AsmHirConversion.TypeConversion
import Control.Monad.Reader
import Llvm.AsmHirConversion.Specialization
import Llvm.ErrorLoc
import Llvm.Hir.DataLayoutMetrics
import Llvm.Hir.Target

class Conversion l1 l2 | l1 -> l2 where
  convert :: l1 -> l2

data ReaderData = ReaderData { mp :: (M.Map (I.Gname, H.Label) A.LabelId)
                             , funname :: I.Gname
                             }
type Rm = Reader ReaderData

withFunName :: I.Gname -> Rm a -> Rm a
withFunName g = withReader (\(ReaderData x _) -> ReaderData x g)


instance Conversion a (Rm b) => Conversion (Maybe a) (Rm (Maybe b)) where
  convert (Just x) = Md.liftM Just (convert x)
  convert Nothing = return Nothing

instance (Conversion a (Rm c), Conversion b (Rm c)) => Conversion (Either a b) (Rm c) where
  convert (Left x) = (convert x)
  convert (Right x) = (convert x)

{- Ir to Ast conversion -} 
instance Conversion H.Label (Rm A.LabelId) where
  convert l = do { (ReaderData r fn) <- ask
                 ; case M.lookup (fn,l) r of 
                   Just l0 -> return l0
                   Nothing -> return $  A.LabelDqString $ "_hoopl_label_" ++ show l 
                 }
              
convert_to_LabelExt :: I.Gname -> H.Label -> Rm A.LabelId              
convert_to_LabelExt fn l = do { (ReaderData r _) <- ask
                              ; case M.lookup (fn,l) r of 
                                Just l0 -> return l0
                                Nothing -> errorLoc FLC $ show (fn,l)
                              }

convert_to_PercentLabelExt :: I.Gname -> H.Label -> Rm A.PercentLabel
convert_to_PercentLabelExt fn l = Md.liftM A.PercentLabel (convert_to_LabelExt fn l)


convert_to_PercentLabel :: H.Label -> Rm A.PercentLabel
convert_to_PercentLabel l = Md.liftM A.PercentLabel (convert l)

convert_to_TargetLabel :: H.Label -> Rm A.TargetLabel
convert_to_TargetLabel l = Md.liftM (A.TargetLabel . A.PercentLabel) (convert l)

convert_to_BlockLabel :: H.Label -> Rm A.BlockLabel
convert_to_BlockLabel l = Md.liftM A.ExplicitBlockLabel (convert l)
    
cnowrap :: Maybe I.NoWrap -> [A.TrapFlag]
cnowrap = maybe [] (\x -> case x of
                       I.Nsw -> [A.Nsw]
                       I.Nuw -> [A.Nuw]
                       I.Nsuw -> [A.Nsw, A.Nuw]
                   )
cexact :: Maybe a -> [A.TrapFlag]          
cexact = maybe [] (\_ -> [A.Exact])


instance Conversion v1 (Rm v2) => Conversion (I.Conversion I.ScalarB v1) (Rm (A.Conversion v2)) where
  convert x = let (op, t1, u, dt1) = case x of
                    I.Trunc (I.T t0 u0) dt0 -> (A.Trunc, tconvert () t0, u0, tconvert () dt0) 
                    I.Zext (I.T t0 u0) dt0 -> (A.Zext, tconvert () t0, u0, tconvert () dt0)
                    I.Sext (I.T t0 u0) dt0 -> (A.Sext, tconvert () t0, u0, tconvert () dt0)
                    I.FpTrunc (I.T t0 u0) dt0 -> (A.FpTrunc, tconvert () t0, u0, tconvert () dt0)
                    I.FpExt (I.T t0 u0) dt0 -> (A.FpExt, tconvert () t0, u0, tconvert () dt0)
                    I.FpToUi (I.T t0 u0) dt0 -> (A.FpToUi, tconvert () t0, u0, tconvert () dt0)
                    I.FpToSi (I.T t0 u0) dt0 -> (A.FpToSi, tconvert () t0, u0, tconvert () dt0)
                    I.UiToFp (I.T t0 u0) dt0 -> (A.UiToFp, tconvert () t0, u0, tconvert () dt0)
                    I.SiToFp (I.T t0 u0) dt0 -> (A.SiToFp, tconvert () t0, u0, tconvert () dt0)
                    I.PtrToInt (I.T t0 u0) dt0 -> (A.PtrToInt, tconvert () t0, u0, tconvert () dt0)
                    I.IntToPtr (I.T t0 u0) dt0 -> (A.IntToPtr, tconvert () t0, u0, tconvert () dt0)
                    I.Bitcast (I.T t0 u0) dt0 -> (A.Bitcast, tconvert () t0, u0, tconvert () dt0)
                    I.AddrSpaceCast (I.T t0 u0) dt0 -> (A.AddrSpaceCast, tconvert () t0, u0, tconvert () dt0)
              in do { u1 <- convert u
                    ; return $ A.Conversion op (A.Typed t1 u1) dt1
                    }

instance Conversion v1 (Rm v2) => Conversion (I.Conversion I.VectorB v1) (Rm (A.Conversion v2)) where
  convert x = let (op, t1, u, dt1) = case x of
                    I.Trunc (I.T t0 u0) dt0 -> (A.Trunc, tconvert () t0, u0, tconvert () dt0) 
                    I.Zext (I.T t0 u0) dt0 -> (A.Zext, tconvert () t0, u0, tconvert () dt0)
                    I.Sext (I.T t0 u0) dt0 -> (A.Sext, tconvert () t0, u0, tconvert () dt0)
                    I.FpTrunc (I.T t0 u0) dt0 -> (A.FpTrunc, tconvert () t0, u0, tconvert () dt0)
                    I.FpExt (I.T t0 u0) dt0 -> (A.FpExt, tconvert () t0, u0, tconvert () dt0)
                    I.FpToUi (I.T t0 u0) dt0 -> (A.FpToUi, tconvert () t0, u0, tconvert () dt0)
                    I.FpToSi (I.T t0 u0) dt0 -> (A.FpToSi, tconvert () t0, u0, tconvert () dt0)
                    I.UiToFp (I.T t0 u0) dt0 -> (A.UiToFp, tconvert () t0, u0, tconvert () dt0)
                    I.SiToFp (I.T t0 u0) dt0 -> (A.SiToFp, tconvert () t0, u0, tconvert () dt0)
                    I.PtrToInt (I.T t0 u0) dt0 -> (A.PtrToInt, tconvert () t0, u0, tconvert () dt0)
                    I.IntToPtr (I.T t0 u0) dt0 -> (A.IntToPtr, tconvert () t0, u0, tconvert () dt0)
                    I.Bitcast (I.T t0 u0) dt0 -> (A.Bitcast, tconvert () t0, u0, tconvert () dt0)
                    I.AddrSpaceCast (I.T t0 u0) dt0 -> (A.AddrSpaceCast, tconvert () t0, u0, tconvert () dt0)
              in do { u1 <- convert u
                    ; return $ A.Conversion op (A.Typed t1 u1) dt1
                    }
                 
mkConversion :: (A.ConvertOp, A.Type, I.Const I.Gname, A.Type) -> Rm A.Const
mkConversion (op, t1, u, dt1) = do { u1 <- convert u
                                   ; return $ A.C_conv $ A.Conversion op (A.Typed t1 u1) dt1                                   
                                   }

instance (Conversion v1 (Rm v2), Conversion idx1 (Rm v2)) => Conversion (I.GetElementPtr I.ScalarB v1 idx1) 
         (Rm (A.GetElementPtr v2)) where
  convert (I.GetElementPtr b u us) = do { ua <- convert u
                                        ; usa <- mapM convert us
                                        ; return $ A.GetElementPtr b (A.Pointer ua) usa
                                        }
          
instance (Conversion v1 (Rm v2), Conversion idx1 (Rm v2)) => Conversion (I.GetElementPtr I.VectorB v1 idx1) 
         (Rm (A.GetElementPtr v2)) where
  convert (I.GetElementPtr b u us) = do { ua <- convert u
                                        ; usa <- mapM convert us
                                        ; return $ A.GetElementPtr b (A.Pointer ua) usa
                                        }                                     

instance Conversion v1 (Rm v2) => Conversion (I.T I.ScalarType v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T I.Dtype v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.ScalarB I.I) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.ScalarB I.F) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.ScalarB I.P) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.VectorB I.I) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.VectorB I.F) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.VectorB I.P) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.RecordB I.D) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.CodeFunB I.X) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.CodeLabelB I.X) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance Conversion v1 (Rm v2) => Conversion (I.T (I.Type I.FirstClassB I.D) v1) (Rm (A.Typed v2)) where
  convert (I.T t v) = Md.liftM (A.Typed $ tconvert () t) (convert v)

instance (Conversion v1 (Rm v2)) => Conversion (I.Select I.ScalarB I.I v1) (Rm (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance (Conversion v1 (Rm v2)) => Conversion (I.Select I.ScalarB I.F v1) (Rm (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance (Conversion v1 (Rm v2)) => Conversion (I.Select I.ScalarB I.P v1) (Rm (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance (Conversion v1 (Rm v2)) => Conversion (I.Select I.VectorB I.I v1) (Rm (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance (Conversion v1 (Rm v2)) => Conversion (I.Select I.VectorB I.F v1) (Rm (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance (Conversion v1 (Rm v2)) => Conversion (I.Select I.VectorB I.P v1) (Rm (A.Select v2)) where
  convert (I.Select u1 u2 u3) = Md.liftM3 A.Select (convert u1) (convert u2) (convert u3)

instance Conversion v1 (Rm v2) => Conversion (I.Icmp I.ScalarB v1) (Rm (A.Icmp v2)) where
  convert (I.Icmp op t u1 u2) = Md.liftM2 (A.Icmp op (tconvert () t)) (convert u1) (convert u2)

instance Conversion v1 (Rm v2) => Conversion (I.Icmp I.VectorB v1) (Rm (A.Icmp v2)) where
  convert (I.Icmp op t u1 u2) = Md.liftM2 (A.Icmp op (tconvert () t)) (convert u1) (convert u2)

instance Conversion v1 (Rm v2) => Conversion (I.Fcmp I.ScalarB v1) (Rm (A.Fcmp v2)) where
  convert (I.Fcmp op t u1 u2) = Md.liftM2 (A.Fcmp op (tconvert () t)) (convert u1) (convert u2)

instance Conversion v1 (Rm v2) => Conversion (I.Fcmp I.VectorB v1) (Rm (A.Fcmp v2)) where
  convert (I.Fcmp op t u1 u2) = Md.liftM2 (A.Fcmp op (tconvert () t)) (convert u1) (convert u2)

instance Conversion v1 (Rm v2) => Conversion (I.ShuffleVector I.I v1) (Rm (A.ShuffleVector v2)) where
  convert (I.ShuffleVector u1 u2 u3) = Md.liftM3 A.ShuffleVector (convert u1) (convert u2) (convert u3)

instance Conversion v1 (Rm v2) => Conversion (I.ShuffleVector I.F v1) (Rm (A.ShuffleVector v2)) where
  convert (I.ShuffleVector u1 u2 u3) = Md.liftM3 A.ShuffleVector (convert u1) (convert u2) (convert u3)

instance Conversion v1 (Rm v2) => Conversion (I.ShuffleVector I.P v1) (Rm (A.ShuffleVector v2)) where
  convert (I.ShuffleVector u1 u2 u3) = Md.liftM3 A.ShuffleVector (convert u1) (convert u2) (convert u3)

instance Conversion v1 (Rm v2) => Conversion (I.ExtractValue v1) (Rm (A.ExtractValue v2)) where
  convert (I.ExtractValue u s) = convert u >>= \u' -> return $ A.ExtractValue u' s

instance Conversion v1 (Rm v2) => Conversion (I.InsertValue v1) (Rm (A.InsertValue v2)) where
  convert (I.InsertValue u1 u2 s) = do { u1' <- convert u1
                                       ; u2' <- convert u2
                                       ; return $ A.InsertValue u1' u2' s
                                       }

instance Conversion v1 (Rm v2) => Conversion (I.ExtractElement I.I v1) (Rm (A.ExtractElement v2)) where
  convert (I.ExtractElement u1 u2) = Md.liftM2 A.ExtractElement (convert u1) (convert u2)

instance Conversion v1 (Rm v2) => Conversion (I.ExtractElement I.F v1) (Rm (A.ExtractElement v2)) where
  convert (I.ExtractElement u1 u2) = Md.liftM2 A.ExtractElement (convert u1) (convert u2)

instance Conversion v1 (Rm v2) => Conversion (I.ExtractElement I.P v1) (Rm (A.ExtractElement v2)) where
  convert (I.ExtractElement u1 u2) = Md.liftM2 A.ExtractElement (convert u1) (convert u2)


instance Conversion v1 (Rm v2) => Conversion (I.InsertElement I.I v1) (Rm (A.InsertElement v2)) where
  convert (I.InsertElement u1 u2 u3) = Md.liftM3 A.InsertElement (convert u1) (convert u2) (convert u3)

instance Conversion v1 (Rm v2) => Conversion (I.InsertElement I.F v1) (Rm (A.InsertElement v2)) where
  convert (I.InsertElement u1 u2 u3) = Md.liftM3 A.InsertElement (convert u1) (convert u2) (convert u3)

instance Conversion v1 (Rm v2) => Conversion (I.InsertElement I.P v1) (Rm (A.InsertElement v2)) where
  convert (I.InsertElement u1 u2 u3) = Md.liftM3 A.InsertElement (convert u1) 
                                       (convert u2) (convert u3)





instance Conversion (I.Const I.Gname) (Rm A.Const) where
  convert x = case x of
    I.C_int s -> return $ A.C_simple $ A.CpInt s 
    I.C_uhex_int s -> return $ A.C_simple $ A.CpUhexInt s
    I.C_shex_int s -> return $ A.C_simple $ A.CpShexInt s 
    I.C_float s -> return $ A.C_simple $ A.CpFloat s
    I.C_null -> return $ A.C_simple $ A.CpNull
    I.C_undef -> return $ A.C_simple $ A.CpUndef
    I.C_true -> return $ A.C_simple $ A.CpTrue
    I.C_false -> return $ A.C_simple $ A.CpFalse
    I.C_zeroinitializer -> return $ A.C_simple $ A.CpZeroInitializer
    I.C_globalAddr s -> return $ A.C_simple $ A.CpGlobalAddr (unspecializeGlobalId s)
    I.C_str s -> return $ A.C_simple $ A.CpStr s
    I.C_u8 s -> return $ A.C_simple $ A.CpBconst $ A.BconstUint8 s
    I.C_u16 s -> return $ A.C_simple $ A.CpBconst $ A.BconstUint16 s
    I.C_u32 s -> return $ A.C_simple $ A.CpBconst $ A.BconstUint32 s 
    I.C_u64 s -> return $ A.C_simple $ A.CpBconst $ A.BconstUint64 s
    I.C_u96 s -> return $ A.C_simple $ A.CpBconst $ A.BconstUint96 s 
    I.C_u128 s -> return $ A.C_simple $ A.CpBconst $ A.BconstUint128 s 
    I.C_s8 s ->  return $ A.C_simple $ A.CpBconst $ A.BconstInt8 s 
    I.C_s16 s -> return $ A.C_simple $ A.CpBconst $ A.BconstInt16 s
    I.C_s32 s -> return $ A.C_simple $ A.CpBconst $ A.BconstInt32 s 
    I.C_s64 s -> return $ A.C_simple $ A.CpBconst $ A.BconstInt64 s 
    I.C_s96 s -> return $ A.C_simple $ A.CpBconst $ A.BconstInt96 s 
    I.C_s128 s -> return $ A.C_simple $ A.CpBconst $ A.BconstInt128 s 

    I.C_struct b fs -> Md.liftM (A.C_complex . (A.Cstruct b)) (mapM convert fs)
    I.C_vector fs -> Md.liftM (A.C_complex . A.Cvector) (mapM convert fs)
    I.C_array fs -> Md.liftM (A.C_complex . A.Carray) (mapM convert fs)
    I.C_vectorN n fs -> do { v <- convert fs
                           ; return  (A.C_complex $ A.Cvector $ (fmap (\_ -> v) [1..n]))
                           }
    I.C_arrayN n fs -> do { v <- convert fs
                          ; return (A.C_complex $ A.Carray $ (fmap (\_ -> v) [1..n]))
                          }
    I.C_labelId a -> Md.liftM A.C_labelId (convert a)
    I.C_block g a -> do { a' <- convert_to_PercentLabelExt g a
                        ; return $ A.C_blockAddress (unspecializeGlobalId g) a'
                        }
    I.C_add nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                          (Md.liftM2 (A.IbinExpr A.Add (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_sub nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                          (Md.liftM2 (A.IbinExpr A.Sub (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_mul nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                          (Md.liftM2 (A.IbinExpr A.Mul (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_udiv nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                           (Md.liftM2 (A.IbinExpr A.Udiv (cexact nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_sdiv nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                           (Md.liftM2 (A.IbinExpr A.Sdiv (cexact nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_urem t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                        (Md.liftM2 (A.IbinExpr A.Urem [] (tconvert () t)) (convert u1) (convert u2))
    I.C_srem t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                        (Md.liftM2 (A.IbinExpr A.Srem [] (tconvert () t)) (convert u1) (convert u2))
    I.C_shl nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                          (Md.liftM2 (A.IbinExpr A.Shl (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_lshr nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                           (Md.liftM2 (A.IbinExpr A.Lshr (cexact nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_ashr nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                           (Md.liftM2 (A.IbinExpr A.Ashr (cexact nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_and t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                       (Md.liftM2 (A.IbinExpr A.And [] (tconvert () t)) (convert u1) (convert u2))
    I.C_or t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                      (Md.liftM2 (A.IbinExpr A.Or [] (tconvert () t)) (convert u1) (convert u2))
    I.C_xor t u1 u2 -> Md.liftM (A.C_binexp . A.Ie)
                       (Md.liftM2 (A.IbinExpr A.Xor [] (tconvert () t)) (convert u1) (convert u2))
    I.C_add_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                            (Md.liftM2 (A.IbinExpr A.Add (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_sub_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                            (Md.liftM2 (A.IbinExpr A.Sub (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_mul_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                            (Md.liftM2 (A.IbinExpr A.Mul (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_udiv_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                             $ Md.liftM2 (A.IbinExpr A.Udiv (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_sdiv_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                             $ Md.liftM2 (A.IbinExpr A.Sdiv (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_urem_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                          $ Md.liftM2 (A.IbinExpr A.Urem [] (tconvert () t)) (convert u1) (convert u2)
    I.C_srem_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                          $ Md.liftM2 (A.IbinExpr A.Srem [] (tconvert () t)) (convert u1) (convert u2)
    I.C_shl_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                            $ Md.liftM2 (A.IbinExpr A.Shl (cnowrap nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_lshr_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                             $ Md.liftM2 (A.IbinExpr A.Lshr (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_ashr_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                             $ Md.liftM2 (A.IbinExpr A.Ashr (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_and_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                         $ Md.liftM2 (A.IbinExpr A.And [] (tconvert () t)) (convert u1) (convert u2)
    I.C_or_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                        $ Md.liftM2 (A.IbinExpr A.Or [] (tconvert () t)) (convert u1) (convert u2)
    I.C_xor_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) 
                         $ Md.liftM2 (A.IbinExpr A.Xor [] (tconvert () t)) (convert u1) (convert u2)
    I.C_fadd fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) 
                           $ Md.liftM2 (A.FbinExpr A.Fadd fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fsub fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) 
                           $ Md.liftM2 (A.FbinExpr A.Fsub fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fmul fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) 
                           $ Md.liftM2 (A.FbinExpr A.Fmul fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fdiv fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) 
                           $ Md.liftM2 (A.FbinExpr A.Fdiv fg (tconvert () t)) (convert u1) (convert u2)
    I.C_frem fg t u1 u2 -> 
      Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Frem fg (tconvert () t)) (convert u1) (convert u2)

    I.C_fadd_V fg t u1 u2 -> 
      Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fadd fg (tconvert () t)) (convert u1) (convert u2)
      
    I.C_fsub_V fg t u1 u2 -> 
      Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fsub fg (tconvert () t)) (convert u1) (convert u2)
                             
    I.C_fmul_V fg t u1 u2 -> 
      Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fmul fg (tconvert () t)) (convert u1) (convert u2)
                             
    I.C_fdiv_V fg t u1 u2 -> 
      Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fdiv fg (tconvert () t)) (convert u1) (convert u2)
      
    I.C_frem_V fg t u1 u2 -> 
      Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Frem fg (tconvert () t)) (convert u1) (convert u2)

    I.C_trunc (I.T t0 u0) dt0 -> mkConversion (A.Trunc, tconvert () t0, u0, tconvert () dt0) 
    I.C_zext (I.T t0 u0) dt0 -> mkConversion (A.Zext, tconvert () t0, u0, tconvert () dt0)
    I.C_sext (I.T t0 u0) dt0 -> mkConversion (A.Sext, tconvert () t0, u0, tconvert () dt0)
    I.C_fptrunc (I.T t0 u0) dt0 -> mkConversion (A.FpTrunc, tconvert () t0, u0, tconvert () dt0)
    I.C_fpext (I.T t0 u0) dt0 -> mkConversion (A.FpExt, tconvert () t0, u0, tconvert () dt0)
    I.C_fptoui (I.T t0 u0) dt0 -> mkConversion (A.FpToUi, tconvert () t0, u0, tconvert () dt0)
    I.C_fptosi (I.T t0 u0) dt0 -> mkConversion (A.FpToSi, tconvert () t0, u0, tconvert () dt0)
    I.C_uitofp (I.T t0 u0) dt0 -> mkConversion (A.UiToFp, tconvert () t0, u0, tconvert () dt0)
    I.C_sitofp (I.T t0 u0) dt0 -> mkConversion (A.SiToFp, tconvert () t0, u0, tconvert () dt0)
    I.C_ptrtoint (I.T t0 u0) dt0 -> mkConversion (A.PtrToInt, tconvert () t0, u0, tconvert () dt0)
    I.C_inttoptr (I.T t0 u0) dt0 -> mkConversion (A.IntToPtr, tconvert () t0, u0, tconvert () dt0)
    I.C_addrspacecast (I.T t0 u0) dt0 -> mkConversion (A.AddrSpaceCast, tconvert () t0, u0, tconvert () dt0)

    I.C_bitcast (I.T t0 u0) dt0 -> mkConversion (A.Bitcast, tconvert () t0, u0, tconvert () dt0)
    
    I.C_trunc_V (I.T t0 u0) dt0 -> mkConversion (A.Trunc, tconvert () t0, u0, tconvert () dt0) 
    I.C_zext_V (I.T t0 u0) dt0 -> mkConversion (A.Zext, tconvert () t0, u0, tconvert () dt0)
    I.C_sext_V (I.T t0 u0) dt0 -> mkConversion (A.Sext, tconvert () t0, u0, tconvert () dt0)
    I.C_fptrunc_V (I.T t0 u0) dt0 -> mkConversion (A.FpTrunc, tconvert () t0, u0, tconvert () dt0)
    I.C_fpext_V (I.T t0 u0) dt0 -> mkConversion (A.FpExt, tconvert () t0, u0, tconvert () dt0)
    I.C_fptoui_V (I.T t0 u0) dt0 -> mkConversion (A.FpToUi, tconvert () t0, u0, tconvert () dt0)
    I.C_fptosi_V (I.T t0 u0) dt0 -> mkConversion (A.FpToSi, tconvert () t0, u0, tconvert () dt0)
    I.C_uitofp_V (I.T t0 u0) dt0 -> mkConversion (A.UiToFp, tconvert () t0, u0, tconvert () dt0)
    I.C_sitofp_V (I.T t0 u0) dt0 -> mkConversion (A.SiToFp, tconvert () t0, u0, tconvert () dt0)
    I.C_ptrtoint_V (I.T t0 u0) dt0 -> mkConversion (A.PtrToInt, tconvert () t0, u0, tconvert () dt0)
    I.C_inttoptr_V (I.T t0 u0) dt0 -> mkConversion (A.IntToPtr, tconvert () t0, u0, tconvert () dt0)
    I.C_addrspacecast_V (I.T t0 u0) dt0 -> mkConversion (A.AddrSpaceCast, tconvert () t0, u0, tconvert () dt0)

    I.C_getelementptr b u us -> do { ua <- convert u
                                   ; usa <- mapM convert us
                                   ; return $ A.C_gep $ A.GetElementPtr b (A.Pointer ua) usa
                                   }

    I.C_getelementptr_V b u us -> do { ua <- convert u
                                     ; usa <- mapM convert us
                                     ; return $ A.C_gep $ A.GetElementPtr b (A.Pointer ua) usa
                                     }
    I.C_select_I a -> Md.liftM A.C_select (convert a)
    I.C_select_F a -> Md.liftM A.C_select (convert a)
    I.C_select_P a -> Md.liftM A.C_select (convert a)
    I.C_select_First cnd t f -> do { cnda <- convert cnd 
                                   ; ta <- convert t
                                   ; fa <- convert f
                                   ; return $ A.C_select (A.Select cnda ta fa)
                                   }
    I.C_select_VI a -> Md.liftM A.C_select (convert a)
    I.C_select_VF a -> Md.liftM A.C_select (convert a)
    I.C_select_VP a -> Md.liftM A.C_select (convert a)
    I.C_icmp a -> Md.liftM A.C_icmp (convert a)
    I.C_icmp_V a -> Md.liftM A.C_icmp (convert a)
    I.C_fcmp a -> Md.liftM A.C_fcmp (convert a)
    I.C_fcmp_V a -> Md.liftM A.C_fcmp (convert a)
    I.C_shufflevector_I a -> Md.liftM A.C_shufflevector (convert a)
    I.C_shufflevector_F a -> Md.liftM A.C_shufflevector (convert a)
    I.C_shufflevector_P a -> Md.liftM A.C_shufflevector (convert a)
    I.C_extractvalue a -> Md.liftM A.C_extractvalue (convert a)
    I.C_insertvalue a -> Md.liftM A.C_insertvalue (convert a)
    I.C_extractelement_I a -> Md.liftM A.C_extractelement (convert a)
    I.C_extractelement_F a -> Md.liftM A.C_extractelement (convert a)
    I.C_extractelement_P a -> Md.liftM A.C_extractelement (convert a) 
    I.C_insertelement_I a -> Md.liftM A.C_insertelement (convert a)
    I.C_insertelement_F a -> Md.liftM A.C_insertelement (convert a)
    I.C_insertelement_P a -> Md.liftM A.C_insertelement (convert a)


instance Conversion I.MdName (Rm A.MdName) where
  convert (I.MdName s) = return $ A.MdName s

instance Conversion I.MdNode (Rm A.MdNode) where
  convert (I.MdNode s) = return $ A.MdNode s
  
instance Conversion I.MdRef (Rm A.MdRef) where  
  convert x = case x of
    I.MdRefName c -> Md.liftM A.MdRefName (convert c)
    I.MdRefNode c -> Md.liftM A.MdRefNode (convert c)

instance Conversion (I.MetaConst I.Gname) (Rm A.MetaConst) where
    convert (I.McStruct c) = Md.liftM A.McStruct $ mapM convert c
    convert (I.McString s) = return $ A.McString s
    convert (I.McMdRef n) = Md.liftM A.McMdRef $ convert n
    convert (I.McSsa i) = return $ A.McSsa $ unLid i
    convert (I.McSimple sc) = Md.liftM A.McSimple (convert sc)

instance Conversion (I.MetaKindedConst I.Gname) (Rm A.MetaKindedConst) where
  convert x = case x of
    (I.MetaKindedConst mk mc) -> Md.liftM (A.MetaKindedConst (tconvert () mk)) (convert mc)
    I.UnmetaKindedNull -> return A.UnmetaKindedNull

instance Conversion (I.Value I.Gname) (Rm A.Value) where
    convert (I.Val_ssa a) = return $ A.Val_local $ unLid a
    convert (I.Val_const a) = Md.liftM A.Val_const (convert a)

instance Conversion I.CallSiteType (Rm A.Type) where
  convert t = case t of
    I.CallSiteTypeRet x -> return $ tconvert () x
    I.CallSiteTypeFun ft as -> return $ tconvert () (I.Tpointer (ucast ft) as)
    
instance Conversion (I.FunPtr I.Gname) (Rm A.FunName) where
  convert x = case x of
    I.Fun_null -> return A.FunName_null
    I.Fun_undef -> return A.FunName_undef    
    I.FunId g -> return $ A.FunNameGlobal (A.GolG $ unspecializeGlobalId g)
    I.FunSsa l -> return $ A.FunNameGlobal (A.GolL $ unLid l)
    I.FunIdBitcast (I.T st c) dt -> do { tv1 <- convert (I.T st c)
                                       ; return $ A.FunNameBitcast tv1 (tconvert () dt)
                                       }
    I.FunIdInttoptr (I.T st c) dt -> do { tv1 <- convert (I.T st c)
                                        ; return $ A.FunNameInttoptr tv1 (tconvert () dt)
                                        }

instance Conversion (I.FunPtr I.Gname, I.CallFunInterface I.Gname) (Rm (A.TailCall, A.CallSite)) where
  convert  (fn, I.CallFunInterface { I.cfi_tail = tc, I.cfi_castType = castType
                                   , I.cfi_signature = sig, I.cfi_funAttrs = fa}) = 
    do { fna <- convert fn
       ; (cc, pa, ret, aps) <- convert sig
       ; let funType = case castType of
               Just t -> tconvert () t
               Nothing -> ret
       ; return (tc, A.CallSiteFun cc pa funType fna aps fa)
       }
    
instance Conversion (I.FunPtr I.Gname, I.InvokeFunInterface I.Gname) (Rm A.CallSite) where
  convert  (fn, I.InvokeFunInterface { I.ifi_castType = castType, I.ifi_signature = sig, I.ifi_funAttrs = fa}) =  
    do { fna <- convert fn
       ; (cc, pa, ret, aps) <- convert sig
       ; let funType = case castType of
               Just t -> tconvert () t
               Nothing -> ret
       ; return (A.CallSiteFun cc pa funType fna aps fa)
       }
    
instance Conversion (I.Type I.CodeFunB I.X) (Rm (A.Type, [I.RetAttr], Maybe A.VarArgParam)) where    
  convert t@(I.Tfunction (_,retAttrs) pts ma) = return (tconvert () (ptr0 t), retAttrs, ma)
                                          
getReturnType :: I.Type I.CodeFunB I.X -> Rm (A.Type, [I.RetAttr], Maybe A.VarArgParam)
getReturnType t@(I.Tfunction (rt,retAttrs) _ ma) = return (tconvert () rt, retAttrs, ma)

instance Conversion (I.AsmCode, I.CallAsmInterface I.Gname) (Rm A.InlineAsmExp) where
  convert (I.AsmCode dia s1 s2, I.CallAsmInterface t mse mas aps fa) = 
    do { apsa <- mapM convert aps
       ; (ta,_,_) <- convert t
       ; return (A.InlineAsmExp ta mse mas dia s1 s2 apsa fa)
       }

instance Conversion (I.Clause I.Gname) (Rm A.Clause) where
    convert (I.Catch tv) = convert tv >>= \tv' -> return $ A.ClauseCatch tv'
    convert (I.Filter tc) = convert tc >>= \tc' -> return $ A.ClauseFilter tc'
    convert (I.CcoS tc) = convert tc >>= return . A.ClauseConversion
    convert (I.CcoV tc) = convert tc >>= return . A.ClauseConversion

instance Conversion I.GlobalOrLocalId (Rm A.GlobalOrLocalId) where
    convert g = return g

instance Conversion (I.Minst I.Gname) (Rm A.ComputingInst) where
  convert mi = 
    let I.Minst cst fn params = unspecializeMinst mi
    in do { fna <- convert (I.FunId fn)
          ; cst0 <- convert cst
          ; apa <- mapM convert params
          ; return $ A.ComputingInst Nothing $ A.RhsCall A.TcNon $ A.CallSiteFun Nothing [] cst0 fna apa []
          }

instance Conversion (I.Cinst I.Gname) (Rm A.ComputingInst) where
  convert cinst = case unspecializeRegisterIntrinsic cinst of
    Just (gid, typ, opds, lhs) -> 
      do { opdsa <- mapM convert opds
         ; let rtTyp = maybe A.Tvoid (\_ -> tconvert () typ) lhs
         ; return $ A.ComputingInst (fmap unLid lhs) $ A.RhsCall A.TcNon $ A.CallSiteFun Nothing [] rtTyp 
           (A.FunNameGlobal $ A.GolG $ unspecializeGlobalId gid) opdsa []
         }
    Nothing -> 
      case maybe cinst id (unspecializeIntrinsics cinst) of 
        I.I_alloca mar t mtv ma lhs -> 
          do { mtva <- maybeM convert mtv 
             ; return $ A.ComputingInst (Just (unLid lhs)) $ A.RhsMemOp $ A.Alloca mar (tconvert () t) mtva ma
             }
        I.I_load atom tv aa nonterm invr nonull lhs -> 
          do { tva <- convert tv 
             ; return $ A.ComputingInst (Just (unLid lhs)) $ A.RhsMemOp $ A.Load atom (A.Pointer tva) aa nonterm invr nonull
             }
        I.I_loadatomic atom v tv aa lhs -> 
          do { tva <- convert tv 
             ; return $ A.ComputingInst (Just (unLid lhs)) $ A.RhsMemOp $ A.LoadAtomic atom v (A.Pointer tva) aa
             }
        I.I_store atom tv1 tv2 aa nonterm -> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; return $ A.ComputingInst Nothing (A.RhsMemOp $ A.Store atom tv1a (A.Pointer tv2a) aa nonterm)
             }
        I.I_storeatomic atom v tv1 tv2 aa -> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; return $ A.ComputingInst Nothing (A.RhsMemOp $ A.StoreAtomic atom v tv1a (A.Pointer tv2a) aa)
             }
        I.I_cmpxchg_I wk b1 tv1 tv2 tv3 b2 sord ford lhs-> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; tv3a <- convert tv3
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsMemOp $ A.CmpXchg wk b1 (A.Pointer tv1a) tv2a tv3a b2 sord ford)
             }
        I.I_cmpxchg_F wk b1 tv1 tv2 tv3 b2 sord ford lhs->
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; tv3a <- convert tv3
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsMemOp $ A.CmpXchg wk b1 (A.Pointer tv1a) tv2a tv3a b2 sord ford)
             }
        I.I_cmpxchg_P wk b1 tv1 tv2 tv3 b2 sord ford lhs->
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; tv3a <- convert tv3
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsMemOp $ A.CmpXchg wk b1 (A.Pointer tv1a) tv2a tv3a b2 sord ford)
             }    
        I.I_atomicrmw b1 op tv1 tv2 b2 mf lhs-> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsMemOp $ A.AtomicRmw b1 op (A.Pointer tv1a) tv2a b2 mf)
             }
        I.I_fence b fo -> return $ A.ComputingInst Nothing $ A.RhsMemOp $ A.Fence b fo 
        I.I_va_arg tv t lhs-> 
          do { tv1 <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) $ A.RhsVaArg $ A.VaArg tv1 (tconvert () t)
             }
        I.I_landingpad t1 t2 pf b cs lhs-> 
          do { pfa <- convert pf
             ; csa <- mapM convert cs
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsLandingPad $ A.LandingPad (tconvert () t1) (tconvert () t2) pfa b csa)
             }
        I.I_extractelement_I tv1 tv2 lhs-> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExtractElement $ A.ExtractElement tv1a tv2a)
             }
        I.I_extractelement_F tv1 tv2 lhs-> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExtractElement $ A.ExtractElement tv1a tv2a)
             }
        I.I_extractelement_P tv1 tv2 lhs-> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExtractElement $ A.ExtractElement tv1a tv2a)
             }    
        I.I_extractvalue tv1 idx lhs-> 
          do { tv1a <- convert tv1
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExtractValue $ A.ExtractValue tv1a idx)
             }
        I.I_getelementptr b ptr idx lhs-> 
          do { ptra <- convert ptr
             ; idxa <- mapM convert idx
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprGetElementPtr $ A.GetElementPtr b (A.Pointer ptra) idxa) 
             }
        I.I_getelementptr_V b ptr idx lhs ->
          do { ptra <- convert ptr
             ; idxa <- mapM convert idx
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprGetElementPtr $ A.GetElementPtr b (A.Pointer ptra) idxa) 
             }    
        I.I_icmp op t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprIcmp $ A.Icmp op (tconvert () t) v1a v2a)
             }
        I.I_icmp_V op t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprIcmp $ A.Icmp op (tconvert () t) v1a v2a)
             }    
        I.I_fcmp op t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprFcmp $ A.Fcmp op (tconvert () t) v1a v2a)
             }
        I.I_fcmp_V op t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprFcmp $ A.Fcmp op (tconvert () t) v1a v2a)
             }
        I.I_add n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Add (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_sub n t v1 v2 lhs -> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Sub (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_mul n t v1 v2 lhs -> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Mul (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_udiv n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Udiv (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_sdiv n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Sdiv (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_urem t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Urem [] (tconvert () t) v1a v2a)
             }
        I.I_srem t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Srem [] (tconvert () t) v1a v2a)
             }
        I.I_shl n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Shl (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_lshr n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Lshr (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_ashr n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Ashr (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_and t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2 
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.And [] (tconvert () t) v1a v2a)
             }
        I.I_or t v1 v2 lhs-> 
          do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just $ unLid lhs) 
           (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Or [] (tconvert () t) v1a v2a)
         }
        I.I_xor t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Xor [] (tconvert () t) v1a v2a)
             }
        I.I_add_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Add (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_sub_V n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Sub (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_mul_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Mul (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_udiv_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Udiv (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_sdiv_V n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Sdiv (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_urem_V t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Urem [] (tconvert () t) v1a v2a)
             }
        I.I_srem_V t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Srem [] (tconvert () t) v1a v2a)
             }
        I.I_shl_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Shl (cnowrap n) (tconvert () t) v1a v2a)
             }
        I.I_lshr_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Lshr (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_ashr_V n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Ashr (cexact n) (tconvert () t) v1a v2a)
             }
        I.I_and_V t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.And [] (tconvert () t) v1a v2a)
             }
        I.I_or_V t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Or [] (tconvert () t) v1a v2a)
             }
        I.I_xor_V t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Ie $ A.IbinExpr A.Xor [] (tconvert () t) v1a v2a)
             }    
        I.I_fadd n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
             }
        I.I_fsub n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fsub n (tconvert () t) v1a v2a)
             }
        I.I_fmul n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fmul n (tconvert () t) v1a v2a)
             }
        I.I_fdiv n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fdiv n (tconvert () t) v1a v2a)
             }
        I.I_frem n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Frem n (tconvert () t) v1a v2a)
             }
        I.I_fadd_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
             }
        I.I_fsub_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fsub n (tconvert () t) v1a v2a)
             }
        I.I_fmul_V n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fmul n (tconvert () t) v1a v2a)
             }
        I.I_fdiv_V n t v1 v2 lhs-> 
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Fdiv n (tconvert () t) v1a v2a)
             }
        I.I_frem_V n t v1 v2 lhs->
          do { v1a <- convert v1
             ; v2a <- convert v2
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprBinExpr $ A.Fe $ A.FbinExpr A.Frem n (tconvert () t) v1a v2a)
             }
        I.I_trunc tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Trunc tva (tconvert () dt)) 
             }
        I.I_zext tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Zext tva (tconvert () dt)) 
             }    
        I.I_sext tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Sext tva (tconvert () dt)) 
             }    
        I.I_fptrunc tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpTrunc tva (tconvert () dt)) 
             }    
        I.I_fpext tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpExt tva (tconvert () dt)) 
             }    
        I.I_fptoui tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpToUi tva (tconvert () dt)) 
             }    
        I.I_fptosi tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpToSi tva (tconvert () dt)) 
             }    
        I.I_uitofp tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.UiToFp tva (tconvert () dt)) 
             }    
        I.I_sitofp tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.SiToFp tva (tconvert () dt)) 
             }    
        I.I_ptrtoint tv dt lhs ->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.PtrToInt tva (tconvert () dt)) 
             }    
        I.I_inttoptr tv dt lhs -> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.IntToPtr tva (tconvert () dt)) 
             }    
        I.I_bitcast tv dt lhs -> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Bitcast tva (tconvert () dt)) 
             }    
        I.I_bitcast_D tv dt lhs -> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Bitcast tva (tconvert () dt)) 
             }      
        I.I_addrspacecast tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.AddrSpaceCast tva (tconvert () dt)) 
             }    
        I.I_trunc_V tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Trunc tva (tconvert () dt)) 
             }
        I.I_zext_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Zext tva (tconvert () dt)) 
             }    
        I.I_sext_V tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.Sext tva (tconvert () dt)) 
             }    
        I.I_fptrunc_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpTrunc tva (tconvert () dt)) 
             }    
        I.I_fpext_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpExt tva (tconvert () dt)) 
             }    
        I.I_fptoui_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpToUi tva (tconvert () dt)) 
             }    
        I.I_fptosi_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.FpToSi tva (tconvert () dt)) 
             }    
        I.I_uitofp_V tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.UiToFp tva (tconvert () dt)) 
             }    
        I.I_sitofp_V tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.SiToFp tva (tconvert () dt)) 
             }    
        I.I_ptrtoint_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.PtrToInt tva (tconvert () dt)) 
             }    
        I.I_inttoptr_V tv dt lhs->
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.IntToPtr tva (tconvert () dt)) 
             }    
        I.I_addrspacecast_V tv dt lhs-> 
          do { tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) 
               (A.RhsExpr $ A.ExprConversion $ A.Conversion A.AddrSpaceCast tva (tconvert () dt)) 
             }    
        I.I_select_I cnd t f lhs->
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_select_F cnd t f lhs->
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_select_P cnd t f lhs->
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_select_First cnd t f lhs->
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_select_VI cnd t f lhs-> 
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_select_VF cnd t f lhs-> 
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_select_VP cnd t f lhs-> 
          do { cnda <- convert cnd
             ; ta <- convert t
             ; fa <- convert f
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsExpr $ A.ExprSelect $ A.Select cnda ta fa) 
             }
        I.I_insertelement_I vtv tv idx lhs-> 
          do { vtva <- convert vtv
             ; tva <- convert tv
             ; idxa <- convert idx
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsInsertElement $ A.InsertElement vtva tva idxa) 
             }
        I.I_insertelement_F vtv tv idx lhs-> 
          do { vtva <- convert vtv
             ; tva <- convert tv
             ; idxa <- convert idx
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsInsertElement $ A.InsertElement vtva tva idxa) 
             }
        I.I_insertelement_P vtv tv idx lhs-> 
          do { vtva <- convert vtv
             ; tva <- convert tv
             ; idxa <- convert idx
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsInsertElement $ A.InsertElement vtva tva idxa) 
             }
        I.I_shufflevector_I tv1 tv2 tv3 lhs->
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; tv3a <- convert tv3
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsShuffleVector $ A.ShuffleVector tv1a tv2a tv3a) 
             }
        I.I_shufflevector_F tv1 tv2 tv3 lhs->
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; tv3a <- convert tv3
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsShuffleVector $ A.ShuffleVector tv1a tv2a tv3a) 
             }
        I.I_shufflevector_P tv1 tv2 tv3 lhs-> 
          do { tv1a <- convert tv1
             ; tv2a <- convert tv2
             ; tv3a <- convert tv3
             ; return $ A.ComputingInst (Just $ unLid lhs) (A.RhsShuffleVector $ A.ShuffleVector tv1a tv2a tv3a) 
             }
        I.I_insertvalue vtv tv idx lhs-> 
          do { vtva <- convert vtv
             ; tva <- convert tv
             ; return $ A.ComputingInst (Just $ unLid lhs) $ A.RhsInsertValue $ A.InsertValue vtva tva idx 
             }
        I.I_call_fun fn cfi lhs-> 
          do { (tc, csa) <- convert (fn, cfi)
             ; return $ A.ComputingInst (fmap unLid lhs) $ A.RhsCall tc csa 
             }
        I.I_call_asm asm cai lhs ->
          do { csa <- convert (asm, cai)
             ; return $ A.ComputingInst (fmap unLid lhs) $ A.RhsInlineAsm csa 
             } 
        _ -> errorLoc FLC $ show cinst
                                      

instance Conversion (I.FunOperand (I.Value I.Gname)) (Rm A.ActualParam) where
  convert x = case x of
    I.FunOperandData t pa ma v -> 
      do { va <- convert v 
         ; return $ A.ActualParamData (tconvert () t) 
           (appendAlignment ma (fmap unspecializePAttr pa)) va
         }
    I.FunOperandByVal t pa ma v -> 
      do { va <- convert v 
         ; return $ A.ActualParamData (tconvert () t) 
           (appendAlignment ma (A.PaByVal:(fmap unspecializePAttr pa))) va
         }                                      
    I.FunOperandLabel t pa ma (I.Val_const (I.C_labelId v)) -> 
      do { va <- convert_to_PercentLabel v 
         ; return $ A.ActualParamLabel (tconvert () t) 
           (appendAlignment ma (fmap unspecializePAttr pa)) va
         }
    I.FunOperandAsRet t pa1 ma v ->
      do { va <- convert v
         ; return $ A.ActualParamData (tconvert () t) 
           (appendAlignment ma (A.PaSRet:(fmap unspecializePAttr pa1))) va
         }      
    I.FunOperandExt I.Sign t pa1 ma v ->
      do { va <- convert v
         ; return $ A.ActualParamData (tconvert () t) 
           (appendAlignment ma (A.PaSignExt:(fmap unspecializePAttr pa1))) va
         }
    I.FunOperandExt I.Zero t pa1 ma v ->
      do { va <- convert v
         ; return $ A.ActualParamData (tconvert () t) 
           (appendAlignment ma (A.PaZeroExt:(fmap unspecializePAttr pa1))) va
         }      

instance Conversion (I.MetaOperand I.Gname) (Rm A.ActualParam) where
  convert x = case x of
    I.MetaOperandData t pa1 ma v -> 
      do { va <- convert v
         ; return $ A.ActualParamData (tconvert () t) (appendAlignment ma pa1) va
         }
    I.MetaOperandMeta mc -> Md.liftM (A.ActualParamMeta) (convert mc)

instance Conversion (I.Aliasee I.Gname) (Rm A.Const) where
  convert (I.Aliasee v) = return $ A.C_simple $ A.CpGlobalAddr $ unspecializeGlobalId v
  convert (I.AliaseeConversion a) = Md.liftM A.C_conv (convert a)
  convert (I.AliaseeGEP a) = Md.liftM A.C_gep (convert a)


convertAliasee :: I.Aliasee I.Gname -> Rm A.Aliasee 
convertAliasee ae = case ae of
  I.AliaseeTyped t a -> convert a >>= \ax -> return $ A.Aliasee (A.Typed (tconvert () t) ax)
  I.AliaseeConversion a ->  Md.liftM A.AliaseeConversion (convert a)
  I.AliaseeGEP a -> Md.liftM A.AliaseeGetElementPtr (convert a)

instance Conversion (I.Prefix I.Gname) (Rm A.Prefix) where
  convert (I.Prefix n) = Md.liftM A.Prefix (convert n)

instance Conversion (I.Prologue I.Gname) (Rm A.Prologue) where
  convert (I.Prologue n) = Md.liftM A.Prologue (convert n)

instance Conversion (I.TypedConstOrNull I.Gname) (Rm A.TypedConstOrNull) where
  convert x = case x of
    I.TypedConst tv -> Md.liftM A.TypedConst (convert tv)
    I.UntypedNull -> return A.UntypedNull

instance Conversion (I.FunOperand ()) (Rm A.FormalParam) where
  convert x = case x of
    I.FunOperandData dt pa ma _ -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (fmap unspecializePAttr pa)) 
      (A.FimplicitParam)
    I.FunOperandByVal dt pa ma _ -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaByVal:(fmap unspecializePAttr pa))) 
      (A.FimplicitParam)
    I.FunOperandAsRet dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaSRet:(fmap unspecializePAttr pa))) 
      A.FimplicitParam
    I.FunOperandExt I.Sign dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaSignExt:(fmap unspecializePAttr pa))) 
      A.FimplicitParam
    I.FunOperandExt I.Zero dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaZeroExt:(fmap unspecializePAttr pa))) 
      A.FimplicitParam

instance Conversion (I.FunOperand I.Lname) (Rm A.FormalParam) where
  convert x = case x of
    I.FunOperandData dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (fmap unspecializePAttr pa)) 
      (A.FexplicitParam $ unLid fp)
    I.FunOperandByVal dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaByVal:(fmap unspecializePAttr pa))) 
      (A.FexplicitParam $ unLid fp)
    I.FunOperandAsRet dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaSRet:(fmap unspecializePAttr pa))) 
      (A.FexplicitParam $ unLid fp)
    I.FunOperandExt I.Sign dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaSignExt:(fmap unspecializePAttr pa))) 
      (A.FexplicitParam $ unLid fp)
    I.FunOperandExt I.Zero dt pa ma fp -> 
      return $ A.FormalParamData (tconvert () dt) (appendAlignment ma (A.PaZeroExt:(fmap unspecializePAttr pa))) 
      (A.FexplicitParam $ unLid fp)
  
instance Conversion (I.FunSignature I.Lname) (Rm (Maybe A.CallConv, [A.ParamAttr], A.Type, A.FormalParamList)) where
  convert I.FunSignature { I.fs_callConv = cc, I.fs_type = typ, I.fs_params = paras } =
    do { (ret, attrs, mv) <- getReturnType typ
       ; paras0 <- mapM convert paras -- TODO : check the paramter types match fs_type
       ; return (Just cc, fmap unspecializeRetAttr attrs, ret, A.FormalParamList paras0 mv)
       }
         
instance Conversion (I.FunSignature ()) (Rm (Maybe A.CallConv, [A.ParamAttr], A.Type, A.FormalParamList)) where
  convert I.FunSignature { I.fs_callConv = cc, I.fs_type = typ, I.fs_params = paras } =
    do { (ret, attrs, mv) <- getReturnType typ
       ; paras0 <- mapM convert paras
       ; return (Just cc, fmap unspecializeRetAttr attrs, ret, A.FormalParamList paras0 mv)
       }

instance Conversion (I.FunSignature (I.Value I.Gname)) (Rm (Maybe A.CallConv, [A.ParamAttr], A.Type, [A.ActualParam])) where
  convert I.FunSignature { I.fs_callConv = cc, I.fs_type = typ, I.fs_params = paras } =
    do { (ret, attrs, mv) <- convert typ
       ; paras0 <- mapM convert paras
       ; return (Just cc, fmap unspecializeRetAttr attrs, ret, paras0)
       }
  

instance Conversion (I.FunctionInterface I.Gname) (Rm A.FunctionPrototype) where
  convert (I.FunctionInterface { I.fi_linkage = f0 
                               , I.fi_visibility = f1 
                               , I.fi_dllstorage = f2
                               , I.fi_signature = f3
                               , I.fi_fun_name = f6
                               , I.fi_addr_naming = f8
                               , I.fi_fun_attrs = f9 
                               , I.fi_section = f10 
                               , I.fi_comdat = f11
                               , I.fi_alignment = f12 
                               , I.fi_gc = f13
                               , I.fi_prefix = f14 
                               , I.fi_prologue = f15 }) =
    do { f15a <- convert f15
       ; f14a <- convert f14
       ; (f3a, f3b, f3c, f3d) <- convert f3
       ; return $ A.FunctionPrototype { A.fp_linkage = f0 
                                      , A.fp_visibility = f1 
                                      , A.fp_dllstorage = f2 
                                      , A.fp_callConv = f3a
                                      , A.fp_retAttrs = f3b
                                      , A.fp_retType = f3c
                                      , A.fp_fun_name = unspecializeGlobalId f6
                                      , A.fp_param_list = f3d
                                      , A.fp_addr_naming = f8
                                      , A.fp_fun_attrs = f9
                                      , A.fp_section = f10
                                      , A.fp_comdat = fmap convert_Comdat f11
                                      , A.fp_alignment = f12
                                      , A.fp_gc = f13
                                      , A.fp_prefix = f14a
                                      , A.fp_prologue = f15a
                                      }
       }



instance Conversion (I.FunctionDeclare I.Gname) (Rm A.FunctionPrototype) where
  convert x = case x of
    I.FunctionDeclareData { I.fd_linkage = f0 
                          , I.fd_visibility = f1 
                          , I.fd_dllstorage = f2 
                          , I.fd_signature = f3 
                          , I.fd_fun_name = f6
                          , I.fd_addr_naming = f8 
                          , I.fd_fun_attrs = f9 
                          , I.fd_section = f10 
                          , I.fd_comdat = f11 
                          , I.fd_alignment = f12 
                          , I.fd_gc = f13 
                          , I.fd_prefix = f14
                          , I.fd_prologue = f15
                          } ->
      do { f15a <- convert f15
         ; f14a <- convert f14
         ; (f3a, f3b, f3c, f3d) <- convert f3
         ; return $ A.FunctionPrototype { A.fp_linkage = f0 
                                        , A.fp_visibility = f1 
                                        , A.fp_dllstorage = f2 
                                        , A.fp_callConv = f3a
                                        , A.fp_retAttrs = f3b
                                        , A.fp_retType = f3c
                                        , A.fp_fun_name = unspecializeGlobalId f6
                                        , A.fp_param_list = f3d
                                        , A.fp_addr_naming = f8
                                        , A.fp_fun_attrs = f9
                                        , A.fp_section = f10
                                        , A.fp_comdat = fmap convert_Comdat f11
                                        , A.fp_alignment = f12
                                        , A.fp_gc = f13
                                        , A.fp_prefix = f14a
                                        , A.fp_prologue = f15a
                                        }
         }
    I.FunctionDeclareMeta { I.fd_retType = f5
                          , I.fd_fun_name = f6
                          , I.fd_metakinds = f7
                          , I.fd_fun_attrs = f8
                          } -> 
      do { let f7a = fmap (\x -> case x of
                              Left m -> A.FormalParamMeta (tconvert () m) A.FimplicitParam
                              Right m -> A.FormalParamData (tconvert () m) [] A.FimplicitParam
                          ) f7
         ; return $ A.FunctionPrototype { A.fp_linkage = Nothing
                                        , A.fp_visibility = Nothing
                                        , A.fp_dllstorage = Nothing
                                        , A.fp_callConv = Nothing
                                        , A.fp_retAttrs = []
                                        , A.fp_retType = (tconvert () f5)
                                        , A.fp_fun_name = unspecializeGlobalId f6
                                        , A.fp_param_list = A.FormalParamList f7a Nothing
                                        , A.fp_addr_naming = Nothing
                                        , A.fp_fun_attrs = f8
                                        , A.fp_section = Nothing
                                        , A.fp_comdat = Nothing
                                        , A.fp_alignment = Nothing
                                        , A.fp_gc = Nothing
                                        , A.fp_prefix = Nothing
                                        , A.fp_prologue = Nothing
                                        }
         }


instance Conversion (I.Pinst I.Gname) (Rm A.PhiInst) where
  convert (I.Pinst t branches mg) = 
    Md.liftM (A.PhiInst (Just $ unLid mg) (tconvert () t)) 
    (mapM (pairM convert convert_to_PercentLabel) branches)

instance Conversion (I.Tinst I.Gname) (Rm A.TerminatorInst) where
  convert (I.T_ret_void) = return A.RetVoid
  convert (I.T_return tvs) = Md.liftM A.Return (mapM convert tvs)
  convert (I.T_br t) = Md.liftM A.Br (convert_to_TargetLabel t)
  convert (I.T_cbr cnd t f) = Md.liftM3 A.Cbr (convert cnd) (convert_to_TargetLabel t) (convert_to_TargetLabel f)
  convert (I.T_indirectbr cnd bs) = Md.liftM2 A.IndirectBr (convert cnd) (mapM convert_to_TargetLabel bs)
  convert (I.T_switch (cnd,d) cases) = Md.liftM3 A.Switch (convert cnd) (convert_to_TargetLabel d) 
                                       (mapM (pairM convert convert_to_TargetLabel) cases)
  convert (I.T_invoke fptr ifi t f mg) = 
    Md.liftM3 (A.Invoke (fmap unLid mg)) (convert (fptr, ifi))
    (convert_to_TargetLabel t) (convert_to_TargetLabel f)
  convert (I.T_invoke_asm asm cai t f lhs) =
    Md.liftM3 (A.InvokeInlineAsm (fmap unLid lhs)) (convert (asm, cai))
    (convert_to_TargetLabel t) (convert_to_TargetLabel f)
  convert (I.T_resume tv) = Md.liftM A.Resume (convert tv)
  convert I.T_unreachable = return A.Unreachable
  convert I.T_unwind = return A.Unwind

instance Conversion (I.Dbg I.Gname) (Rm A.Dbg) where
  convert (I.Dbg mv mc) = Md.liftM2 A.Dbg (convert mv) (convert mc)

instance Conversion PhiInstWithDbg (Rm A.PhiInstWithDbg) where
  convert (PhiInstWithDbg ins dbgs) = Md.liftM2 A.PhiInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion CInstWithDbg (Rm A.ComputingInstWithDbg) where
  convert (CInstWithDbg ins dbgs) = Md.liftM2 A.ComputingInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion MInstWithDbg (Rm A.ComputingInstWithDbg) where
  convert (MInstWithDbg ins dbgs) = Md.liftM2 A.ComputingInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion TerminatorInstWithDbg (Rm A.TerminatorInstWithDbg) where
  convert (TerminatorInstWithDbg term dbgs) = Md.liftM2 A.TerminatorInstWithDbg (convert term) (mapM convert dbgs)

instance Conversion (I.TlAlias I.Gname) (Rm A.TlAlias) where
  convert (I.TlAlias  g v dll tlm na l a) = convertAliasee a >>= return . (A.TlAlias (unspecializeGlobalId g) v dll tlm na l)
  
instance Conversion (I.TlUnamedMd I.Gname) (Rm A.TlUnamedMd) where  
  convert x = let (I.TlUnamedMd s tv) = unspecializeUnamedMd x
              in Md.liftM (A.TlUnamedMd s) (convert tv) 

instance Conversion I.TlNamedMd (Rm A.TlNamedMd) where  
  convert (I.TlNamedMd m ns) = Md.liftM (A.TlNamedMd m) (mapM convert ns)
                               
instance Conversion (I.TlDeclare I.Gname) (Rm A.TlDeclare) where
  convert (I.TlDeclare f) = convert f >>= return . A.TlDeclare
  
instance Conversion I.TlDeclareIntrinsic (Rm A.TlDeclare) where
  convert f = convert (unspecializeDeclareIntrinsic f) 

instance Conversion (I.TlDefine I.Gname ()) (Rm A.TlDefine) where
  convert (I.TlDefine f elbl g) = 
    withFunName (I.fi_fun_name f) $ 
    do { (bl, bm) <- graphToBlocks g
       ; fa <- convert f
       ; elbla <- convert elbl
       ; let entryblk = case M.lookup elbla bm of
               Just x -> x
               Nothing -> error $ "irrefutable: entry block " ++ show elbl ++ " does not exist."
       ; let bs'' = entryblk:(filter (\x -> x /= entryblk) bl) 
       ; return $ A.TlDefine fa bs''
       } -- TODO: this method will NOT emit the new nodes generated by hoopl passes, it should be fixed ASAP.

                             
instance Conversion (I.TlIntrinsic I.Gname) (Rm A.TlGlobal) where
  convert x = convert (unspecializeTlIntrinsics x)

instance Conversion (I.TlGlobal I.Gname) (Rm A.TlGlobal) where
  convert x = case x of
    (I.TlGlobalDtype a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) ->
      do { a10a <- maybeM convert a10
         ; return $ A.TlGlobal (Just $ unspecializeGlobalId a1) a2 a3 a4 a5 a6 (fmap (tconvert ()) a7) 
           a8 a8a (tconvert () a9) a10a a11 (fmap convert_Comdat a12) a13
         }
    (I.TlGlobalOpaque a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) ->
      do { a10a <- maybeM convert a10
         ; return $ A.TlGlobal (Just $ unspecializeGlobalId a1) a2 a3 a4 a5 a6 (fmap (tconvert ()) a7) 
           a8 a8a (tconvert () a9) a10a a11 (fmap convert_Comdat a12) a13
         }
      
convert_Comdat :: I.Comdat I.Gname -> A.Comdat      
convert_Comdat (I.Comdat n) = A.Comdat (fmap unspecializeDollarId n)
    
instance Conversion I.TlTypeDef (Rm A.TlTypeDef) where    
  convert x = case x of
    (I.TlFunTypeDef lid t) -> return (A.TlTypeDef (unLid lid) (tconvert () t))
    (I.TlDatTypeDef lid t) -> return (A.TlTypeDef (unLid lid) (tconvert () t))
    (I.TlOpqTypeDef lid t) -> return (A.TlTypeDef (unLid lid) (tconvert () t))

instance Conversion I.TlDepLibs (Rm A.TlDepLibs) where  
  convert (I.TlDepLibs s) = return (A.TlDepLibs s)
  
instance Conversion I.TlUnamedType (Rm A.TlUnamedType) where  
  convert (I.TlUnamedType i t) = return (A.TlUnamedType i (tconvert () t))
  
instance Conversion I.TlModuleAsm (Rm A.TlModuleAsm) where  
  convert (I.TlModuleAsm s) = return (A.TlModuleAsm s)

instance Conversion I.TlAttribute (Rm A.TlAttribute) where
  convert (I.TlAttribute n l) = return (A.TlAttribute n l)
  
instance Conversion (I.TlComdat I.Gname) (Rm A.TlComdat) where  
  convert (I.TlComdat l s) = return (A.TlComdat (unspecializeDollarId l) s)

instance Conversion I.TlComment (Rm [String]) where  
  convert (I.TlComment a) = return (I.commentize a)

type Pblock = (A.BlockLabel, [A.PhiInstWithDbg], [A.ComputingInstWithDbg])

getLabelId :: A.BlockLabel -> A.LabelId
getLabelId (A.ImplicitBlockLabel _) = error "ImplicitBlockLabel should be normalized"
getLabelId (A.ExplicitBlockLabel l) = l

isComment :: A.ComputingInstWithDbg -> Bool
isComment x = case x of
  A.ComputingInstWithComment _ -> True
  _ -> False

data PhiInstWithDbg = PhiInstWithDbg (I.Pinst I.Gname) [I.Dbg I.Gname] deriving (Eq, Ord, Show)
data CInstWithDbg = CInstWithDbg (I.Cinst I.Gname) [I.Dbg I.Gname] deriving (Eq, Ord, Show)
data MInstWithDbg = MInstWithDbg (I.Minst I.Gname) [I.Dbg I.Gname] deriving (Eq, Ord, Show)
data TerminatorInstWithDbg = TerminatorInstWithDbg (I.Tinst I.Gname) [I.Dbg I.Gname] deriving (Eq, Ord, Show)

convertNode :: I.Node I.Gname () e x -> Rm ([A.Block], M.Map A.LabelId A.Block, Maybe Pblock)
               -> Rm ([A.Block], M.Map A.LabelId A.Block, Maybe Pblock)
convertNode (I.Lnode a) p = do { (bl, bs, Nothing) <- p
                               ; a' <- convert_to_BlockLabel a
                               ; return (bl, bs, Just (a', [], []))
                               }
convertNode (I.Pnode a dbgs) p = do { (bl, bs, pblk) <- p
                                    ; case pblk of
                                      Just (pb, phis, l) | all isComment l -> 
                                        do { a' <- convert (PhiInstWithDbg a dbgs)
                                           ; return (bl, bs, Just (pb, a':phis, l))
                                           }
                                      _ -> errorLoc FLC $ "irrefutable:unexpected case " ++ show pblk
                                    }
convertNode (I.Cnode a dbgs) p = do { (bl, bs, Just (pb, phis, cs)) <- p
                                    ; a' <- convert (CInstWithDbg a dbgs)
                                    ; return (bl, bs, Just (pb, phis, a':cs))
                                    }
convertNode (I.Mnode a dbgs) p = do { (bl, bs, Just (pb, phis, cs)) <- p
                                    ; a' <- convert (MInstWithDbg a dbgs)
                                    ; return (bl, bs, Just (pb, phis, a':cs))
                                    }
convertNode (I.Comment a) p = do { (bl, bs, Just (pb, phis, cs)) <- p
                                 ; let comments = reverse $ fmap A.ComputingInstWithComment (I.commentize a)
                                 ; return (bl, bs, Just (pb, phis, comments ++ cs))
                                 }
convertNode (I.Tnode a dbgs) p = do { (bl, bs, pb) <- p
                                    ; a' <- convert (TerminatorInstWithDbg a dbgs)
                                    ; case pb of
                                      Nothing -> error "irrefutable"
                                      Just (l, phis, cs) ->
                                        let blk = A.Block l (reverse phis) (reverse cs) a'
                                        in return (blk:bl, M.insert (getLabelId l) blk bs, Nothing)
                                    }
convertNode (I.Enode _ _) _ = error "irrefutable:Enode should be converted to LLVM node"
  
graphToBlocks :: H.Graph (I.Node I.Gname ()) H.C H.C -> Rm ([A.Block], M.Map A.LabelId A.Block)
graphToBlocks g = do { (bl, bs, Nothing) <- H.foldGraphNodes convertNode g (return ([], M.empty, Nothing))
                     ; return (reverse bl, bs)
                     }

toplevel2Ast :: I.Toplevel I.Gname () -> Rm A.Toplevel
toplevel2Ast (I.ToplevelAlias g) = Md.liftM A.ToplevelAlias (convert g)
toplevel2Ast (I.ToplevelUnamedMd s) = Md.liftM (A.ToplevelUnamedMd) (convert s)
toplevel2Ast (I.ToplevelNamedMd m) = Md.liftM A.ToplevelNamedMd (convert m) 
toplevel2Ast (I.ToplevelDeclare f) = Md.liftM A.ToplevelDeclare (convert f)
toplevel2Ast (I.ToplevelDeclareIntrinsic f) = Md.liftM A.ToplevelDeclare (convert f)
toplevel2Ast (I.ToplevelDefine f) = Md.liftM A.ToplevelDefine (convert f)
toplevel2Ast (I.ToplevelGlobal s) = Md.liftM A.ToplevelGlobal (convert s)
toplevel2Ast (I.ToplevelTypeDef t) = Md.liftM A.ToplevelTypeDef (convert t)
toplevel2Ast (I.ToplevelDepLibs qs) = Md.liftM A.ToplevelDepLibs (convert qs)
toplevel2Ast (I.ToplevelUnamedType i) = Md.liftM A.ToplevelUnamedType (convert i)
toplevel2Ast (I.ToplevelModuleAsm q) = Md.liftM A.ToplevelModuleAsm (convert q)
toplevel2Ast (I.ToplevelComdat l) = Md.liftM A.ToplevelComdat (convert l)
toplevel2Ast (I.ToplevelAttribute n) = Md.liftM A.ToplevelAttribute (convert n)
toplevel2Ast (I.ToplevelIntrinsic n) = Md.liftM A.ToplevelGlobal (convert n)
toplevel2Ast (I.ToplevelComment n) = Md.liftM A.ToplevelComment (convert n)

hirToAsm ::  M.Map (I.Gname, H.Label) A.LabelId -> I.SpecializedModule I.Gname () -> A.Module
hirToAsm iLm (I.SpecializedModule (Target dlm) (I.Module ts)) = 
  let (A.Module tl) = runReader (Md.liftM A.Module (mapM toplevel2Ast ts)) 
                      (ReaderData iLm (I.Gname (errorLoc FLC $ "<fatal error>")))
      ls = toLayoutSpec dlm
      tt = toTriple dlm
  in A.Module ((A.ToplevelDataLayout $ A.TlDataLayout $ A.DataLayout ls):(A.ToplevelTriple $ A.TlTriple tt):tl)
