{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Llvm.Data.Conversion.IrAstConversion(irToAst) where

#define FLC  (I.FileLoc $(I.srcLoc))

import qualified Compiler.Hoopl as H
import qualified Control.Monad as Md
import qualified Data.Map as M
import qualified Llvm.Data.Ast as A
import qualified Llvm.Data.Ir as I
import Llvm.Util.Monadic (maybeM, pairM)
import Llvm.Data.Conversion.TypeConversion
import Control.Monad.Reader
import Llvm.Data.Conversion.IntrinsicsSpecialization


class Conversion l1 l2 | l1 -> l2 where
  convert :: l1 -> l2

data ReaderData = ReaderData { mp :: (M.Map (A.GlobalId, H.Label) A.LabelId)
                             , funname :: A.GlobalId
                             }
type Rm = Reader ReaderData

withFunName :: A.GlobalId -> Rm a -> Rm a
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
                 
mkConversion :: (A.ConvertOp, A.Type, I.Const, A.Type) -> Rm A.Const
mkConversion (op, t1, u, dt1) = do { u1 <- convert u
                                   ; return $ A.C_conv $ A.Conversion op (A.Typed t1 u1) dt1                                   
                                   }

instance (Conversion v1 (Rm v2)) => Conversion (I.GetElementPtr I.ScalarB v1) (Rm (A.GetElementPtr v2)) where
  convert (I.GetElementPtr b u us) = do { ua <- convert u
                                        ; usa <- mapM convert us
                                        ; return $ A.GetElementPtr b (A.Pointer ua) usa
                                        }
          
instance (Conversion v1 (Rm v2)) => Conversion (I.GetElementPtr I.VectorB v1) (Rm (A.GetElementPtr v2)) where
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
  convert (I.InsertElement u1 u2 u3) = Md.liftM3 A.InsertElement (convert u1) (convert u2) (convert u3)





instance Conversion I.Const (Rm A.Const) where
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
    I.C_globalAddr s -> return $ A.C_simple $ A.CpGlobalAddr s
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


    (I.C_struct b fs) -> Md.liftM (A.C_complex . (A.Cstruct b)) (mapM convert fs)
    (I.C_vector fs) -> Md.liftM (A.C_complex . A.Cvector) (mapM convert fs)
    (I.C_array fs) -> Md.liftM (A.C_complex . A.Carray) (mapM convert fs)
    (I.C_vectorN n fs) -> do { v <- convert fs
                             ; return  (A.C_complex $ A.Cvector $ (fmap (\_ -> v) [1..n]))
                             }
    (I.C_arrayN n fs) -> do { v <- convert fs
                            ; return (A.C_complex $ A.Carray $ (fmap (\_ -> v) [1..n]))
                            }
--    I.C_localId a -> return $ A.C_localId a
    I.C_labelId a -> Md.liftM A.C_labelId (convert a)
    I.C_block g a -> do { a' <- convert_to_PercentLabel a
                        ; return $ A.C_blockAddress g a'
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


    I.C_add_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) (Md.liftM2 (A.IbinExpr A.Add (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_sub_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) (Md.liftM2 (A.IbinExpr A.Sub (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_mul_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) (Md.liftM2 (A.IbinExpr A.Mul (cnowrap nw) (tconvert () t)) (convert u1) (convert u2))
    I.C_udiv_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Udiv (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_sdiv_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Sdiv (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_urem_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Urem [] (tconvert () t)) (convert u1) (convert u2)
    I.C_srem_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Srem [] (tconvert () t)) (convert u1) (convert u2)
    I.C_shl_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Shl (cnowrap nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_lshr_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Lshr (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_ashr_V nw t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Ashr (cexact nw) (tconvert () t)) (convert u1) (convert u2)
    I.C_and_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.And [] (tconvert () t)) (convert u1) (convert u2)
    I.C_or_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Or [] (tconvert () t)) (convert u1) (convert u2)
    I.C_xor_V t u1 u2 -> Md.liftM (A.C_binexp . A.Ie) $ Md.liftM2 (A.IbinExpr A.Xor [] (tconvert () t)) (convert u1) (convert u2)

    I.C_fadd fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fadd fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fsub fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fsub fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fmul fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fmul fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fdiv fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fdiv fg (tconvert () t)) (convert u1) (convert u2)
    I.C_frem fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Frem fg (tconvert () t)) (convert u1) (convert u2)

    I.C_fadd_V fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fadd fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fsub_V fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fsub fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fmul_V fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fmul fg (tconvert () t)) (convert u1) (convert u2)
    I.C_fdiv_V fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Fdiv fg (tconvert () t)) (convert u1) (convert u2)
    I.C_frem_V fg t u1 u2 -> Md.liftM (A.C_binexp . A.Fe) $ Md.liftM2 (A.FbinExpr A.Frem fg (tconvert () t)) (convert u1) (convert u2)

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


instance Conversion I.MdVar (Rm A.MdVar) where
    convert (I.MdVar s) = return $ A.MdVar s

instance Conversion I.MdNode (Rm A.MdNode) where
    convert (I.MdNode s) = return $ A.MdNode s

instance Conversion I.MetaConst (Rm A.MetaConst) where
    convert (I.McStruct c) = mapM convert c >>= return . A.McStruct
    convert (I.McString s) = return $ A.McString s
    convert (I.McMn n) = convert n >>= return . A.McMn
    convert (I.McMv n) = convert n >>= return . A.McMv
    convert (I.McRef i) = return $ A.McRef i
    convert (I.McSimple sc) = Md.liftM A.McSimple (convert sc)

instance Conversion I.MetaKindedConst (Rm A.MetaKindedConst) where
  convert x = case x of
    (I.MetaKindedConst mk mc) -> Md.liftM (A.MetaKindedConst (tconvert () mk)) (convert mc)
    I.UnmetaKindedNull -> return A.UnmetaKindedNull

{-
instance Conversion I.FunName (Rm A.FunName) where
    convert (I.FunNameGlobal g) = return $ A.FunNameGlobal g
    convert (I.FunNameString s) = return $ A.FunNameString s
-}

instance Conversion I.Value (Rm A.Value) where
    convert (I.Val_ssa a) = return $ A.Val_local a
    convert (I.Val_const a) = Md.liftM A.Val_const (convert a)

instance Conversion I.CallSiteType (Rm A.Type) where
  convert t = case t of
    I.CallSiteRet x -> return $ tconvert () x
    I.CallSiteFun ft as -> return $ tconvert () (I.Tpointer (I.ucast ft) as)
    
instance Conversion I.FunPtr (Rm A.FunName) where
  convert x = case x of
    I.Fun_null -> return A.FunName_null
    I.Fun_undef -> return A.FunName_undef    
    I.FunId g -> return $ A.FunNameGlobal (A.GolG g)
    I.FunSsa l -> return $ A.FunNameGlobal (A.GolL l)
    I.FunIdBitcast (I.T st c) dt -> do { tv1 <- convert (I.T st c)
                                       ; return $ A.FunNameBitcast tv1 (tconvert () dt)
                                       }
    I.FunIdInttoptr (I.T st c) dt -> do { tv1 <- convert (I.T st c)
                                        ; return $ A.FunNameInttoptr tv1 (tconvert () dt)
                                        }

instance Conversion I.CallSite (Rm A.CallSite) where
  convert  (I.CsFun cc pa t fn aps fa) = do { fna <- convert fn
                                            ; apsa <- mapM convert aps
                                            ; ta <- convert t
                                            ; return $ A.CsFun cc pa ta fna apsa fa
                                            }
  convert  (I.CsAsm t mse mas dia s1 s2 aps fa) = 
    do { apsa <- mapM convert aps
       ; ta <- convert t
       ; return $ A.CsAsm ta mse mas dia s1 s2 apsa fa
       }

instance Conversion I.Clause (Rm A.Clause) where
    convert (I.Catch tv) = convert tv >>= \tv' -> return $ A.Catch tv'
    convert (I.Filter tc) = convert tc >>= \tc' -> return $ A.Filter tc'
    convert (I.CcoS tc) = convert tc >>= return . A.Cco
    convert (I.CcoV tc) = convert tc >>= return . A.Cco    

instance Conversion I.GlobalOrLocalId (Rm A.GlobalOrLocalId) where
    convert g = return g

{-
instance Conversion I.PersFn (Rm A.PersFn) where
    convert (I.PersFnId s) = return $ A.PersFnId $ s
    convert (I.PersFnCastS c) = convert c >>= return . A.PersFnCast
    convert (I.PersFnCastV c) = convert c >>= return . A.PersFnCast    
    convert (I.PersFnUndef) = return $ A.PersFnUndef
    convert (I.PersFnNull) = return $ A.PersFnNull
    convert (I.PersFnConst c) = Md.liftM A.PersFnConst (convert c)
-}


instance Conversion I.Minst (Rm A.ComputingInst) where
  convert (I.Minst cst fn params lhs) = 
    do { fna <- convert (I.FunId fn)
       ; cst0 <- convert cst
       ; apa <- mapM convert params
       ; return $ A.ComputingInst lhs $ A.Call A.TcNon $ A.CsFun Nothing [] cst0 fna apa []
       }

instance Conversion I.Cinst (Rm A.ComputingInst) where
  convert cinst = case maybe cinst id (unspecializeIntrinsics cinst) of 
    I.I_alloca mar t mtv ma lhs -> do { mtva <- maybeM convert mtv 
                                      ; return $ A.ComputingInst (Just lhs) $ A.RmO $ A.Alloca mar (tconvert () t) mtva ma
                                      }
    I.I_load atom tv aa nonterm invr nonull lhs -> do { tva <- convert tv 
                                                      ; return $ A.ComputingInst (Just lhs) $ A.RmO $ A.Load atom (A.Pointer tva) aa nonterm invr nonull
                                                      }
    I.I_loadatomic atom v tv aa lhs -> do { tva <- convert tv 
                                          ; return $ A.ComputingInst (Just lhs) $ A.RmO $ A.LoadAtomic atom v (A.Pointer tva) aa
                                          }
    I.I_store atom tv1 tv2 aa nonterm -> do { tv1a <- convert tv1
                                            ; tv2a <- convert tv2
                                            ; return $ A.ComputingInst Nothing (A.RmO $ A.Store atom tv1a (A.Pointer tv2a) aa nonterm)
                                            }
    I.I_storeatomic atom v tv1 tv2 aa -> do { tv1a <- convert tv1
                                            ; tv2a <- convert tv2
                                            ; return $ A.ComputingInst Nothing (A.RmO $ A.StoreAtomic atom v tv1a (A.Pointer tv2a) aa)
                                            }
    I.I_cmpxchg_I wk b1 tv1 tv2 tv3 b2 sord ford lhs-> do { tv1a <- convert tv1
                                                           ; tv2a <- convert tv2
                                                           ; tv3a <- convert tv3
                                                           ; return $ A.ComputingInst (Just lhs) (A.RmO $ A.CmpXchg wk b1 (A.Pointer tv1a) tv2a tv3a b2 sord ford)
                                                           }
    I.I_cmpxchg_F wk b1 tv1 tv2 tv3 b2 sord ford lhs->
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; tv3a <- convert tv3
         ; return $ A.ComputingInst (Just lhs) (A.RmO $ A.CmpXchg wk b1 (A.Pointer tv1a) tv2a tv3a b2 sord ford)
         }    
    I.I_cmpxchg_P wk b1 tv1 tv2 tv3 b2 sord ford lhs->
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; tv3a <- convert tv3
         ; return $ A.ComputingInst (Just lhs) (A.RmO $ A.CmpXchg wk b1 (A.Pointer tv1a) tv2a tv3a b2 sord ford)
         }    
    I.I_atomicrmw b1 op tv1 tv2 b2 mf lhs-> 
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; return $ A.ComputingInst (Just lhs) (A.RmO $ A.AtomicRmw b1 op (A.Pointer tv1a) tv2a b2 mf)
         }
    I.I_fence b fo -> return $ A.ComputingInst Nothing $ A.RmO $ A.Fence b fo 
    I.I_va_arg tv t lhs-> 
      do { tv1 <- convert tv
         ; return $ A.ComputingInst (Just lhs) $ A.RvA $ A.VaArg tv1 (tconvert () t)
         }
      {-
    I.I_llvm_va_start v -> 
      do { let t1 = tconvert () (I.ptr0 I.i8)
         ; va <- convert v
         ; return $ A.ComputingInst Nothing $ A.Call A.TcNon $ A.CsFun Nothing [] A.Tvoid (A.FunNameGlobal $ A.GolG $ A.GlobalIdAlphaNum "llvm.va_start") 
           [A.ActualParamData t1 [] Nothing va []] []
         }      
    I.I_llvm_va_end v -> 
      do { let t1 = tconvert () (I.ptr0 I.i8) -- t
         ; va <- convert v 
         ; return $ A.ComputingInst Nothing $ A.Call A.TcNon $ A.CsFun Nothing [] A.Tvoid (A.FunNameGlobal $ A.GolG $ A.GlobalIdAlphaNum "llvm.va_end") 
           [A.ActualParamData t1 [] Nothing va []] []
         }      -}
    I.I_landingpad t1 t2 pf b cs lhs-> 
      do { pfa <- convert pf
         ; csa <- mapM convert cs
         ; return $ A.ComputingInst (Just lhs) (A.RlP $ A.LandingPad (tconvert () t1) (tconvert () t2) pfa b csa)
         }
    I.I_extractelement_I tv1 tv2 lhs-> 
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; return $ A.ComputingInst (Just lhs) (A.ReE $ A.ExtractElement tv1a tv2a)
         }
    I.I_extractelement_F tv1 tv2 lhs-> 
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; return $ A.ComputingInst (Just lhs) (A.ReE $ A.ExtractElement tv1a tv2a)
         }
    I.I_extractelement_P tv1 tv2 lhs-> 
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; return $ A.ComputingInst (Just lhs) (A.ReE $ A.ExtractElement tv1a tv2a)
         }    
    I.I_extractvalue tv1 idx lhs-> 
      do { tv1a <- convert tv1
         ; return $ A.ComputingInst (Just lhs) (A.ReV $ A.ExtractValue tv1a idx)
         }
    I.I_getelementptr b ptr idx lhs-> 
      do { ptra <- convert ptr
         ; idxa <- mapM convert idx
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.EgEp $ A.GetElementPtr b (A.Pointer ptra) idxa) 
         }
    I.I_getelementptr_V b ptr idx lhs ->
      do { ptra <- convert ptr
         ; idxa <- mapM convert idx
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.EgEp $ A.GetElementPtr b (A.Pointer ptra) idxa) 
         }    
    I.I_icmp op t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.EiC $ A.Icmp op (tconvert () t) v1a v2a)
         }
    I.I_icmp_V op t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.EiC $ A.Icmp op (tconvert () t) v1a v2a)
         }    
    I.I_fcmp op t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.EfC $ A.Fcmp op (tconvert () t) v1a v2a)
         }
    I.I_fcmp_V op t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.EfC $ A.Fcmp op (tconvert () t) v1a v2a)
         }
    I.I_add n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Add (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_sub n t v1 v2 lhs -> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Sub (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_mul n t v1 v2 lhs -> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Mul (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_udiv n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Udiv (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_sdiv n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Sdiv (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_urem t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Urem [] (tconvert () t) v1a v2a)
         }
    I.I_srem t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Srem [] (tconvert () t) v1a v2a)
         }
    I.I_shl n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Shl (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_lshr n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Lshr (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_ashr n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Ashr (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_and t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2 
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.And [] (tconvert () t) v1a v2a)
         }
    I.I_or t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Or [] (tconvert () t) v1a v2a)
         }
    I.I_xor t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Xor [] (tconvert () t) v1a v2a)
         }
    I.I_add_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Add (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_sub_V n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Sub (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_mul_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Mul (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_udiv_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Udiv (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_sdiv_V n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Sdiv (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_urem_V t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Urem [] (tconvert () t) v1a v2a)
         }
    I.I_srem_V t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Srem [] (tconvert () t) v1a v2a)
         }
    I.I_shl_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Shl (cnowrap n) (tconvert () t) v1a v2a)
         }
    I.I_lshr_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Lshr (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_ashr_V n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Ashr (cexact n) (tconvert () t) v1a v2a)
         }
    I.I_and_V t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.And [] (tconvert () t) v1a v2a)
         }
    I.I_or_V t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Or [] (tconvert () t) v1a v2a)
         }
    I.I_xor_V t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Ie $ A.IbinExpr A.Xor [] (tconvert () t) v1a v2a)
         }    
    I.I_fadd n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fsub n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fmul n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fdiv n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_frem n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fadd_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fsub_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fmul_V n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_fdiv_V n t v1 v2 lhs-> 
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_frem_V n t v1 v2 lhs->
      do { v1a <- convert v1
         ; v2a <- convert v2
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Eb $ A.Fe $ A.FbinExpr A.Fadd n (tconvert () t) v1a v2a)
         }
    I.I_trunc tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Trunc tva (tconvert () dt)) 
         }
    I.I_zext tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Zext tva (tconvert () dt)) 
         }    
    I.I_sext tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Sext tva (tconvert () dt)) 
         }    
    I.I_fptrunc tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpTrunc tva (tconvert () dt)) 
         }    
    I.I_fpext tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpExt tva (tconvert () dt)) 
         }    
    I.I_fptoui tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpToUi tva (tconvert () dt)) 
         }    
    I.I_fptosi tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpToSi tva (tconvert () dt)) 
         }    
    I.I_uitofp tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.UiToFp tva (tconvert () dt)) 
         }    
    I.I_sitofp tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.SiToFp tva (tconvert () dt)) 
         }    
    I.I_ptrtoint tv dt lhs ->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.PtrToInt tva (tconvert () dt)) 
         }    
    I.I_inttoptr tv dt lhs -> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.IntToPtr tva (tconvert () dt)) 
         }    
    I.I_bitcast tv dt lhs -> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Bitcast tva (tconvert () dt)) 
         }    
    I.I_bitcast_D tv dt lhs -> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Bitcast tva (tconvert () dt)) 
         }      
    I.I_addrspacecast tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.AddrSpaceCast tva (tconvert () dt)) 
         }    
    I.I_trunc_V tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Trunc tva (tconvert () dt)) 
         }
    I.I_zext_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Zext tva (tconvert () dt)) 
         }    
    I.I_sext_V tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.Sext tva (tconvert () dt)) 
         }    
    I.I_fptrunc_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpTrunc tva (tconvert () dt)) 
         }    
    I.I_fpext_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpExt tva (tconvert () dt)) 
         }    
    I.I_fptoui_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpToUi tva (tconvert () dt)) 
         }    
    I.I_fptosi_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.FpToSi tva (tconvert () dt)) 
         }    
    I.I_uitofp_V tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.UiToFp tva (tconvert () dt)) 
         }    
    I.I_sitofp_V tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.SiToFp tva (tconvert () dt)) 
         }    
    I.I_ptrtoint_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.PtrToInt tva (tconvert () dt)) 
         }    
    I.I_inttoptr_V tv dt lhs->
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.IntToPtr tva (tconvert () dt)) 
         }    
    I.I_addrspacecast_V tv dt lhs-> 
      do { tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Ec $ A.Conversion A.AddrSpaceCast tva (tconvert () dt)) 
         }    
    I.I_select_I cnd t f lhs->
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_select_F cnd t f lhs->
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_select_P cnd t f lhs->
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_select_First cnd t f lhs->
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_select_VI cnd t f lhs-> 
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_select_VF cnd t f lhs-> 
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_select_VP cnd t f lhs-> 
      do { cnda <- convert cnd
         ; ta <- convert t
         ; fa <- convert f
         ; return $ A.ComputingInst (Just lhs) (A.Re $ A.Es $ A.Select cnda ta fa) 
         }
    I.I_insertelement_I vtv tv idx lhs-> 
      do { vtva <- convert vtv
         ; tva <- convert tv
         ; idxa <- convert idx
         ; return $ A.ComputingInst (Just lhs) (A.RiE $ A.InsertElement vtva tva idxa) 
         }
    I.I_insertelement_F vtv tv idx lhs-> 
      do { vtva <- convert vtv
         ; tva <- convert tv
         ; idxa <- convert idx
         ; return $ A.ComputingInst (Just lhs) (A.RiE $ A.InsertElement vtva tva idxa) 
         }
    I.I_insertelement_P vtv tv idx lhs-> 
      do { vtva <- convert vtv
         ; tva <- convert tv
         ; idxa <- convert idx
         ; return $ A.ComputingInst (Just lhs) (A.RiE $ A.InsertElement vtva tva idxa) 
         }
    I.I_shufflevector_I tv1 tv2 tv3 lhs->
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; tv3a <- convert tv3
         ; return $ A.ComputingInst (Just lhs) (A.RsV $ A.ShuffleVector tv1a tv2a tv3a) 
         }
    I.I_shufflevector_F tv1 tv2 tv3 lhs->
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; tv3a <- convert tv3
         ; return $ A.ComputingInst (Just lhs) (A.RsV $ A.ShuffleVector tv1a tv2a tv3a) 
         }
    I.I_shufflevector_P tv1 tv2 tv3 lhs-> 
      do { tv1a <- convert tv1
         ; tv2a <- convert tv2
         ; tv3a <- convert tv3
         ; return $ A.ComputingInst (Just lhs) (A.RsV $ A.ShuffleVector tv1a tv2a tv3a) 
         }
    I.I_insertvalue vtv tv idx lhs-> 
      do { vtva <- convert vtv
         ; tva <- convert tv
         ; return $ A.ComputingInst (Just lhs) $ A.RiV $ A.InsertValue vtva tva idx 
         }
    I.I_call_fun tc cc pa cstype fn ap fna lhs-> 
      do { csa <- convert (I.CsFun cc pa cstype fn ap fna)
         ; return $ A.ComputingInst lhs $ A.Call tc csa 
         }
    I.I_call_asm tc t dia b1 b2 qs1 qs2 as fa lhs -> 
      do { csa <- convert (I.CsAsm t dia b1 b2 qs1 qs2 as fa) 
         ; return $ A.ComputingInst lhs $ A.Call tc csa 
         } 
      {-
    I.I_llvm_memcpy memLen tv1 tv2 tv3 tv4 tv5 -> 
      do { (A.Typed t1 v1) <- convert tv1
         ; (A.Typed t2 v2) <- convert tv2
         ; (A.Typed t3 v3) <- convert tv3
         ; (A.Typed t4 v4) <- convert tv4
         ; (A.Typed t5 v5) <- convert tv5
         ; let nm = case memLen of
                 I.MemLenI32 -> "llvm.memcpy.p0i8.p0i8.i32"
                 I.MemLenI64 -> "llvm.memcpy.p0i8.p0i8.i64"
         ; return $ A.ComputingInst Nothing $ A.Call A.TcNon $ A.CsFun Nothing [] A.Tvoid (A.FunNameGlobal $ A.GolG $ A.GlobalIdAlphaNum nm) 
           [A.ActualParamData t1 [] Nothing v1 []
           ,A.ActualParamData t2 [] Nothing v2 []
           ,A.ActualParamData t3 [] Nothing v3 []
           ,A.ActualParamData t4 [] Nothing v4 []
           ,A.ActualParamData t5 [] Nothing v5 []
           ] []
         }   -}
                                      
instance Conversion I.ActualParam (Rm A.ActualParam) where
  convert x = case x of
    (I.ActualParamData t pa1 ma v pa2) -> do { va <- convert v 
                                             ; return $ A.ActualParamData (tconvert () t) pa1 ma va pa2
                                             }
    (I.ActualParamLabel t pa1 ma v pa2) -> do { va <- convert_to_PercentLabel v 
                                              ; return $ A.ActualParamLabel (tconvert () t) pa1 ma va pa2
                                              }
--    (I.ActualParamMeta mc) -> Md.liftM (A.ActualParamMeta) (convert mc)

instance Conversion I.MetaParam (Rm A.ActualParam) where
  convert x = case x of
    (I.MetaParamData t pa1 ma v pa2) -> do { va <- convert v 
                                             ; return $ A.ActualParamData (tconvert () t) pa1 ma va pa2
                                             }
                                        {-
    (I.ActualParamLabel t pa1 ma v pa2) -> do { va <- convert v 
                                              ; return $ A.ActualParamData (tconvert () t) pa1 ma va pa2
                                              } -}
    (I.MetaParamMeta mc) -> Md.liftM (A.ActualParamMeta) (convert mc)


instance Conversion I.Aliasee (Rm A.Aliasee) where
  convert (I.AtV tv) = Md.liftM A.AtV (convert tv)
  convert (I.Ac a) = Md.liftM A.Ac (convert a)
  convert (I.AcV a) = Md.liftM A.Ac (convert a)  
  convert (I.Agep a) = Md.liftM A.AgEp (convert a)
  convert (I.AgepV a) = Md.liftM A.AgEp (convert a)  

instance Conversion I.Prefix (Rm A.Prefix) where
  convert (I.Prefix n) = Md.liftM A.Prefix (convert n)

instance Conversion I.Prologue (Rm A.Prologue) where
  convert (I.Prologue n) = Md.liftM A.Prologue (convert n)

instance Conversion I.TypedConstOrNull (Rm A.TypedConstOrNull) where
  convert x = case x of
    I.TypedConst tv -> Md.liftM A.TypedConst (convert tv)
    I.UntypedNull -> return A.UntypedNull

instance Conversion I.FunctionPrototype (Rm A.FunctionPrototype) where
    convert (I.FunctionPrototype f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f10a f11 f12 f13 f14) =
      do { f13' <- convert f13
         ; f14' <- convert f14
         ; return $ A.FunctionPrototype f0 f1 f2 f3 f4 (tconvert () f5) 
           f6 (tconvert () f7) f8 f9 f10 f10a f11 f12 f13' f14'
         }


instance Conversion I.Pinst (Rm A.PhiInst) where
  convert (I.Pinst t branches mg) = 
    Md.liftM (A.PhiInst (Just mg) (tconvert () t)) 
    (mapM (pairM convert convert_to_PercentLabel) branches)

instance Conversion I.Tinst (Rm A.TerminatorInst) where
  convert (I.T_ret_void) = return A.RetVoid
  convert (I.T_return tvs) = Md.liftM A.Return (mapM convert tvs)
  convert (I.T_br t) = Md.liftM A.Br (convert_to_TargetLabel t)
  convert (I.T_cbr cnd t f) = Md.liftM3 A.Cbr (convert cnd) (convert_to_TargetLabel t) (convert_to_TargetLabel f)
  convert (I.T_indirectbr cnd bs) = Md.liftM2 A.IndirectBr (convert cnd) (mapM convert_to_TargetLabel bs)
  convert (I.T_switch (cnd,d) cases) = Md.liftM3 A.Switch (convert cnd) (convert_to_TargetLabel d) 
                                       (mapM (pairM convert convert_to_TargetLabel) cases)
  convert (I.T_invoke conv ra fty ptr aps fa  t f mg) = 
    Md.liftM3 (A.Invoke mg) (convert (I.CsFun conv ra fty ptr aps fa))
    (convert_to_TargetLabel t) (convert_to_TargetLabel f)
  convert (I.T_invoke_asm rt se as dia dq1 dq2 aps fa t f lhs) =
    Md.liftM3 (A.Invoke lhs) (convert (I.CsAsm rt se as dia dq1 dq2 aps fa))
    (convert_to_TargetLabel t) (convert_to_TargetLabel f)
  convert (I.T_resume tv) = Md.liftM A.Resume (convert tv)
  convert I.T_unreachable = return A.Unreachable
  convert I.T_unwind = return A.Unwind

instance Conversion I.Dbg (Rm A.Dbg) where
  convert (I.Dbg mv mc) = Md.liftM2 A.Dbg (convert mv) (convert mc)

instance Conversion PhiInstWithDbg (Rm A.PhiInstWithDbg) where
  convert (PhiInstWithDbg ins dbgs) = Md.liftM2 A.PhiInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion CInstWithDbg (Rm A.ComputingInstWithDbg) where
  convert (CInstWithDbg ins dbgs) = Md.liftM2 A.ComputingInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion MInstWithDbg (Rm A.ComputingInstWithDbg) where
  convert (MInstWithDbg ins dbgs) = Md.liftM2 A.ComputingInstWithDbg (convert ins) (mapM convert dbgs)

instance Conversion TerminatorInstWithDbg (Rm A.TerminatorInstWithDbg) where
  convert (TerminatorInstWithDbg term dbgs) = Md.liftM2 A.TerminatorInstWithDbg (convert term) (mapM convert dbgs)

    
instance Conversion I.TlTriple (Rm A.TlTriple) where
  convert (I.TlTriple x) = return (A.TlTriple x)

instance Conversion I.TlDataLayout (Rm A.TlDataLayout) where
  convert (I.TlDataLayout x) = return (A.TlDataLayout x)

instance Conversion I.TlAlias (Rm A.TlAlias) where  
  convert (I.TlAlias  g v dll tlm na l a) = convert a >>= return . (A.TlAlias g v dll tlm na l)

instance Conversion I.TlDbgInit (Rm A.TlDbgInit) where
  convert (I.TlDbgInit s i) = return (A.TlDbgInit s i)
  
instance Conversion I.TlStandaloneMd (Rm A.TlStandaloneMd) where  
  convert (I.TlStandaloneMd s tv) = convert tv >>= return . (A.TlStandaloneMd s)

instance Conversion I.TlNamedMd (Rm A.TlNamedMd) where  
  convert (I.TlNamedMd m ns) = do { m' <- convert m
                                  ; ns' <- mapM convert ns
                                  ; return $ A.TlNamedMd m' ns'
                                  }
                               
instance Conversion I.TlDeclare (Rm A.TlDeclare) where                               
  convert (I.TlDeclare f) = convert f >>= return . A.TlDeclare
  
instance Conversion (I.TlDefine a) (Rm A.TlDefine) where
  convert (I.TlDefine f elbl g) = 
    withFunName (I.fp_fun_name f) $ 
    do { (bl, bm) <- graphToBlocks g
       ; f' <- convert f
       ; elbla <- convert elbl
       ; let entryblk = case M.lookup elbla bm of
               Just x -> x
               Nothing -> error $ "irrefutable: entry block " ++ show elbl ++ " does not exist."
       ; let bs'' = entryblk:(filter (\x -> x /= entryblk) bl) 
       ; return $ A.TlDefine f' bs''
       } -- TODO: this method will NOT emit the new nodes generated by hoopl passes, it should be fixed ASAP.

                             
instance Conversion I.TlGlobal (Rm A.TlGlobal) where
  convert x = case x of
    (I.TlGlobalDtype a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) ->
      do { a10a <- maybeM convert a10
         ; return $ A.TlGlobal a1 a2 a3 a4 a5 a6 (fmap (tconvert ()) a7) 
           a8 a8a (tconvert () a9) a10a a11 a12 a13
         }
    (I.TlGlobalOpaque a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) ->
      do { a10a <- maybeM convert a10
         ; return $ A.TlGlobal a1 a2 a3 a4 a5 a6 (fmap (tconvert ()) a7) 
           a8 a8a (tconvert () a9) a10a a11 a12 a13
         }
    
instance Conversion I.TlTypeDef (Rm A.TlTypeDef) where    
  convert x = case x of
    (I.TlFunTypeDef lid t) -> return (A.TlTypeDef lid (tconvert () t))
    (I.TlDatTypeDef lid t) -> return (A.TlTypeDef lid (tconvert () t))
    (I.TlOpqTypeDef lid t) -> return (A.TlTypeDef lid (tconvert () t))

instance Conversion I.TlDepLibs (Rm A.TlDepLibs) where  
  convert (I.TlDepLibs s) = return (A.TlDepLibs s)
  
instance Conversion I.TlUnamedType (Rm A.TlUnamedType) where  
  convert (I.TlUnamedType i t) = return (A.TlUnamedType i (tconvert () t))
  
instance Conversion I.TlModuleAsm (Rm A.TlModuleAsm) where  
  convert (I.TlModuleAsm s) = return (A.TlModuleAsm s)

instance Conversion I.TlAttribute (Rm A.TlAttribute) where
  convert (I.TlAttribute n l) = return (A.TlAttribute n l)
  
instance Conversion I.TlComdat (Rm A.TlComdat) where  
  convert (I.TlComdat l s) = return (A.TlComdat l s)

    
type Pblock = (A.BlockLabel, [A.PhiInstWithDbg], [A.ComputingInstWithDbg])

getLabelId :: A.BlockLabel -> A.LabelId
getLabelId (A.ImplicitBlockLabel _) = error "ImplicitBlockLabel should be normalized"
getLabelId (A.ExplicitBlockLabel l) = l

isComment :: A.ComputingInstWithDbg -> Bool
isComment x = case x of
  A.ComputingInstWithComment _ -> True
  _ -> False

data PhiInstWithDbg = PhiInstWithDbg I.Pinst [I.Dbg] deriving (Eq, Ord, Show)
data CInstWithDbg = CInstWithDbg I.Cinst [I.Dbg] deriving (Eq, Ord, Show)
data MInstWithDbg = MInstWithDbg I.Minst [I.Dbg] deriving (Eq, Ord, Show)
data TerminatorInstWithDbg = TerminatorInstWithDbg I.Tinst [I.Dbg] deriving (Eq, Ord, Show)

convertNode :: I.Node a e x -> Rm ([A.Block], M.Map A.LabelId A.Block, Maybe Pblock)
               -> Rm ([A.Block], M.Map A.LabelId A.Block, Maybe Pblock)
convertNode (I.Lnode a) p = do { (bl, bs, Nothing) <- p
                               ; a' <- convert_to_BlockLabel a
                               ; return (bl, bs, Just (a', [], []))
                               }
convertNode (I.Pnode a dbgs) p = do { (bl, bs, pblk) <- p
                                    ; case pblk of
                                      Just (pb, phis, l) | all isComment l -> do { a' <- convert (PhiInstWithDbg a dbgs)
                                                                                 ; return (bl, bs, Just (pb, a':phis, l))
                                                                                 }
                                      _ -> I.errorLoc FLC $ "irrefutable:unexpected case " ++ show pblk
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
                                 ; return (bl, bs, Just (pb, phis, (A.ComputingInstWithComment a):cs))
                                 }
convertNode (I.Tnode a dbgs) p = do { (bl, bs, pb) <- p
                                    ; a' <- convert (TerminatorInstWithDbg a dbgs)
                                    ; case pb of
                                      Nothing -> error "irrefutable"
                                      Just (l, phis, cs) ->
                                        let blk = A.Block l (reverse phis) (reverse cs) a'
                                        in return (blk:bl, M.insert (getLabelId l) blk bs, Nothing)
                                    }
convertNode (I.Enode _) _ = error "irrefutable:Additional node should be converted to LLVM node"
  
graphToBlocks :: H.Graph (I.Node a) H.C H.C -> Rm ([A.Block], M.Map A.LabelId A.Block)
graphToBlocks g = do { (bl, bs, Nothing) <- H.foldGraphNodes convertNode g (return ([], M.empty, Nothing))
                     ; return (reverse bl, bs)
                     }

toplevel2Ast :: I.Toplevel a -> Rm A.Toplevel
toplevel2Ast (I.ToplevelTriple q) = Md.liftM A.ToplevelTriple (convert q)
toplevel2Ast (I.ToplevelDataLayout q) = Md.liftM A.ToplevelDataLayout (convert q)
toplevel2Ast (I.ToplevelAlias g) = Md.liftM A.ToplevelAlias (convert g)
toplevel2Ast (I.ToplevelDbgInit s) = Md.liftM A.ToplevelDbgInit (convert s)
toplevel2Ast (I.ToplevelStandaloneMd s) = Md.liftM (A.ToplevelStandaloneMd) (convert s)
toplevel2Ast (I.ToplevelNamedMd m) = Md.liftM A.ToplevelNamedMd (convert m) 
toplevel2Ast (I.ToplevelDeclare f) = Md.liftM A.ToplevelDeclare (convert f)
toplevel2Ast (I.ToplevelDefine f) = Md.liftM A.ToplevelDefine (convert f)
toplevel2Ast (I.ToplevelGlobal s) = Md.liftM A.ToplevelGlobal (convert s)
toplevel2Ast (I.ToplevelTypeDef t) = Md.liftM A.ToplevelTypeDef (convert t)
toplevel2Ast (I.ToplevelDepLibs qs) = Md.liftM A.ToplevelDepLibs (convert qs)
toplevel2Ast (I.ToplevelUnamedType i) = Md.liftM A.ToplevelUnamedType (convert i)
toplevel2Ast (I.ToplevelModuleAsm q) = Md.liftM A.ToplevelModuleAsm (convert q)
toplevel2Ast (I.ToplevelComdat l) = Md.liftM A.ToplevelComdat (convert l)
toplevel2Ast (I.ToplevelAttribute n) = Md.liftM A.ToplevelAttribute (convert n)

irToAst ::  M.Map (A.GlobalId, H.Label) A.LabelId -> I.Module a -> A.Module
irToAst iLm (I.Module ts) = runReader (Md.liftM A.Module (mapM toplevel2Ast ts)) (ReaderData iLm (A.GlobalIdNum 0))
