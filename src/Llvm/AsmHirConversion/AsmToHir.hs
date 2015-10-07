{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module Llvm.AsmHirConversion.AsmToHir(asmToHir) where

import Llvm.ErrorLoc
#define FLC  (FileLoc $(srcLoc))

import qualified Compiler.Hoopl as H
import qualified Control.Monad as Md
import qualified Data.Map as M
import qualified Llvm.Asm.Data as A
import qualified Llvm.Hir.Data as I
import Llvm.Hir.Cast
import Llvm.AsmHirConversion.LabelMapM
import Llvm.Util.Monadic (maybeM, pairM)
import Llvm.AsmHirConversion.TypeConversion
import Data.Maybe (fromJust)
import Control.Monad.Reader
import Llvm.AsmHirConversion.Specialization
import Llvm.Hir.DataLayoutMetrics
import Llvm.AsmHirConversion.MapLabels(compMapping)
import Llvm.Hir.Target

data ReaderData = ReaderData  { typedefs :: M.Map A.LocalId A.Type
                              , funname :: I.Gname
                              , idLabelMap :: IdLabelMap
                              }

type MM = ReaderT ReaderData (LabelMapM H.SimpleUniqueMonad)

typeDefs :: MM (M.Map A.LocalId A.Type)
typeDefs = ask >>= return . typedefs

funName :: MM I.Gname
funName = ask >>= return . funname

withFunName :: I.Gname -> MM a -> MM a
withFunName g f = withReaderT (\(ReaderData x _ lm) -> ReaderData x g lm) f


stripOffPa :: [A.ParamAttr] -> [A.ParamAttr -> Bool] -> 
              ([I.PAttr], [Maybe A.ParamAttr])
stripOffPa palist preds = 
  let (l,r) = foldl (\(pl, bl) pa -> let pl0 = filter (\x -> not $ pa x) pl
                                         out = filter pa pl
                                     in case out of
                                       [x] -> (pl0, bl++[Just x])
                                       [] -> (pl0, bl++[Nothing])
                    ) (palist,[]) preds
  in (fmap specializePAttr l, r)


{- Ast to Ir conversion -}


isTvector :: MP -> A.Type -> Bool
isTvector mp t = let (ta::I.Utype) = tconvert mp t
                 in case ta of
                   (I.UtypeVectorI _) -> True
                   (I.UtypeVectorF _) -> True
                   (I.UtypeVectorP _) -> True
                   _ -> False

getElemPtrIsTvector :: MP -> A.GetElementPtr v -> Bool
getElemPtrIsTvector mp (A.GetElementPtr n (A.Pointer (A.Typed t _)) l) = isTvector mp t

conversionIsTvector :: MP -> A.Conversion v -> Bool
conversionIsTvector mp (A.Conversion _ _ dt) = isTvector mp dt

convert_LabelId :: A.LabelId -> MM H.Label 
convert_LabelId x = do { fn <- funName
                       ; lift (labelFor (fn, x))
                       }


convert_PercentLabel :: A.PercentLabel -> MM H.Label 
convert_PercentLabel (A.PercentLabel l) = convert_LabelId l


convert_PercentLabelEx :: I.Gname -> A.PercentLabel -> MM (Maybe H.Label)
convert_PercentLabelEx g (A.PercentLabel l) = convert_LabelIdEx g l
  where 
    convert_LabelIdEx :: I.Gname -> A.LabelId -> MM (Maybe H.Label)
    convert_LabelIdEx fn x = do { r <- ask
                                ; let oldlabel = M.lookup (fn,x) (a2h $ idLabelMap r)
                                ; newlabel <- lift (getLabel (fn, x))
                                ; case (newlabel, oldlabel) of
                                  (Nothing, Nothing) -> errorLoc FLC $ show (fn,x)
                                  (Just x, Just y) | x == y -> return $ Just x  
                                  (Nothing, Just y) -> return $ Just y
                                  _ -> errorLoc FLC $ show (newlabel, oldlabel) ++ show (fn, x)
                                }


convert_TargetLabel :: A.TargetLabel -> MM H.Label 
convert_TargetLabel (A.TargetLabel tl) = convert_PercentLabel tl 

convert_BlockLabel :: A.BlockLabel -> MM H.Label
convert_BlockLabel (A.ImplicitBlockLabel p) = 
  error $ "ImplicitBlockLabel @" ++ show p 
  ++ " should be normalized away in Asm.Simplification, and should not be leaked to Ast2Ir."
convert_BlockLabel (A.ExplicitBlockLabel b) = convert_LabelId b 


convert_ComplexConstant :: A.ComplexConstant -> (MM (I.Const I.Gname))
convert_ComplexConstant (A.Cstruct b fs) = Md.liftM (I.C_struct b) 
                                           (mapM convert_TypedConstOrNUll fs)
convert_ComplexConstant (A.Cvector fs) = Md.liftM I.C_vector 
                                         (mapM convert_TypedConstOrNUll fs)
convert_ComplexConstant (A.Carray fs) = Md.liftM I.C_array 
                                        (mapM convert_TypedConstOrNUll fs)


data Binexp s v where {
  Add :: (Maybe I.NoWrap) -> I.Type s I.I -> v -> v -> Binexp s v;
  Sub :: (Maybe I.NoWrap) -> I.Type s I.I -> v -> v -> Binexp s v;
  Mul :: (Maybe I.NoWrap) -> I.Type s I.I -> v -> v -> Binexp s v;
  Udiv :: (Maybe I.Exact) -> I.Type s I.I -> v -> v -> Binexp s v;
  Sdiv :: (Maybe I.Exact) -> I.Type s I.I -> v -> v -> Binexp s v;
  Urem :: I.Type s I.I -> v -> v -> Binexp s v;
  Srem :: I.Type s I.I -> v -> v -> Binexp s v;
  Shl :: (Maybe I.NoWrap) -> I.Type s I.I -> v -> v -> Binexp s v;
  Lshr :: (Maybe I.Exact) -> I.Type s I.I -> v -> v -> Binexp s v;
  Ashr :: (Maybe I.Exact) -> I.Type s I.I -> v -> v -> Binexp s v;
  And :: I.Type s I.I -> v -> v -> Binexp s v;
  Or :: I.Type s I.I -> v -> v -> Binexp s v;
  Xor :: I.Type s I.I -> v -> v -> Binexp s v;
  } deriving (Eq, Ord, Show)

data FBinexp s v where {
  Fadd :: I.FastMathFlags -> I.Type s I.F -> v -> v -> FBinexp s v;
  Fsub :: I.FastMathFlags -> I.Type s I.F -> v -> v -> FBinexp s v;
  Fmul :: I.FastMathFlags -> I.Type s I.F -> v -> v -> FBinexp s v;
  Fdiv :: I.FastMathFlags -> I.Type s I.F -> v -> v -> FBinexp s v;
  Frem :: I.FastMathFlags -> I.Type s I.F -> v -> v -> FBinexp s v;
  } deriving (Eq, Ord, Show)




convert_to_Binexp :: (u -> MM v) -> A.IbinExpr u -> MM (Binexp I.ScalarB v)
convert_to_Binexp cvt (A.IbinExpr op cs t u1 u2) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; let ta::I.Type I.ScalarB I.I = dcast FLC $ ((tconvert mp t)::I.Utype)
     ; return (convert_IbinOp op cs ta u1a u2a)
     }
  where convert_IbinOp :: A.IbinOp -> [A.TrapFlag] -> I.Type I.ScalarB I.I -> v -> v -> Binexp I.ScalarB v
        convert_IbinOp op cs = case op of
          A.Add -> Add (getnowrap cs)
          A.Sub -> Sub (getnowrap cs)
          A.Mul -> Mul (getnowrap cs)
          A.Udiv -> Udiv (getexact cs)
          A.Sdiv -> Sdiv (getexact cs)
          A.Shl -> Shl (getnowrap cs)
          A.Lshr -> Lshr (getexact cs)
          A.Ashr -> Ashr (getexact cs)
          A.Urem -> Urem
          A.Srem -> Srem
          A.And -> And
          A.Or -> Or
          A.Xor -> Xor

convert_to_Binexp_V :: (u -> MM v) -> A.IbinExpr u -> MM (Binexp I.VectorB v)
convert_to_Binexp_V cvt (A.IbinExpr op cs t u1 u2) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; let ta::I.Type I.VectorB I.I = dcast FLC $ ((tconvert mp t)::I.Utype)
     ; return (convert_IbinOp op cs ta u1a u2a)
     }
  where convert_IbinOp :: A.IbinOp -> [A.TrapFlag] -> I.Type I.VectorB I.I -> v -> v -> Binexp I.VectorB v
        convert_IbinOp op cs = case op of
          A.Add -> Add (getnowrap cs)
          A.Sub -> Sub (getnowrap cs)
          A.Mul -> Mul (getnowrap cs)
          A.Udiv -> Udiv (getexact cs)
          A.Sdiv -> Sdiv (getexact cs)
          A.Shl -> Shl (getnowrap cs)
          A.Lshr -> Lshr (getexact cs)
          A.Ashr -> Ashr (getexact cs)
          A.Urem -> Urem
          A.Srem -> Srem
          A.And -> And
          A.Or -> Or
          A.Xor -> Xor


getnowrap :: [A.TrapFlag] -> Maybe I.NoWrap
getnowrap x = case x of
  [A.Nsw] -> Just I.Nsw
  [A.Nuw] -> Just I.Nuw
  [A.Nsw,A.Nuw] -> Just I.Nsuw
  [A.Nuw,A.Nsw] -> Just I.Nsuw
  [] -> Nothing
  _ -> error ("irrefutable error1 " ++ show x)

getexact :: [A.TrapFlag] -> Maybe I.Exact
getexact x = case x of
  [A.Exact] -> Just I.Exact
  [] -> Nothing
  _ -> error "irrefutable error2"


convert_to_FBinexp :: (u -> MM v) -> A.FbinExpr u -> (MM (FBinexp I.ScalarB v))
convert_to_FBinexp cvt (A.FbinExpr op cs t u1 u2) = 
    do { mp <- typeDefs
       ; u1a <- cvt u1
       ; u2a <- cvt u2
       ; let ta::I.Type I.ScalarB I.F = dcast FLC $ ((tconvert mp t)::I.Utype)
       ; return ((convertFop op) cs ta u1a u2a)
       }
    where
      convertFop o = case o of
        A.Fadd -> Fadd
        A.Fsub -> Fsub
        A.Fmul -> Fmul
        A.Fdiv -> Fdiv
        A.Frem -> Frem


convert_to_FBinexp_V :: (u -> MM v) -> A.FbinExpr u -> (MM (FBinexp I.VectorB v))
convert_to_FBinexp_V cvt (A.FbinExpr op cs t u1 u2) = 
    do { mp <- typeDefs
       ; u1a <- cvt u1
       ; u2a <- cvt u2
       ; let ta::I.Type I.VectorB I.F = dcast FLC $ ((tconvert mp t)::I.Utype)
       ; return ((convertFop op) cs ta u1a u2a)
       }
    where
      convertFop o = case o of
        A.Fadd -> Fadd
        A.Fsub -> Fsub
        A.Fmul -> Fmul
        A.Fdiv -> Fdiv
        A.Frem -> Frem


convert_to_Conversion :: (u -> MM v) -> A.Conversion u -> (MM (I.Conversion I.ScalarB v))
convert_to_Conversion cvt  (A.Conversion op (A.Typed t u) dt) = 
    do { mp <- typeDefs
       ; u1 <- cvt u 
       ; let (t1::I.Utype) = tconvert mp t
             (dt1::I.Utype) = tconvert mp dt
             newOp = case op of
               A.Trunc -> let (t2::I.Type I.ScalarB I.I) = dcast FLC t1
                              (dt2::I.Type I.ScalarB I.I) = dcast FLC dt1
                          in I.Trunc (I.T t2 u1) dt2
               A.Zext -> let (t2::I.Type I.ScalarB I.I) = dcast FLC t1
                             (dt2::I.Type I.ScalarB I.I) = dcast FLC dt1
                         in I.Zext (I.T t2 u1) dt2
               A.Sext -> let (t2::I.Type I.ScalarB I.I) = dcast FLC t1
                             (dt2::I.Type I.ScalarB I.I) = dcast FLC dt1
                         in I.Sext (I.T t2 u1) dt2
               A.FpTrunc -> let (t2::I.Type I.ScalarB I.F) = dcast FLC t1
                                (dt2::I.Type I.ScalarB I.F) = dcast FLC dt1
                            in I.FpTrunc (I.T t2 u1) dt2
               A.FpExt -> let (t2::I.Type I.ScalarB I.F) = dcast FLC t1
                              (dt2::I.Type I.ScalarB I.F) = dcast FLC dt1
                          in I.FpExt (I.T t2 u1) dt2 
               A.FpToUi -> let (t2::I.Type I.ScalarB I.F) = dcast FLC t1
                               (dt2::I.Type I.ScalarB I.I) = dcast FLC dt1
                           in I.FpToUi (I.T t2 u1) dt2 
               A.FpToSi -> let (t2::I.Type I.ScalarB I.F) = dcast FLC t1
                               (dt2::I.Type I.ScalarB I.I) = dcast FLC dt1
                           in I.FpToSi (I.T t2 u1) dt2 
               A.UiToFp -> let (t2::I.Type I.ScalarB I.I) = dcast FLC t1
                               (dt2::I.Type I.ScalarB I.F) = dcast FLC dt1
                           in I.UiToFp (I.T t2 u1) dt2 
               A.SiToFp -> let (t2::I.Type I.ScalarB I.I) = dcast FLC t1
                               (dt2::I.Type I.ScalarB I.F) = dcast FLC dt1
                           in I.SiToFp (I.T t2 u1) dt2 
               A.PtrToInt -> let (t2::I.Type I.ScalarB I.P) = dcast FLC t1
                                 (dt2::I.Type I.ScalarB I.I) = dcast FLC dt1
                             in I.PtrToInt (I.T t2 u1) dt2 
               A.IntToPtr -> let (t2::I.Type I.ScalarB I.I) = dcast FLC t1
                                 (dt2::I.Type I.ScalarB I.P) = dcast FLC dt1
                             in I.IntToPtr (I.T t2 u1) dt2 
               A.Bitcast -> let (t2::I.Dtype) = dcast FLC t1
                                (dt2::I.Dtype) = dcast FLC dt1
                            in I.Bitcast (I.T t2 u1) dt2 
               A.AddrSpaceCast -> let (t2::I.Type I.ScalarB I.P) = dcast FLC t1
                                      (dt2::I.Type I.ScalarB I.P) = dcast FLC dt1
                                  in I.AddrSpaceCast (I.T t2 u1) dt2 
       ; return newOp
       }

convert_to_Conversion_V :: (u -> MM v) -> A.Conversion u -> (MM (I.Conversion I.VectorB v))
convert_to_Conversion_V cvt  (A.Conversion op (A.Typed t u) dt) = 
    do { mp <- typeDefs
       ; u1 <- cvt u 
       ; let (t1::I.Utype) = tconvert mp t
             (dt1::I.Utype) = tconvert mp dt
             newOp = case op of
               A.Trunc -> let (t2::I.Type I.VectorB I.I) = dcast FLC t1
                              (dt2::I.Type I.VectorB I.I) = dcast FLC dt1
                          in I.Trunc (I.T t2 u1) dt2
               A.Zext -> let (t2::I.Type I.VectorB I.I) = dcast FLC t1
                             (dt2::I.Type I.VectorB I.I) = dcast FLC dt1
                         in I.Zext (I.T t2 u1) dt2 
               A.Sext -> let (t2::I.Type I.VectorB I.I) = dcast FLC t1
                             (dt2::I.Type I.VectorB I.I) = dcast FLC dt1
                         in I.Sext (I.T t2 u1) dt2 
               A.FpTrunc -> let (t2::I.Type I.VectorB I.F) = dcast FLC t1
                                (dt2::I.Type I.VectorB I.F) = dcast FLC dt1
                            in I.FpTrunc (I.T t2 u1) dt2 
               A.FpExt -> let (t2::I.Type I.VectorB I.F) = dcast FLC t1
                              (dt2::I.Type I.VectorB I.F) = dcast FLC dt1
                          in I.FpExt (I.T t2 u1) dt2 
               A.FpToUi -> let (t2::I.Type I.VectorB I.F) = dcast FLC t1
                               (dt2::I.Type I.VectorB I.I) = dcast FLC dt1
                           in I.FpToUi (I.T t2 u1) dt2 
               A.FpToSi -> let (t2::I.Type I.VectorB I.F) = dcast FLC t1
                               (dt2::I.Type I.VectorB I.I) = dcast FLC dt1
                           in I.FpToSi (I.T t2 u1) dt2 
               A.UiToFp -> let (t2::I.Type I.VectorB I.I) = dcast FLC t1
                               (dt2::I.Type I.VectorB I.F) = dcast FLC dt1
                           in I.UiToFp (I.T t2 u1) dt2 
               A.SiToFp -> let (t2::I.Type I.VectorB I.I) = dcast FLC t1
                               (dt2::I.Type I.VectorB I.F) = dcast FLC dt1
                           in I.SiToFp (I.T t2 u1) dt2 
               A.PtrToInt -> let (t2::I.Type I.VectorB I.P) = dcast FLC t1
                                 (dt2::I.Type I.VectorB I.I) = dcast FLC dt1
                             in I.PtrToInt (I.T t2 u1) dt2 
               A.IntToPtr -> let (t2::I.Type I.VectorB I.I) = dcast FLC t1
                                 (dt2::I.Type I.VectorB I.P) = dcast FLC dt1
                             in I.IntToPtr (I.T t2 u1) dt2 
               A.Bitcast -> let (t2::I.Dtype) = dcast FLC t1
                                (dt2::I.Dtype) = dcast FLC dt1
                            in I.Bitcast (I.T t2 u1) dt2 
               A.AddrSpaceCast -> let (t2::I.Type I.VectorB I.P) = dcast FLC t1
                                      (dt2::I.Type I.VectorB I.P) = dcast FLC dt1
                                  in I.AddrSpaceCast (I.T t2 u1) dt2 
       ; return newOp
       }


convert_to_GetElementPtr :: (u -> MM v) -> (u -> MM idx) -> A.GetElementPtr u -> (MM (I.GetElementPtr I.ScalarB v idx))
convert_to_GetElementPtr cvt cvidx (A.GetElementPtr b (A.Pointer (A.Typed t u)) us) = 
  do { mp <- typeDefs
     ; ua <- cvt u
     ; let (ta::I.Type I.ScalarB I.P) = dcast FLC ((tconvert mp t)::I.Utype)
     ; usa <- mapM convert_Tv_Tint us
     ; return $ I.GetElementPtr b (I.T ta ua) usa
     }
  where
    convert_Tv_Tint (A.Typed t v) = 
      do { mp <- typeDefs
         ; va <- cvidx v
         ; let (ta::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t)::I.Utype)
         ; return $ I.T ta va
         }


convert_to_GetElementPtr_V :: (u -> MM v) -> (u -> MM idx) -> A.GetElementPtr u -> (MM (I.GetElementPtr I.VectorB v idx))
convert_to_GetElementPtr_V cvt cvidx (A.GetElementPtr b (A.Pointer (A.Typed t u)) us) = 
  do { mp <- typeDefs
     ; ua <- cvt u
     ; let (ta::I.Type I.VectorB I.P) = dcast FLC ((tconvert mp t)::I.Utype)
     ; usa <- mapM (convert_Tv_Tint) us
     ; return $ I.GetElementPtr b (I.T ta ua) usa
     }
  where
    convert_Tv_Tint (A.Typed te v) = do { mp <- typeDefs
                                        ; va <- cvidx v
                                        ; let (ta::I.Utype) = tconvert mp te
                                        ; return $ I.T (dcast FLC ta) va
                                        }


cast_to_EitherScalarOrVectorI :: FileLoc -> I.T I.Utype v -> 
                                 Either (I.T (I.Type I.ScalarB I.I) v) (I.T (I.Type I.VectorB I.I) v)
cast_to_EitherScalarOrVectorI flc (I.T t v) = case t of
  I.UtypeScalarI e -> Left $ I.T e v
  I.UtypeVectorI e -> Right $ I.T e v
  _ -> errorLoc flc "$$$$"
  
  
convert_to_Select_I :: (u -> MM v) -> A.Select u -> (MM (I.Select I.ScalarB I.I v))
convert_to_Select_I cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return $ I.Select (Left (I.T t1a u1a)) (I.T t2a u2a) (I.T t3a u3a)
     }

convert_to_Select_VI :: (u -> MM v) -> A.Select u -> (MM (I.Select I.VectorB I.I v))
convert_to_Select_VI cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let t1a = cast_to_EitherScalarOrVectorI FLC (I.T ((tconvert mp t1)::I.Utype) u1a)
           (t2a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return $ I.Select t1a (I.T t2a u2a) (I.T t3a u3a)
     }

convert_to_Select_F :: (u -> MM v) -> A.Select u -> (MM (I.Select I.ScalarB I.F v))
convert_to_Select_F cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.F) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.ScalarB I.F) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return $ I.Select (Left (I.T t1a u1a)) (I.T t2a u2a) (I.T t3a u3a)
     }

convert_to_Select_VF :: (u -> MM v) -> A.Select u -> (MM (I.Select I.VectorB I.F v))
convert_to_Select_VF cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let t1a = cast_to_EitherScalarOrVectorI FLC (I.T ((tconvert mp t1)::I.Utype) u1a)
           (t2a::I.Type I.VectorB I.F) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.VectorB I.F) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return $ I.Select t1a (I.T t2a u2a) (I.T t3a u3a)
     }

convert_to_Select_P :: (u -> MM v) -> A.Select u -> (MM (I.Select I.ScalarB I.P v))
convert_to_Select_P cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.P) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.ScalarB I.P) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return $ I.Select (Left (I.T t1a u1a)) (I.T t2a u2a) (I.T t3a u3a)
     }


convert_to_Select_VP :: (u -> MM v) -> A.Select u -> (MM (I.Select I.VectorB I.P v))
convert_to_Select_VP cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let t1a = cast_to_EitherScalarOrVectorI FLC (I.T ((tconvert mp t1)::I.Utype) u1a)
           (t2a::I.Type I.VectorB I.P) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.VectorB I.P) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return $ I.Select t1a (I.T t2a u2a) (I.T t3a u3a)
     }


convert_to_Select_Record :: (u -> MM v) -> A.Select u -> (MM (I.Select I.FirstClassB I.D v))
convert_to_Select_Record cvt (A.Select (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.ScalarB I.I) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.FirstClassB I.D) = squeeze FLC (dcast FLC ((tconvert mp t2)::I.Utype))
           (t3a::I.Type I.FirstClassB I.D) = squeeze FLC (dcast FLC ((tconvert mp t3)::I.Utype))
     ; return $ I.Select (Left (I.T t1a u1a)) (I.T t2a u2a) (I.T t3a u3a)
     }


convert_to_Icmp :: (u -> MM v) -> A.Icmp u -> MM (I.Icmp I.ScalarB v)
convert_to_Icmp cvt (A.Icmp op t u1 u2) = 
  do { mp <- typeDefs
     ; let (t1::I.IntOrPtrType I.ScalarB) = dcast FLC ((tconvert mp t)::I.Utype)
     ; u1a <- cvt u1 
     ; u2a <- cvt u2
     ; return (I.Icmp op t1 u1a u2a)
     }

convert_to_Icmp_V :: (u -> MM v) -> A.Icmp u -> MM (I.Icmp I.VectorB v)
convert_to_Icmp_V cvt (A.Icmp op t u1 u2) = 
  do { mp <- typeDefs
     ; let (t1::I.IntOrPtrType I.VectorB) = dcast FLC ((tconvert mp t)::I.Utype)
     ; u1a <- cvt u1 
     ; u2a <- cvt u2
     ; return (I.Icmp op t1 u1a u2a)
     }

convert_to_Fcmp :: (u -> MM v) -> A.Fcmp u -> MM (I.Fcmp I.ScalarB v)
convert_to_Fcmp cvt (A.Fcmp op t u1 u2) = 
  do { mp <- typeDefs
     ; let (t1::I.Type I.ScalarB I.F) = dcast FLC ((tconvert mp t)::I.Utype)
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; return (I.Fcmp op t1 u1a u2a)
     }

convert_to_Fcmp_V :: (u -> MM v) -> A.Fcmp u -> MM (I.Fcmp I.VectorB v)
convert_to_Fcmp_V cvt (A.Fcmp op t u1 u2) = 
  do { mp <- typeDefs
     ; let (t1::I.Type I.VectorB I.F) = dcast FLC ((tconvert mp t)::I.Utype)
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; return (I.Fcmp op t1 u1a u2a)
     }

convert_to_ShuffleVector_I :: (u -> MM v) -> A.ShuffleVector u -> MM (I.ShuffleVector I.I v)
convert_to_ShuffleVector_I cvt (A.ShuffleVector (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) =
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return (I.ShuffleVector (I.T t1a u1a) (I.T t2a u2a) (I.T t3a u3a))
     }

convert_to_ShuffleVector_F :: (u -> MM v) -> A.ShuffleVector u -> MM (I.ShuffleVector I.F v)
convert_to_ShuffleVector_F cvt (A.ShuffleVector (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) =
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.VectorB I.F) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.VectorB I.F) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return (I.ShuffleVector (I.T t1a u1a) (I.T t2a u2a) (I.T t3a u3a))
     }

convert_to_ShuffleVector_P :: (u -> MM v) -> A.ShuffleVector u -> MM (I.ShuffleVector I.P v)
convert_to_ShuffleVector_P cvt (A.ShuffleVector (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) =
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.VectorB I.P) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.VectorB I.P) = dcast FLC ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.VectorB I.I) = dcast FLC ((tconvert mp t3)::I.Utype)
     ; return (I.ShuffleVector (I.T t1a u1a) (I.T t2a u2a) (I.T t3a u3a))
     }

convert_to_ExtractValue :: (u -> MM v) -> A.ExtractValue u -> MM (I.ExtractValue v)
convert_to_ExtractValue cvt (A.ExtractValue (A.Typed t u) s) = 
  do { mp <- typeDefs
     ; let (ta::I.Type I.RecordB I.D) = dcast FLC ((tconvert mp t)::I.Utype)
     ; ua <- cvt u 
     ; return (I.ExtractValue (I.T ta ua) s)
     }

convert_to_InsertValue :: (u -> MM v) -> A.InsertValue u -> MM (I.InsertValue v)
convert_to_InsertValue cvt (A.InsertValue (A.Typed t1 u1) (A.Typed t2 u2) s) = 
  do { mp <- typeDefs
     ; let (t1a::I.Type I.RecordB I.D) = dcast FLC ((tconvert mp t1)::I.Utype)
           (t2a::I.Dtype) = dcast FLC ((tconvert mp t2)::I.Utype)
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; return $ I.InsertValue (I.T t1a u1a) (I.T t2a u2a) s
     }

convert_to_ExtractElement_I :: (u -> MM v) -> A.ExtractElement u -> MM (I.ExtractElement I.I v)
convert_to_ExtractElement_I cvt (A.ExtractElement (A.Typed t1 u1) (A.Typed t2 u2)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; let (t1a::I.Type I.VectorB I.I) = dcast FLC $ ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t2)::I.Utype)
     ; return $ I.ExtractElement (I.T t1a u1a) (I.T t2a u2a)
     }

convert_to_ExtractElement_F :: (u -> MM v) -> A.ExtractElement u -> MM (I.ExtractElement I.F v)
convert_to_ExtractElement_F cvt (A.ExtractElement (A.Typed t1 u1) (A.Typed t2 u2)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; let (t1a::I.Type I.VectorB I.F) = dcast FLC $ ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t2)::I.Utype)
     ; return $ I.ExtractElement (I.T t1a u1a) (I.T t2a u2a)
     }

convert_to_ExtractElement_P :: (u -> MM v) -> A.ExtractElement u -> MM (I.ExtractElement I.P v)
convert_to_ExtractElement_P cvt (A.ExtractElement (A.Typed t1 u1) (A.Typed t2 u2)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; let (t1a::I.Type I.VectorB I.P) = dcast FLC $ ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t2)::I.Utype)
     ; return $ I.ExtractElement (I.T t1a u1a) (I.T t2a u2a)
     }

convert_to_InsertElement_I :: (u -> MM v) -> A.InsertElement u ->  MM (I.InsertElement I.I v)
convert_to_InsertElement_I cvt (A.InsertElement (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.VectorB I.I) = dcast FLC $ ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t3)::I.Utype)
     ; return $ I.InsertElement (I.T t1a u1a) (I.T t2a u2a) (I.T t3a u3a)
     }

convert_to_InsertElement_F :: (u -> MM v) -> A.InsertElement u ->  MM (I.InsertElement I.F v)
convert_to_InsertElement_F cvt (A.InsertElement (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.VectorB I.F) = dcast FLC $ ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.F) = dcast FLC $ ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t3)::I.Utype)
     ; return $ I.InsertElement (I.T t1a u1a) (I.T t2a u2a) (I.T t3a u3a)
     }

convert_to_InsertElement_P :: (u -> MM v) -> A.InsertElement u ->  MM (I.InsertElement I.P v)
convert_to_InsertElement_P cvt (A.InsertElement (A.Typed t1 u1) (A.Typed t2 u2) (A.Typed t3 u3)) = 
  do { mp <- typeDefs
     ; u1a <- cvt u1
     ; u2a <- cvt u2
     ; u3a <- cvt u3
     ; let (t1a::I.Type I.VectorB I.P) = dcast FLC $ ((tconvert mp t1)::I.Utype)
           (t2a::I.Type I.ScalarB I.P) = dcast FLC $ ((tconvert mp t2)::I.Utype)
           (t3a::I.Type I.ScalarB I.I) = dcast FLC $ ((tconvert mp t3)::I.Utype)
     ; return $ I.InsertElement (I.T t1a u1a) (I.T t2a u2a) (I.T t3a u3a)
     }


convert_SimpleConst :: A.SimpleConstant -> I.Const I.Gname
convert_SimpleConst x = case x of
  A.CpInt s -> I.C_int s
  A.CpUhexInt s -> I.C_uhex_int s
  A.CpShexInt s -> I.C_shex_int s
  A.CpFloat s -> I.C_float s
  A.CpNull -> I.C_null
  A.CpUndef -> I.C_undef
  A.CpTrue -> I.C_true
  A.CpFalse -> I.C_false
  A.CpZeroInitializer -> I.C_zeroinitializer
  A.CpGlobalAddr s -> I.C_globalAddr (specializeGlobalId s)
  A.CpStr s -> I.C_str s
  A.CpBconst s -> convert_Bconst s
  
convert_Bconst :: A.BinaryConstant -> I.Const I.Gname
convert_Bconst x = case x of
  A.BconstUint8 s -> I.C_u8 s
  A.BconstUint16 s -> I.C_u16 s
  A.BconstUint32 s -> I.C_u32 s
  A.BconstUint64 s -> I.C_u64 s
  A.BconstUint96 s -> I.C_u96 s
  A.BconstUint128 s -> I.C_u128 s
  A.BconstInt8 s -> I.C_s8 s
  A.BconstInt16 s -> I.C_s16 s
  A.BconstInt32 s -> I.C_s32 s
  A.BconstInt64 s -> I.C_s64 s
  A.BconstInt96 s -> I.C_s96 s
  A.BconstInt128 s -> I.C_s128 s

convert_Const :: A.Const -> MM (I.Const I.Gname)
convert_Const x = 
  let cvt = convert_Const
  in case x of
    A.C_simple a -> return $ convert_SimpleConst a
    A.C_complex a -> convert_ComplexConstant a
    A.C_labelId a -> Md.liftM I.C_labelId (convert_LabelId a)
    A.C_blockAddress g a -> do { a' <- convert_PercentLabelEx (specializeGlobalId g) a
                               ; case a' of
                                 Just a'' -> return $ I.C_block (specializeGlobalId g) a''
                                 Nothing -> errorLoc FLC $ show x 
                               }
    A.C_binexp (A.Ie a@(A.IbinExpr _ _ t _ _)) -> 
      do { mp <- typeDefs
         ; if isTvector mp t then
             do { x <- convert_to_Binexp_V cvt a 
                ; let y = case x of
                        Add n ta v1a v2a -> I.C_add_V n ta v1a v2a
                        Sub n ta v1a v2a -> I.C_sub_V n ta v1a v2a
                        Mul n ta v1a v2a -> I.C_mul_V n ta v1a v2a
                        Udiv n ta v1a v2a -> I.C_udiv_V n ta v1a v2a
                        Sdiv n ta v1a v2a -> I.C_sdiv_V n ta v1a v2a
                        Urem ta v1a v2a -> I.C_urem_V ta v1a v2a
                        Srem ta v1a v2a -> I.C_srem_V ta v1a v2a
                        Shl n ta v1a v2a -> I.C_shl_V n ta v1a v2a
                        Lshr n ta v1a v2a -> I.C_lshr_V n ta v1a v2a
                        Ashr n ta v1a v2a -> I.C_ashr_V n ta v1a v2a
                        And ta v1a v2a -> I.C_and_V ta v1a v2a
                        Or ta v1a v2a -> I.C_or_V ta v1a v2a
                        Xor ta v1a v2a -> I.C_xor_V ta v1a v2a
                ; return y 
                }
           else
             do { x <- convert_to_Binexp cvt a 
                ; let y = case x of
                        Add n ta v1a v2a -> I.C_add n ta v1a v2a
                        Sub n ta v1a v2a -> I.C_sub n ta v1a v2a
                        Mul n ta v1a v2a -> I.C_mul n ta v1a v2a
                        Udiv n ta v1a v2a -> I.C_udiv n ta v1a v2a
                        Sdiv n ta v1a v2a -> I.C_sdiv n ta v1a v2a
                        Urem ta v1a v2a -> I.C_urem ta v1a v2a
                        Srem ta v1a v2a -> I.C_srem ta v1a v2a
                        Shl n ta v1a v2a -> I.C_shl n ta v1a v2a
                        Lshr n ta v1a v2a -> I.C_lshr n ta v1a v2a
                        Ashr n ta v1a v2a -> I.C_ashr n ta v1a v2a
                        And ta v1a v2a -> I.C_and ta v1a v2a
                        Or ta v1a v2a -> I.C_or ta v1a v2a
                        Xor ta v1a v2a -> I.C_xor ta v1a v2a
                ; return y 
                }
         }
    A.C_binexp (A.Fe a@(A.FbinExpr _ _ t _ _)) -> 
      do { mp <- typeDefs
         ; if isTvector mp t then
             do { x <- convert_to_FBinexp_V cvt a
                ; let y = case x of
                        Fadd n ta v1a v2a -> I.C_fadd_V n ta v1a v2a
                        Fsub n ta v1a v2a -> I.C_fsub_V n ta v1a v2a
                        Fmul n ta v1a v2a -> I.C_fmul_V n ta v1a v2a
                        Fdiv n ta v1a v2a -> I.C_fdiv_V n ta v1a v2a
                        Frem n ta v1a v2a -> I.C_frem_V n ta v1a v2a
                ; return y 
                }
           else
             do { x <- convert_to_FBinexp cvt a
                ; let y = case x of
                        Fadd n ta v1a v2a -> I.C_fadd n ta v1a v2a
                        Fsub n ta v1a v2a -> I.C_fsub n ta v1a v2a
                        Fmul n ta v1a v2a -> I.C_fmul n ta v1a v2a
                        Fdiv n ta v1a v2a -> I.C_fdiv n ta v1a v2a
                        Frem n ta v1a v2a -> I.C_frem n ta v1a v2a
                ; return y 
                }
         }
    A.C_conv a ->
      do { mp <- typeDefs
         ; if conversionIsTvector mp a then
             do { x <- convert_to_Conversion_V cvt a
                ; let y = case x of
                        I.Trunc tv dt -> I.C_trunc_V tv dt
                        I.Zext tv dt -> I.C_zext_V tv dt
                        I.Sext tv dt -> I.C_sext_V tv dt
                        I.FpTrunc tv dt -> I.C_fptrunc_V tv dt
                        I.FpExt tv dt -> I.C_fpext_V tv dt
                        I.FpToUi tv dt -> I.C_fptoui_V tv dt
                        I.FpToSi tv dt -> I.C_fptosi_V tv dt
                        I.UiToFp tv dt -> I.C_uitofp_V tv dt
                        I.SiToFp tv dt -> I.C_sitofp_V tv dt
                        I.PtrToInt tv dt -> I.C_ptrtoint_V tv dt
                        I.IntToPtr tv dt -> I.C_inttoptr_V tv dt
                        I.Bitcast tv dt -> I.C_bitcast tv dt
                        I.AddrSpaceCast tv dt -> I.C_addrspacecast_V tv dt 
                ; return y
                }
           else
             do { x <- convert_to_Conversion cvt a
                ; let y = case x of
                        I.Trunc tv dt -> I.C_trunc tv dt
                        I.Zext tv dt -> I.C_zext tv dt
                        I.Sext tv dt -> I.C_sext tv dt
                        I.FpTrunc tv dt -> I.C_fptrunc tv dt
                        I.FpExt tv dt -> I.C_fpext tv dt
                        I.FpToUi tv dt -> I.C_fptoui tv dt
                        I.FpToSi tv dt -> I.C_fptosi tv dt
                        I.UiToFp tv dt -> I.C_uitofp tv dt
                        I.SiToFp tv dt -> I.C_sitofp tv dt
                        I.PtrToInt tv dt -> I.C_ptrtoint tv dt
                        I.IntToPtr tv dt -> I.C_inttoptr tv dt
                        I.Bitcast tv dt -> I.C_bitcast tv dt
                        I.AddrSpaceCast tv dt -> I.C_addrspacecast tv dt 
                ; return y
                }
         }
    A.C_gep a -> 
      do { mp <- typeDefs
         ; if getElemPtrIsTvector mp a then 
             do { (I.GetElementPtr b t idx) <- convert_to_GetElementPtr_V convert_Const convert_Const a
                ; return $ I.C_getelementptr_V b t idx
                }
           else 
             do { (I.GetElementPtr b t idx) <- convert_to_GetElementPtr convert_Const convert_Const a
                ; return $ I.C_getelementptr b t idx
                }
         }
    A.C_select a@(A.Select _ (A.Typed t _) _) -> 
      do { mp <- typeDefs
         ; case matchType mp t of
           Tk_VectorI -> Md.liftM I.C_select_VI (convert_to_Select_VI cvt a)
           Tk_ScalarI -> Md.liftM I.C_select_I (convert_to_Select_I cvt a)
           Tk_VectorF -> Md.liftM I.C_select_VF (convert_to_Select_VF cvt a)
           Tk_ScalarF -> Md.liftM I.C_select_F (convert_to_Select_F cvt a)
           Tk_VectorP -> Md.liftM I.C_select_VP (convert_to_Select_VP cvt a)
           Tk_ScalarP -> Md.liftM I.C_select_P (convert_to_Select_P cvt a)
           Tk_RecordD -> do { (I.Select (Left cnd) t f) <- convert_to_Select_Record cvt a 
                            ; return $ I.C_select_First cnd t f
                            }
         }
    A.C_icmp a@(A.Icmp _ t _ _) -> 
      do { mp <- typeDefs
         ; if isTvector mp t then Md.liftM I.C_icmp_V (convert_to_Icmp_V cvt a)
           else Md.liftM I.C_icmp (convert_to_Icmp cvt a)
         }
    A.C_fcmp a@(A.Fcmp _ t _ _) -> 
      do { mp <- typeDefs
         ; if isTvector mp t then Md.liftM I.C_fcmp_V (convert_to_Fcmp_V cvt a)
           else Md.liftM I.C_fcmp (convert_to_Fcmp cvt a)
         }
    A.C_shufflevector a@(A.ShuffleVector (A.Typed t _) _ _) -> 
      do { mp <- typeDefs
         ; case matchType mp t of
           Tk_VectorI -> Md.liftM I.C_shufflevector_I (convert_to_ShuffleVector_I cvt a)
           Tk_VectorF -> Md.liftM I.C_shufflevector_F (convert_to_ShuffleVector_F cvt a)
           Tk_VectorP -> Md.liftM I.C_shufflevector_P (convert_to_ShuffleVector_P cvt a)
         }
    A.C_extractvalue a -> Md.liftM I.C_extractvalue (convert_to_ExtractValue cvt a)
    A.C_insertvalue a -> Md.liftM I.C_insertvalue (convert_to_InsertValue cvt a)
    A.C_extractelement a@(A.ExtractElement (A.Typed t _) _) -> 
      do { mp <- typeDefs
         ; case matchType mp t of
           Tk_VectorI -> Md.liftM I.C_extractelement_I (convert_to_ExtractElement_I cvt a)
           Tk_VectorF -> Md.liftM I.C_extractelement_F (convert_to_ExtractElement_F cvt a)
           Tk_VectorP -> Md.liftM I.C_extractelement_P (convert_to_ExtractElement_P cvt a)
         }
    A.C_insertelement a@(A.InsertElement (A.Typed t _) _ _) -> 
      do { mp <- typeDefs
         ; case matchType mp t of
           Tk_VectorI -> Md.liftM I.C_insertelement_I (convert_to_InsertElement_I cvt a)
           Tk_VectorF -> Md.liftM I.C_insertelement_F (convert_to_InsertElement_F cvt a)
           Tk_VectorP -> Md.liftM I.C_insertelement_P (convert_to_InsertElement_P cvt a)
         }
        
convert_MdName :: A.MdName -> (MM I.MdName)
convert_MdName (A.MdName s) = return $ I.MdName s

convert_MdNode :: A.MdNode -> (MM I.MdNode)
convert_MdNode (A.MdNode s) = return $ I.MdNode s

convert_MdRef :: A.MdRef -> MM I.MdRef
convert_MdRef x = case x of
  A.MdRefName n -> Md.liftM I.MdRefName (convert_MdName n)
  A.MdRefNode n -> Md.liftM I.MdRefNode (convert_MdNode n)  

convert_MetaConst :: A.MetaConst -> MM (I.MetaConst I.Gname)
convert_MetaConst (A.McStruct c) = Md.liftM I.McStruct (mapM convert_MetaKindedConst c)
convert_MetaConst (A.McString s) = return $ I.McString s
convert_MetaConst (A.McMdRef n) = Md.liftM I.McMdRef (convert_MdRef n)
convert_MetaConst (A.McSsa i) = return $ I.McSsa (sLid i)
convert_MetaConst (A.McSimple sc) = Md.liftM I.McSimple (convert_Const sc)

convert_MetaKindedConst :: A.MetaKindedConst -> MM (I.MetaKindedConst I.Gname)
convert_MetaKindedConst x = 
  do { mp <- typeDefs
     ; case x of
       (A.MetaKindedConst mk mc) -> Md.liftM (I.MetaKindedConst (tconvert mp mk)) (convert_MetaConst mc)
       A.UnmetaKindedNull -> return I.UnmetaKindedNull
     }




convert_FunPtr :: A.FunName -> MM (I.FunPtr I.Gname)
convert_FunPtr fn = 
  case fn of
    A.FunNameGlobal g -> case g of
      A.GolG g0 -> return $ I.FunId $ specializeGlobalId g0
      A.GolL l0 -> return $ I.FunSsa $ sLid l0
    A.FunNameBitcast tv t ->
      do { mp <- typeDefs
         ; (I.T st c) <- convert_to_DtypedConst tv
         ; let t1::I.Utype = tconvert mp t
         ; return $ I.FunIdBitcast (I.T st c) (dcast FLC t1)
         }
    A.FunNameInttoptr tv t -> 
       do { mp <- typeDefs
          ; (I.T st c) <- convert_to_DtypedConst tv
          ; let t1::I.Utype = tconvert mp t
          ; return $ I.FunIdInttoptr (I.T st c) (dcast FLC t1)
          }
    A.FunName_null -> return I.Fun_null
    A.FunName_undef -> return I.Fun_undef


convert_FunId :: A.FunName -> MM I.Gname
convert_FunId (A.FunNameGlobal (A.GolG g)) = return $ specializeGlobalId g
convert_FunId x = errorLoc FLC $ show x


convert_Value :: A.Value -> MM (I.Value I.Gname)
convert_Value (A.Val_local a) = return $ I.Val_ssa $ sLid a
convert_Value (A.Val_const a) = Md.liftM I.Val_const (convert_Const a)

convert_to_Minst :: A.CallSite -> MM (Maybe (I.Minst I.Gname))
convert_to_Minst x = case x of
  (A.CallSiteFun cc pa t fn aps fa) | any isMetaParam aps ->
    do { mp <- typeDefs
       ; fna <- convert_FunId fn
       ; let ert = A.splitCallReturnType t
       ; apsa <- mapM convert_MetaParam aps                
       ; return (Just $ specializeMinst 
                 $ I.Minst (I.CallSiteTypeRet $ dcast FLC ((tconvert mp (fst ert))::I.Utype)) fna apsa)
       }
  _ -> return Nothing 

convert_to_CallFunInterface :: A.TailCall -> A.CallSite 
                               -> MM (Bool, I.FunPtr I.Gname, I.CallFunInterface I.Gname)
convert_to_CallFunInterface tc (A.CallSiteFun cc pa t fn aps fa) = 
  do { mp <- typeDefs
     ; let ert = A.splitCallReturnType t
     ; erta <- eitherRet (fmap specializeRetAttr pa) ert aps
     ; fna <- convert_FunPtr fn
     ; apsa <- mapM convert_ActualParam aps
     ; return (fst ert == A.Tvoid, fna, I.CallFunInterface { I.cfi_tail = tc 
                                                           , I.cfi_castType = fst erta
                                                           , I.cfi_signature = I.FunSignature (maybe I.Ccc id cc) (snd erta) apsa 
                                                           , I.cfi_funAttrs = fa
                                                           })
     }

convert_to_InvokeFunInterface :: A.CallSite -> MM (Bool, I.FunPtr I.Gname, I.InvokeFunInterface I.Gname)
convert_to_InvokeFunInterface (A.CallSiteFun cc pa t fn aps fa) = 
  do { mp <- typeDefs
     ; let ert = A.splitCallReturnType t
     ; erta <- eitherRet (fmap specializeRetAttr pa) ert aps
     ; fna <- convert_FunPtr fn
     ; apsa <- mapM convert_ActualParam aps
     ; return (fst ert == A.Tvoid, fna, I.InvokeFunInterface { I.ifi_castType = fst erta
                                                             , I.ifi_signature = I.FunSignature (maybe I.Ccc id cc) 
                                                                                 (snd erta) apsa 
                                                             , I.ifi_funAttrs = fa
                                                             })
     }

convert_to_CallAsmInterface :: A.InlineAsmExp -> MM (Bool, I.AsmCode, I.CallAsmInterface I.Gname)
convert_to_CallAsmInterface  (A.InlineAsmExp t dia b1 b2 qs1 qs2 as fa) =
  do { mp <- typeDefs
     ; let ert = A.splitCallReturnType t
     ; erta <- eitherRet [] ert as
     ; asa <- mapM convert_ActualParam as
     ; let rt::I.Utype = tconvert mp (fst ert)
     ; return (fst ert == A.Tvoid, I.AsmCode b2 qs1 qs2, I.CallAsmInterface (snd erta) dia b1 asa fa)
     }

    
eitherRet :: [I.RetAttr] -> (A.Type, Maybe (A.Type, A.AddrSpace)) 
             -> [A.ActualParam] -> MM (Maybe (I.Type I.ScalarB I.P), I.Type I.CodeFunB I.X)
eitherRet retAttrs (rt, ft) actualParams = 
  let ts = fmap (\x -> case x of
                    A.ActualParamData t pa _ -> (t, pa)
                    A.ActualParamLabel t pa _ -> (ucast t, pa)
                    _ -> errorLoc FLC $ show x
                ) actualParams
  in 
  do { mp <- typeDefs
     ; funType <- composeTfunction (rt,retAttrs) ts Nothing
     ; case ft of
       Just (fta,as) -> case (dcast FLC ((tconvert mp fta)::I.Utype))::I.Type I.CodeFunB I.X of
         I.Tfunction (rt0,_) mts mv -> 
           return (Just $ I.Tpointer (ucast $ I.Tfunction (rt0, retAttrs) mts mv) (tconvert mp as), funType)
       Nothing -> return (Nothing, funType) 
     }

convert_Clause :: A.Clause -> MM (I.Clause I.Gname)
convert_Clause x = case x of 
  (A.ClauseCatch (A.Typed t v)) -> do { mp <- typeDefs
                                      ; let (ti::I.Dtype) = dcast FLC ((tconvert mp t)::I.Utype)
                                      ; vi <- convert_Value v
                                      ; return $ I.Catch (I.T ti vi)
                                      }
  (A.ClauseFilter tc) -> Md.liftM I.Filter (convert_TypedConstOrNUll tc)
  (A.ClauseConversion tc) ->  
    do { mp <- typeDefs
       ; if conversionIsTvector mp tc then Md.liftM I.CcoV (convert_to_Conversion_V convert_Value tc)
         else Md.liftM I.CcoS (convert_to_Conversion convert_Value tc)
       }


convert_GlobalOrLocalId :: A.GlobalOrLocalId -> MM I.GlobalOrLocalId
convert_GlobalOrLocalId = return

convert_Expr_CInst :: (Maybe A.LocalId, A.Expr) -> MM (I.Cinst I.Gname)
convert_Expr_CInst (Just lhs, A.ExprGetElementPtr c) = 
  do { mp <- typeDefs
     ; if getElemPtrIsTvector mp c then 
         do { (I.GetElementPtr b t idx) <- convert_to_GetElementPtr_V convert_Value convert_Value c
            ; return $ I.I_getelementptr_V b t idx (sLid lhs)
            }
       else 
         do { (I.GetElementPtr b t idx) <- convert_to_GetElementPtr convert_Value convert_Value c
            ; return $ I.I_getelementptr b t idx (sLid lhs)
            }
     }
convert_Expr_CInst (Just lhs, A.ExprIcmp a@(A.Icmp _ t _ _)) = 
  do { mp <- typeDefs
     ; if isTvector mp t then 
         do { (I.Icmp op ta v1a v2a) <- convert_to_Icmp_V convert_Value a
            ; return $ I.I_icmp_V op ta v1a v2a (sLid lhs)
            }
       else 
         do { (I.Icmp op ta v1a v2a) <- convert_to_Icmp convert_Value a
            ; return $ I.I_icmp op ta v1a v2a (sLid lhs)
            }
     }
convert_Expr_CInst (Just lhs, A.ExprFcmp a@(A.Fcmp _ t _ _)) = 
  do { mp <- typeDefs
     ; if isTvector mp t then 
         do { (I.Fcmp op ta v1a v2a) <- convert_to_Fcmp_V convert_Value a
            ; return $ I.I_fcmp_V op ta v1a v2a (sLid lhs)
            }
       else 
         do { (I.Fcmp op ta v1a v2a) <- convert_to_Fcmp convert_Value a
            ; return $ I.I_fcmp op ta v1a v2a (sLid lhs)
            }
     }
convert_Expr_CInst (Just lhs, A.ExprBinExpr (A.Ie a@(A.IbinExpr _ _ t _ _))) = 
  do { mp <- typeDefs
     ; if not $ isTvector mp t then 
         do { x <- convert_to_Binexp convert_Value a 
            ; let y = case x of
                    Add n ta v1a v2a -> I.I_add n ta v1a v2a (sLid lhs)
                    Sub n ta v1a v2a -> I.I_sub n ta v1a v2a (sLid lhs) 
                    Mul n ta v1a v2a -> I.I_mul n ta v1a v2a (sLid lhs)
                    Udiv n ta v1a v2a -> I.I_udiv n ta v1a v2a (sLid lhs)
                    Sdiv n ta v1a v2a -> I.I_sdiv n ta v1a v2a (sLid lhs)
                    Urem ta v1a v2a -> I.I_urem ta v1a v2a (sLid lhs)
                    Srem ta v1a v2a -> I.I_srem ta v1a v2a (sLid lhs)
                    Shl n ta v1a v2a -> I.I_shl n ta v1a v2a (sLid lhs)
                    Lshr n ta v1a v2a -> I.I_lshr n ta v1a v2a (sLid lhs)
                    Ashr n ta v1a v2a -> I.I_ashr n ta v1a v2a (sLid lhs)
                    And ta v1a v2a -> I.I_and ta v1a v2a (sLid lhs)
                    Or ta v1a v2a -> I.I_or ta v1a v2a (sLid lhs)
                    Xor ta v1a v2a -> I.I_xor ta v1a v2a (sLid lhs)
            ; return y 
            }
       else 
         do { x <- convert_to_Binexp_V convert_Value a 
            ; let y = case x of
                    Add n ta v1a v2a -> I.I_add_V n ta v1a v2a (sLid lhs)
                    Sub n ta v1a v2a -> I.I_sub_V n ta v1a v2a (sLid lhs)
                    Mul n ta v1a v2a -> I.I_mul_V n ta v1a v2a (sLid lhs)
                    Udiv n ta v1a v2a -> I.I_udiv_V n ta v1a v2a (sLid lhs)
                    Sdiv n ta v1a v2a -> I.I_sdiv_V n ta v1a v2a (sLid lhs)
                    Urem ta v1a v2a -> I.I_urem_V ta v1a v2a (sLid lhs)
                    Srem ta v1a v2a -> I.I_srem_V ta v1a v2a (sLid lhs)
                    Shl n ta v1a v2a -> I.I_shl_V n ta v1a v2a (sLid lhs)
                    Lshr n ta v1a v2a -> I.I_lshr_V n ta v1a v2a (sLid lhs)
                    Ashr n ta v1a v2a -> I.I_ashr_V n ta v1a v2a (sLid lhs)
                    And ta v1a v2a -> I.I_and_V ta v1a v2a (sLid lhs)
                    Or ta v1a v2a -> I.I_or_V ta v1a v2a (sLid lhs)
                    Xor ta v1a v2a -> I.I_xor_V ta v1a v2a (sLid lhs)
            ; return y 
            }
     }
convert_Expr_CInst (Just lhs, A.ExprBinExpr (A.Fe a@(A.FbinExpr _ _ t _ _))) = 
  do { mp <- typeDefs
     ; if not $ isTvector mp t then 
       do { x <- convert_to_FBinexp convert_Value a
          ; let y = case x of
                  Fadd n ta v1a v2a -> I.I_fadd n ta v1a v2a (sLid lhs)
                  Fsub n ta v1a v2a -> I.I_fsub n ta v1a v2a (sLid lhs)
                  Fmul n ta v1a v2a -> I.I_fmul n ta v1a v2a (sLid lhs)
                  Fdiv n ta v1a v2a -> I.I_fdiv n ta v1a v2a (sLid lhs)
                  Frem n ta v1a v2a -> I.I_frem n ta v1a v2a (sLid lhs)
          ; return y 
          }
     else
       do { x <- convert_to_FBinexp_V convert_Value a
          ; let y = case x of
                  Fadd n ta v1a v2a -> I.I_fadd_V n ta v1a v2a (sLid lhs)
                  Fsub n ta v1a v2a -> I.I_fsub_V n ta v1a v2a (sLid lhs)
                  Fmul n ta v1a v2a -> I.I_fmul_V n ta v1a v2a (sLid lhs)
                  Fdiv n ta v1a v2a -> I.I_fdiv_V n ta v1a v2a (sLid lhs)
                  Frem n ta v1a v2a -> I.I_frem_V n ta v1a v2a (sLid lhs)
          ; return y
          }
   }
convert_Expr_CInst (Just lhs, A.ExprConversion a) =  
  do { mp <- typeDefs
     ; if not $ conversionIsTvector mp a then 
         do { x <- convert_to_Conversion convert_Value a
            ; let y = case x of
                   I.Trunc tv dt -> I.I_trunc tv dt (sLid lhs)
                   I.Zext tv dt -> I.I_zext tv dt (sLid lhs)
                   I.Sext tv dt -> I.I_sext tv dt (sLid lhs)
                   I.FpTrunc tv dt -> I.I_fptrunc tv dt (sLid lhs)
                   I.FpExt tv dt -> I.I_fpext tv dt (sLid lhs)
                   I.FpToUi tv dt -> I.I_fptoui tv dt (sLid lhs)
                   I.FpToSi tv dt -> I.I_fptosi tv dt (sLid lhs)
                   I.UiToFp tv dt -> I.I_uitofp tv dt (sLid lhs)
                   I.SiToFp tv dt -> I.I_sitofp tv dt (sLid lhs)
                   I.PtrToInt tv dt -> I.I_ptrtoint tv dt (sLid lhs)
                   I.IntToPtr tv dt -> I.I_inttoptr tv dt (sLid lhs)
                   I.Bitcast tv@(I.T st v) dt -> case (st, dt) of
                     (I.DtypeScalarP sta, I.DtypeScalarP dta) -> I.I_bitcast (I.T sta v) dta (sLid lhs)
                     (_,_) -> I.I_bitcast_D tv dt (sLid lhs)
                   I.AddrSpaceCast tv dt -> I.I_addrspacecast tv dt (sLid lhs)
           ; return y 
           }
       else 
         do { x <- convert_to_Conversion_V convert_Value a
            ; let y = case x of
                   I.Trunc tv dt -> I.I_trunc_V tv dt (sLid lhs)
                   I.Zext tv dt -> I.I_zext_V tv dt (sLid lhs)
                   I.Sext tv dt -> I.I_sext_V tv dt (sLid lhs)
                   I.FpTrunc tv dt -> I.I_fptrunc_V tv dt (sLid lhs)
                   I.FpExt tv dt -> I.I_fpext_V tv dt (sLid lhs)
                   I.FpToUi tv dt -> I.I_fptoui_V tv dt (sLid lhs)
                   I.FpToSi tv dt -> I.I_fptosi_V tv dt (sLid lhs)
                   I.UiToFp tv dt -> I.I_uitofp_V tv dt (sLid lhs)
                   I.SiToFp tv dt -> I.I_sitofp_V tv dt (sLid lhs)
                   I.PtrToInt tv dt -> I.I_ptrtoint_V tv dt (sLid lhs)
                   I.IntToPtr tv dt -> I.I_inttoptr_V tv dt (sLid lhs)
                   I.Bitcast tv@(I.T st v) dt -> case (st, dt) of
                     (I.DtypeScalarP sta, I.DtypeScalarP dta) -> I.I_bitcast (I.T sta v) dta (sLid lhs)
                     (_,_) -> I.I_bitcast_D tv dt (sLid lhs)
                   I.AddrSpaceCast tv dt -> I.I_addrspacecast_V tv dt (sLid lhs)
           ; return y
           }
     }
convert_Expr_CInst (Just lhs, A.ExprSelect a@(A.Select _ (A.Typed t _) _)) = 
  do { mp <- typeDefs
     ; case matchType mp t of
       Tk_ScalarI -> do { (I.Select (Left cnd) t f) <- convert_to_Select_I convert_Value a
                        ; return $ I.I_select_I cnd t f (sLid lhs)
                        }
       Tk_ScalarF -> do { (I.Select (Left cnd) t f) <- convert_to_Select_F convert_Value a
                        ; return $ I.I_select_F cnd t f (sLid lhs)
                        }
       Tk_ScalarP -> do { (I.Select (Left cnd) t f) <- convert_to_Select_P convert_Value a
                        ; return $ I.I_select_P cnd t f (sLid lhs)
                        }
       Tk_RecordD -> do { (I.Select (Left cnd) t f) <- convert_to_Select_Record convert_Value a
                        ; return $ I.I_select_First cnd t f (sLid lhs)
                        }
       Tk_VectorI -> do { (I.Select cnd t f) <- convert_to_Select_VI convert_Value a
                        ; return $ I.I_select_VI cnd t f (sLid lhs)
                        }
       Tk_VectorF -> do { (I.Select cnd t f) <- convert_to_Select_VF convert_Value a
                        ; return $ I.I_select_VF cnd t f (sLid lhs)
                        }
       Tk_VectorP -> do { (I.Select cnd t f) <- convert_to_Select_VP convert_Value a
                        ; return $ I.I_select_VP cnd t f (sLid lhs)
                        }
     }


convert_MemOp :: (Maybe A.LocalId, A.MemOp) -> MM (I.Cinst I.Gname)
convert_MemOp (mlhs, c) = case (mlhs, c) of
  (Just lhs, A.Alloca mar t mtv ma) -> 
    do { mp <- typeDefs
       ; ti <- convert_Type_Dtype FLC t
       ; mtvi <- maybeM (convert_to_TypedValue_SI FLC) mtv
       ; return (I.I_alloca mar ti mtvi ma (sLid lhs))
       }
  (Just lhs, A.Load atom (A.Pointer tv) aa nonterm inv nonul) -> 
    do { tvi <- convert_to_TypedAddrValue FLC tv
       ; return (I.I_load atom tvi aa nonterm inv nonul (sLid lhs))
       }
  (Just lhs, A.LoadAtomic  at v (A.Pointer tv) aa) -> 
    do { tvi <- convert_to_TypedAddrValue FLC tv
       ; return (I.I_loadatomic at v tvi aa (sLid lhs))
       }
  (Nothing, A.Store atom tv1 (A.Pointer tv2) aa nt) -> 
    do { tv1a <- convert_to_DtypedValue tv1
       ; tv2a <- convert_to_TypedAddrValue FLC tv2
       ; return $ I.I_store atom tv1a tv2a aa nt
       }
  (Nothing, A.StoreAtomic atom v tv1 (A.Pointer tv2) aa) -> 
    do { tv1a <- convert_to_DtypedValue tv1
       ; tv2a <- convert_to_TypedAddrValue FLC tv2
       ; return $ I.I_storeatomic atom v tv1a tv2a aa
       }
  (Nothing, A.Fence  b fo) -> return $ I.I_fence b fo
  (Just lhs, A.CmpXchg wk b1 (A.Pointer tv1) tv2@(A.Typed t2 _) tv3 b2 mf ff) -> 
    do { mp <- typeDefs
       ; tv1a <- convert_to_TypedAddrValue FLC tv1
       ; case matchType mp t2 of
         Tk_ScalarI -> do { tv2a <- convert_to_TypedValue_SI FLC tv2
                          ; tv3a <- convert_to_TypedValue_SI FLC tv3
                          ; return $ I.I_cmpxchg_I wk b1 tv1a tv2a tv3a b2 mf ff (sLid lhs)
                          }
         Tk_ScalarF -> do { tv2a <- convert_to_TypedValue_SF FLC tv2
                          ; tv3a <- convert_to_TypedValue_SF FLC tv3
                          ; return $ I.I_cmpxchg_F wk b1 tv1a tv2a tv3a b2 mf ff (sLid lhs)
                          }
         Tk_ScalarP -> do { tv2a <- convert_to_TypedValue_SP FLC tv2
                          ; tv3a <- convert_to_TypedValue_SP FLC tv3
                          ; return $ I.I_cmpxchg_P wk b1 tv1a tv2a tv3a b2 mf ff (sLid lhs)
                          }
       }
  (Just lhs, A.AtomicRmw b1 op (A.Pointer tv1) tv2 b2 mf) -> 
    do { tv1a <- convert_to_TypedAddrValue FLC tv1
       ; tv2a <- convert_to_TypedValue_SI FLC tv2
       ; return $ I.I_atomicrmw b1 op tv1a tv2a b2 mf (sLid lhs)
       }
  (_,_) -> error $ "AstIrConversion:irrefutable lhs:" ++ show mlhs ++ " rhs:" ++ show c

convert_to_DtypedValue :: A.Typed A.Value -> MM (I.T I.Dtype (I.Value I.Gname))
convert_to_DtypedValue (A.Typed t v) = do { mp <- typeDefs
                                          ; let (ti::I.Dtype) = dcast FLC ((tconvert mp t)::I.Utype)
                                          ; vi <- convert_Value v 
                                          ; return $ I.T ti vi
                                          }
                                              
convert_to_DtypedConst :: A.Typed A.Const -> MM (I.T I.Dtype (I.Const I.Gname))
convert_to_DtypedConst (A.Typed t v) = do { mp <- typeDefs
                                          ; let (ti::I.Dtype) = dcast FLC ((tconvert mp t)::I.Utype)
                                          ; vi <- convert_Const v 
                                          ; return $ I.T ti vi
                                          }


convert_to_TypedValue_SI :: FileLoc -> A.Typed A.Value -> MM (I.T (I.Type I.ScalarB I.I) (I.Value I.Gname))
convert_to_TypedValue_SI lc (A.Typed t v) = do { mp <- typeDefs
                                               ; let (ti::I.Type I.ScalarB I.I) = dcast lc ((tconvert mp t)::I.Utype)
                                               ; vi <- convert_Value v
                                               ; return $ I.T ti vi
                                               }

convert_to_TypedValue_SF :: FileLoc -> A.Typed A.Value -> MM (I.T (I.Type I.ScalarB I.F) (I.Value I.Gname))
convert_to_TypedValue_SF lc (A.Typed t v) = do { mp <- typeDefs
                                               ; let (ti::I.Type I.ScalarB I.F) = dcast lc ((tconvert mp t)::I.Utype)
                                               ; vi <- convert_Value v
                                               ; return $ I.T ti vi
                                               }

convert_to_TypedValue_SP :: FileLoc -> A.Typed A.Value -> MM (I.T (I.Type I.ScalarB I.P) (I.Value I.Gname))
convert_to_TypedValue_SP lc (A.Typed t v) = do { mp <- typeDefs
                                               ; let (ti::I.Type I.ScalarB I.P) = dcast lc ((tconvert mp t)::I.Utype)
                                               ; vi <- convert_Value v
                                               ; return $ I.T ti vi
                                               }

convert_to_TypedAddrValue :: FileLoc -> A.Typed A.Value -> MM (I.T (I.Type I.ScalarB I.P) (I.Value I.Gname))
convert_to_TypedAddrValue lc (A.Typed t v) = do { mp <- typeDefs
                                                ; let (ti::I.Type I.ScalarB I.P) = dcast lc ((tconvert mp t)::I.Utype)
                                                ; vi <- convert_Value v
                                                ; return $ I.T ti vi
                                                }

convert_Type_Dtype :: FileLoc -> A.Type -> MM I.Dtype
convert_Type_Dtype lc t = do { mp <- typeDefs
                             ; return $ dcast lc ((tconvert mp t)::I.Utype)
                             }


convert_Rhs :: (Maybe A.LocalId, A.Rhs) -> MM (I.Node I.Gname () H.O H.O)
convert_Rhs (mlhs, A.RhsMemOp c) = Md.liftM (\x -> I.Cnode x []) (convert_MemOp (mlhs, c))
convert_Rhs (mlhs, A.RhsExpr e) = Md.liftM (\x -> I.Cnode x []) (convert_Expr_CInst (mlhs, e))
convert_Rhs (lhs, A.RhsCall b cs) = 
  case specializeRegisterIntrinsic lhs cs of
    Just (Just r, ml, [m]) -> do { ma <- convert_MetaParam m 
                                 ; case ma of
                                   I.MetaOperandMeta mc -> return $ I.Cnode (I.I_llvm_read_register ml mc (sLid r)) []
                                   _ -> errorLoc FLC $ show ma
                                 }
    Just (Nothing, ml, [m,v]) -> do { ma <- convert_MetaParam m
                                    ; (I.FunOperandData _ _ _ va) <- convert_ActualParam v
                                    ; case ma of
                                      I.MetaOperandMeta mc -> return $ I.Cnode (I.I_llvm_write_register ml mc va) []
                                      _ -> errorLoc FLC $ show ma
                                    }
    Nothing -> 
      do { mc <- convert_to_Minst cs
         ; case mc of
           Just mi | lhs == Nothing -> return $ I.Mnode mi []
           Just _ -> errorLoc FLC $ show lhs ++ " " ++ show cs
           Nothing -> 
             Md.liftM (\x -> I.Cnode x []) $ 
             do { (isvoid, fnptr, csi) <- convert_to_CallFunInterface b cs
                ; return $ maybe (I.I_call_fun fnptr csi (fmap sLid lhs)) id (specializeCallSite (fmap sLid lhs) fnptr csi)
                }
         }
convert_Rhs (lhs, A.RhsInlineAsm cs) = 
  Md.liftM (\x -> I.Cnode x []) $ 
  do { (isvoid, asm, csi) <- convert_to_CallAsmInterface cs
     ; return $ I.I_call_asm asm csi (fmap sLid lhs)
     }
convert_Rhs (Just lhs, A.RhsVaArg (A.VaArg tv t)) = 
  do { tvi <- convert_to_DtypedValue tv
     ; ti <- convert_Type_Dtype FLC t
     ; return (I.Cnode (I.I_va_arg tvi ti (sLid lhs)) [])
     }
convert_Rhs (Just lhs, A.RhsLandingPad (A.LandingPad t1 t2 pf b cs)) = 
  do { pfi <- convert_FunPtr pf
     ; csi <- mapM convert_Clause cs
     ; t1i <- convert_Type_Dtype FLC t1
     ; t2i <- convert_Type_Dtype FLC t2
     ; return (I.Cnode (I.I_landingpad t1i t2i pfi b csi (sLid lhs)) [])
     }
convert_Rhs (Just lhs, A.RhsExtractElement a@(A.ExtractElement (A.Typed t1 _) _)) = 
  do { mp <- typeDefs
     ; case matchType mp t1 of
       Tk_VectorI -> do { (I.ExtractElement vec idx) <- convert_to_ExtractElement_I convert_Value a
                        ; return (I.Cnode (I.I_extractelement_I vec idx (sLid lhs)) [])
                        }
       Tk_VectorF -> do { (I.ExtractElement vec idx) <- convert_to_ExtractElement_F convert_Value a
                        ; return (I.Cnode (I.I_extractelement_F vec idx (sLid lhs)) [])
                        }
       Tk_VectorP -> do { (I.ExtractElement vec idx) <- convert_to_ExtractElement_P convert_Value a
                        ; return (I.Cnode (I.I_extractelement_P vec idx (sLid lhs)) [])
                        }
     }
convert_Rhs (Just lhs, A.RhsInsertElement a@(A.InsertElement (A.Typed t1 _) _ _)) = 
  do { mp <- typeDefs
     ; case matchType mp t1 of
       Tk_VectorI -> do { (I.InsertElement vec val idx) <- convert_to_InsertElement_I convert_Value a
                        ; return (I.Cnode (I.I_insertelement_I vec val idx (sLid lhs)) [])
                        }
       Tk_VectorF -> do { (I.InsertElement vec val idx) <- convert_to_InsertElement_F convert_Value a
                        ; return (I.Cnode (I.I_insertelement_F vec val idx (sLid lhs)) [])
                        }
       Tk_VectorP -> do { (I.InsertElement vec val idx) <- convert_to_InsertElement_P convert_Value a
                        ; return (I.Cnode (I.I_insertelement_P vec val idx (sLid lhs)) [])
                        }
     }
convert_Rhs (Just lhs, A.RhsShuffleVector a@(A.ShuffleVector (A.Typed t _) _ _)) = 
  do { mp <- typeDefs
     ; case matchType mp t of
       Tk_VectorI -> do { (I.ShuffleVector tv1a tv2a tv3a) <- convert_to_ShuffleVector_I convert_Value a
                        ; return (I.Cnode (I.I_shufflevector_I tv1a tv2a tv3a (sLid lhs)) [])
                        }
       Tk_VectorF -> do { (I.ShuffleVector tv1a tv2a tv3a) <- convert_to_ShuffleVector_F convert_Value a
                        ; return (I.Cnode (I.I_shufflevector_F tv1a tv2a tv3a (sLid lhs)) [])
                        }
       Tk_VectorP -> do { (I.ShuffleVector tv1a tv2a tv3a) <- convert_to_ShuffleVector_P convert_Value a
                        ; return (I.Cnode (I.I_shufflevector_P tv1a tv2a tv3a (sLid lhs)) [])
                        }
     }
convert_Rhs (Just lhs, A.RhsExtractValue a) = 
  do { (I.ExtractValue blocka idxa) <- convert_to_ExtractValue convert_Value a
     ; return (I.Cnode (I.I_extractvalue blocka idxa (sLid lhs)) [])
     }
convert_Rhs (Just lhs, A.RhsInsertValue a) = 
  do { (I.InsertValue blocka va idxa) <- convert_to_InsertValue convert_Value a
     ; return (I.Cnode (I.I_insertvalue blocka va idxa (sLid lhs)) [])
     }
convert_Rhs (lhs,rhs) =  errorLoc FLC $ "AstIrConversion:irrefutable error lhs:" ++ show lhs ++ " rhs:" ++ show rhs



specializeFirstParamAsRet :: A.ActualParam -> MM (Maybe (I.FunOperand (I.Value I.Gname)))
specializeFirstParamAsRet x = case x of
  (A.ActualParamData t pa v) ->
    do { mp <- typeDefs
       ; let (ta::I.Utype) = tconvert mp t
       ; va <- convert_Value v
       ; let (plist, bl) = stripOffPa pa sRetByValSignExtZeroExtAlignPreds
       ; case bl of 
         [Nothing, _, Nothing, Nothing, _] -> return Nothing
         [Just _, Nothing, Nothing, Nothing, pa] -> return $ Just $ I.FunOperandAsRet (dcast FLC ta) plist (mapPaAlign pa) va
         [Just _, Just _, _, _, Just (A.PaAlign n)] -> errorLoc FLC "byval cannot be used with sret"
       }
  (A.ActualParamLabel t pa v) ->
    do { mp <- typeDefs
       ; let (ta::I.Utype) = tconvert mp t
       ; va <- convert_PercentLabel v
       ; case ta of
         I.UtypeLabelX lbl -> return Nothing
       }   
  A.ActualParamMeta mc -> errorLoc FLC $ show x ++ " is passed to convert_ActualParam"


convert_ActualParam :: A.ActualParam -> MM (I.FunOperand (I.Value I.Gname))
convert_ActualParam x = case x of
  (A.ActualParamData t pa v) ->
    do { mp <- typeDefs
       ; let (ta::I.Utype) = tconvert mp t
       ; va <- convert_Value v
       ; let (plist, bl) = stripOffPa pa sRetByValSignExtZeroExtAlignPreds
       ; case bl of 
         [Just _, Nothing, Nothing, Nothing, pn] -> return $ I.FunOperandAsRet (dcast FLC ta) plist (mapPaAlign pn) va
         [Nothing, Just _, Nothing, Nothing, pn] -> return $ I.FunOperandByVal (dcast FLC ta) plist (mapPaAlign pn) va
         [Nothing, Nothing, Just _ , Nothing, pn] -> return $ I.FunOperandExt I.Sign (dcast FLC ta) plist (mapPaAlign pn) va
         [Nothing, Nothing, Nothing, Just _ , pn] -> return $ I.FunOperandExt I.Zero (dcast FLC ta) plist (mapPaAlign pn) va
         [Nothing, Nothing, Nothing, Nothing, pn] -> return $ I.FunOperandData (dcast FLC ta) plist (mapPaAlign pn) va
       }
  (A.ActualParamLabel t pa v) ->
    do { mp <- typeDefs
       ; let (ta::I.Utype) = tconvert mp t
       ; va <- convert_PercentLabel v
       ; let preds = [\x -> case x of 
                         A.PaAlign _ -> True
                         _ -> False]
       ; let (plist, [pn]) = stripOffPa pa preds
       ; case ta of
         I.UtypeLabelX lbl -> return $ I.FunOperandLabel lbl plist (mapPaAlign pn) (I.Val_const $ I.C_labelId va)
       }    
  A.ActualParamMeta mc -> errorLoc FLC $ show x ++ " is passed to convert_ActualParam"

isMetaParam :: A.ActualParam -> Bool
isMetaParam x = case x of
  A.ActualParamMeta _ -> True
  _ -> False
  
isFormalMetaParam :: A.FormalParam -> Bool
isFormalMetaParam x = case x of
  A.FormalParamMeta _ _ -> True
  _ -> False

convert_MetaParam :: A.ActualParam -> MM (I.MetaOperand I.Gname)
convert_MetaParam x = case x of
  A.ActualParamMeta mc -> Md.liftM I.MetaOperandMeta (convert_MetaKindedConst mc)
  A.ActualParamData t pa v ->
    do { mp <- typeDefs
       ; let (ta::I.Utype) = tconvert mp t
       ; va <- convert_Value v
       ; case ta of
         I.UtypeLabelX lbl -> errorLoc FLC $ show x
         _ -> return $ I.MetaOperandData (dcast FLC ta) pa (Nothing) va
       }
  _ -> errorLoc FLC $ show x ++ " is passed to convert_MetaParam"


convert_constToAliasee :: A.Const -> MM (I.Aliasee I.Gname)
convert_constToAliasee (A.C_simple (A.CpGlobalAddr v)) = return $ I.Aliasee $ specializeGlobalId v
convert_constToAliasee (A.C_conv cvt@(A.Conversion op src dt)) = convert_Aliasee (A.AliaseeConversion cvt)
convert_constToAliasee x = errorLoc FLC $ show x


convert_Aliasee :: A.Aliasee -> MM (I.Aliasee I.Gname)
convert_Aliasee ae = case ae of
  A.Aliasee (A.Typed t c) -> do { mp <- typeDefs
                                ; let (ta::I.Dtype) = dcast FLC ((tconvert mp t)::I.Utype)
                                ; ca <- convert_constToAliasee c 
                                ; return $ I.AliaseeTyped ta ca
                                }
  A.AliaseeConversion c@(A.Conversion _ _ dt) -> 
    do { mp <- typeDefs
       ; if isTvector mp dt then errorLoc FLC $ show dt
         else Md.liftM I.AliaseeConversion (convert_to_Conversion convert_constToAliasee c)
       }
  A.AliaseeGetElementPtr a -> 
    do { mp <- typeDefs
       ; if getElemPtrIsTvector mp a then errorLoc FLC $ show ae
         else Md.liftM I.AliaseeGEP (convert_to_GetElementPtr convert_constToAliasee convert_Const a)
       }

convert_Prefix :: A.Prefix -> MM (I.Prefix I.Gname)
convert_Prefix (A.Prefix n) = Md.liftM I.Prefix (convert_TypedConstOrNUll n)

convert_Prologue :: A.Prologue -> MM (I.Prologue I.Gname)
convert_Prologue (A.Prologue n) = Md.liftM I.Prologue (convert_TypedConstOrNUll n)

convert_TypedConstOrNUll :: A.TypedConstOrNull -> MM (I.TypedConstOrNull I.Gname)
convert_TypedConstOrNUll x = case x of
  A.TypedConst (A.Typed t v) -> do { mp <- typeDefs
                                   ; vi <- convert_Const v
                                   ; let (ti::I.Dtype) = dcast FLC ((tconvert mp t)::I.Utype)
                                   ; return (I.TypedConst (I.T ti vi))
                                   }
  A.UntypedNull -> return I.UntypedNull


convert_to_FunParamType :: A.FormalParam -> MM (I.FunOperand ())
convert_to_FunParamType x = 
  do { mp <- typeDefs
     ; case x of
       (A.FormalParamData dt pa _) ->
         case stripOffPa pa sRetByValSignExtZeroExtAlignPreds of
           (palist, bl) -> case bl of
             [Just _,  Nothing, Nothing, Nothing, pa] -> 
               return $ I.FunOperandAsRet (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) ()
             [Nothing, Just _,  Nothing, Nothing, pa] -> 
               return $ I.FunOperandByVal (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) ()
             [Nothing, Nothing, Just _,  Nothing, pa] -> 
               return $ I.FunOperandExt I.Sign (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) ()
             [Nothing, Nothing, Nothing, Just _,  pa] -> 
               return $ I.FunOperandExt I.Zero (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) ()
             [Nothing, Nothing, Nothing, Nothing, pa] -> 
               return $ I.FunOperandData (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) ()
             _ -> errorLoc FLC $ show bl
       (A.FormalParamMeta mk fp) -> errorLoc FLC $ show x
     }


mapPaAlign pn = fmap (\(A.PaAlign n) -> I.AlignInByte n) pn


convert_to_FunParamTypeList :: [A.FormalParam] -> MM [I.FunOperand ()]
convert_to_FunParamTypeList l = 
  do { mp <- typeDefs
     ; la <- mapM convert_to_FunParamType l
     ; return la
     }
  
composeTfunction :: (A.Type, [A.RetAttr]) -> [(A.Type, [A.ParamAttr])] -> Maybe A.VarArgParam -> MM (I.Type I.CodeFunB I.X)
composeTfunction (ret, retAttrs) params mv = 
  do { mp <- typeDefs
     ; ts <- mapM (\(dt, pa) -> 
                    do { let (dt0::I.Utype) = tconvert mp dt
                       ; let (plist, bl) = stripOffPa pa sRetByValSignExtZeroExtAlignPreds
                       ; case bl of
                            [Just _,  Nothing, Nothing, Nothing, pn] -> return (I.MtypeAsRet (dcast FLC dt0), mapPaAlign pn)
                            [Nothing, Just _,  Nothing, Nothing, pn] -> return (I.MtypeByVal (dcast FLC dt0), mapPaAlign pn)
                            [Nothing, Nothing, Just _,  Nothing, pn] -> return (I.MtypeExt I.Sign (dcast FLC dt0), mapPaAlign pn)
                            [Nothing, Nothing, Nothing, Just _, pn] -> return (I.MtypeExt I.Zero (dcast FLC dt0), mapPaAlign pn)
                            [Nothing, Nothing, Nothing, Nothing, pn] -> case dt0 of
                              I.UtypeLabelX lt -> return (I.MtypeLabel lt, mapPaAlign pn)
                              _ -> return (I.MtypeData (dcast FLC dt0), mapPaAlign pn)
                            _ -> errorLoc FLC $ show bl
                       }
                  ) params
     ; let (ret0::I.Utype) = tconvert mp ret
     ; return $ I.Tfunction (dcast FLC ret0, retAttrs) ts mv
     }

sRetByValSignExtZeroExtAlignPreds = [ (A.PaSRet==)
                                    , (A.PaByVal==)
                                    , (A.PaSignExt==)
                                    , (A.PaZeroExt==)
                                    , \x -> case x of 
                                         A.PaAlign _ -> True
                                         _ -> False
                                    ]
        
  
convert_FunctionDeclareType :: A.FunctionPrototype -> MM (I.FunctionDeclare I.Gname)
convert_FunctionDeclareType  (A.FunctionPrototype { A.fp_linkage = f0 
                                                  , A.fp_visibility = f1 
                                                  , A.fp_dllstorage = f2 
                                                  , A.fp_callConv = f3 
                                                  , A.fp_retAttrs = f4 
                                                  , A.fp_retType = f5 
                                                  , A.fp_fun_name = f6 
                                                  , A.fp_param_list = f7@(A.FormalParamList fpl mv) 
                                                  , A.fp_addr_naming = f8 
                                                  , A.fp_fun_attrs = f9 
                                                  , A.fp_section = f10 
                                                  , A.fp_comdat = f10a 
                                                  , A.fp_alignment = f11 
                                                  , A.fp_gc = f12 
                                                  , A.fp_prefix = f13 
                                                  , A.fp_prologue = f14}) =
  if not (any isFormalMetaParam fpl) then
    do { f13a <- maybeM convert_Prefix f13
       ; f14a <- maybeM convert_Prologue f14
       ; f7a <- convert_to_FunParamTypeList fpl
       ; ft <- composeTfunction (f5, fmap specializeRetAttr f4) 
               (fmap (\x -> case x of
                         A.FormalParamData dt pa _ -> (dt, pa)
                         _ -> errorLoc FLC $ show x
                     ) fpl) mv
       ; return $ I.FunctionDeclareData { I.fd_linkage = f0 
                                        , I.fd_visibility = f1 
                                        , I.fd_dllstorage = f2 
                                        , I.fd_signature = I.FunSignature (maybe I.Ccc id f3) ft f7a 
                                        , I.fd_fun_name = specializeGlobalId f6
                                        , I.fd_addr_naming = f8 
                                        , I.fd_fun_attrs = f9 
                                        , I.fd_section = f10 
                                        , I.fd_comdat = fmap convert_Comdat f10a 
                                        , I.fd_alignment = f11
                                        , I.fd_gc = f12
                                        , I.fd_prefix = f13a 
                                        , I.fd_prologue = f14a
                                        }
       }
  else 
    do { mp <- typeDefs
       ; return $ I.FunctionDeclareMeta { I.fd_fun_name = specializeGlobalId f6
                                        , I.fd_retType = dcast FLC ((tconvert mp f5)::I.Utype)
                                        , I.fd_metakinds = 
                                          fmap (\x -> case x of
                                                   (A.FormalParamMeta m _) -> Left $ tconvert mp m
                                                   (A.FormalParamData dt _ _) -> Right $ (dcast FLC ((tconvert mp dt)::I.Utype))
                                               ) fpl
                                        , I.fd_fun_attrs = f9 
                                        }
       }

convert_to_FunParam :: A.FormalParam -> MM (I.FunOperand I.Lname)
convert_to_FunParam x = 
  do { mp <- typeDefs
     ; case x of
       (A.FormalParamData dt pa (A.FexplicitParam fp)) ->
         let (palist, bl) = stripOffPa pa sRetByValSignExtZeroExtAlignPreds
         in case bl of
           [Just _,  Nothing, Nothing, Nothing, pa] -> 
             return $ I.FunOperandAsRet (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) (sLid fp)
           [Nothing, Just _,  Nothing, Nothing, pa] -> 
             return $ I.FunOperandByVal (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) (sLid fp)
           [Nothing, Nothing,  Just _,  Nothing, pa] -> 
             return $ I.FunOperandExt I.Sign (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) (sLid fp)
           [Nothing, Nothing, Nothing, Just _,  pa] -> 
             return $ I.FunOperandExt I.Zero (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) (sLid fp)
           [Nothing, Nothing, Nothing, Nothing, pa] -> 
             return $ I.FunOperandData (dcast FLC ((tconvert mp dt)::I.Utype)) palist (mapPaAlign pa) (sLid fp)
           _ -> errorLoc FLC $ show bl
       (A.FormalParamData _ _ A.FimplicitParam) -> 
         errorLoc FLC "implicit param should be normalized in AsmSimplification"
     }
  
convert_FunctionInterface :: A.FunctionPrototype -> MM (I.FunctionInterface I.Gname)
convert_FunctionInterface  (A.FunctionPrototype f0 f1 f2 f3 f4 f5 f6 
                            f7@(A.FormalParamList plist mv) f8 f9 f10 f10a f11 f12 f13 f14) =
  do { mp <- typeDefs
     ; let (f5a::I.Rtype) = dcast FLC ((tconvert mp f5)::I.Utype)
     ; f13a <- maybeM convert_Prefix f13
     ; f14a <- maybeM convert_Prologue f14
     ; f7a <- mapM convert_to_FunParam plist
     ; ft <- composeTfunction (f5, fmap specializeRetAttr f4) 
             (fmap (\x -> case x of
                       A.FormalParamData dt pa _ -> (dt, pa)
                       _ -> errorLoc FLC $ show x
                   ) plist) mv
     ; return $ I.FunctionInterface { I.fi_linkage = f0 
                                    , I.fi_visibility = f1 
                                    , I.fi_dllstorage = f2
                                    , I.fi_signature = I.FunSignature (maybe I.Ccc id f3) ft f7a
                                    , I.fi_fun_name = specializeGlobalId f6
                                    , I.fi_addr_naming = f8 
                                    , I.fi_fun_attrs = f9 
                                    , I.fi_section = f10 
                                    , I.fi_comdat = fmap convert_Comdat f10a 
                                    , I.fi_alignment = f11 
                                    , I.fi_gc = f12 
                                    , I.fi_prefix = f13a 
                                    , I.fi_prologue = f14a
                                    }
     }

convert_PhiInst :: A.PhiInst -> MM (I.Pinst I.Gname)
convert_PhiInst phi@(A.PhiInst mg t branches) = 
  do { mp <- typeDefs
     ; branchesa <- mapM (pairM convert_Value convert_PercentLabel) branches             
     ; let (ta::I.Utype) = tconvert mp t
     ; let (tab::I.Ftype) = case ta of 
             I.UtypeRecordD e -> dcast FLC (squeeze FLC e)
             _ -> dcast FLC ta 
     ; case mg of 
       Just lhs -> return $ I.Pinst tab (fmap (\x -> (fst x, snd x)) branchesa) (sLid lhs)
       Nothing -> errorLoc FLC $ "unused phi" ++ show phi
     }

convert_CInst :: A.ComputingInst -> MM (I.Node I.Gname () H.O H.O)
convert_CInst (A.ComputingInst mg rhs) = convert_Rhs (mg, rhs) 

convert_TerminatorInst :: A.TerminatorInst -> MM (I.Tinst I.Gname)
convert_TerminatorInst (A.RetVoid) = return I.T_ret_void
convert_TerminatorInst (A.Return tvs) = Md.liftM I.T_return (mapM convert_to_DtypedValue tvs)
convert_TerminatorInst (A.Br t) = Md.liftM I.T_br (convert_TargetLabel t)
convert_TerminatorInst (A.Cbr cnd t f) = 
  Md.liftM3 I.T_cbr (convert_Value cnd) (convert_TargetLabel t) 
  (convert_TargetLabel f)
convert_TerminatorInst (A.IndirectBr cnd bs) = 
  Md.liftM2 I.T_indirectbr (convert_to_TypedAddrValue FLC cnd) (mapM convert_TargetLabel bs)
convert_TerminatorInst (A.Switch cnd d cases) = 
  do { dc <- convert_to_TypedValue_SI FLC cnd
     ; dl <- convert_TargetLabel d
     ; other <- mapM (pairM (convert_to_TypedValue_SI FLC) convert_TargetLabel) cases
     ; return $ I.T_switch (dc, dl) other
     }
convert_TerminatorInst (A.Invoke mg cs t f) = 
  do { (_, fptr, csa) <- convert_to_InvokeFunInterface cs
     ; ta <- convert_TargetLabel t
     ; fa <- convert_TargetLabel f
     ; return $ I.T_invoke fptr csa ta fa (fmap sLid mg)
     }
convert_TerminatorInst (A.InvokeInlineAsm mg cs t f) = 
  do { (_, asm, csa) <- convert_to_CallAsmInterface cs
     ; ta <- convert_TargetLabel t
     ; fa <- convert_TargetLabel f
     ; return $ I.T_invoke_asm asm csa ta fa (fmap sLid mg)
     }  
convert_TerminatorInst (A.Resume tv) = Md.liftM I.T_resume (convert_to_DtypedValue tv)
convert_TerminatorInst A.Unreachable = return I.T_unreachable
convert_TerminatorInst A.Unwind = return I.T_unwind

convert_Dbg :: A.Dbg -> MM (I.Dbg I.Gname)
convert_Dbg (A.Dbg mv mc) = Md.liftM2 I.Dbg (convert_MdRef mv) (convert_MetaConst mc)

convert_PhiInstWithDbg :: A.PhiInstWithDbg -> MM (I.Pinst I.Gname, [I.Dbg I.Gname])
convert_PhiInstWithDbg (A.PhiInstWithDbg ins dbgs) = 
  do { ins0 <- convert_PhiInst ins 
     ; dbgs0 <- mapM convert_Dbg dbgs
     ; return (ins0, dbgs0)
     }

convert_CInstWithDbg :: A.ComputingInstWithDbg -> MM (I.Node I.Gname () H.O H.O)
convert_CInstWithDbg (A.ComputingInstWithDbg ins dbgs) = 
  do { ins0 <- convert_CInst ins 
     ; dbgs0 <- mapM convert_Dbg dbgs
     ; case ins0 of
       I.Cnode n _ -> return $ I.Cnode n dbgs0
       I.Mnode n _ -> return $ I.Mnode n dbgs0
     }
  
convert_TerminatorInstWithDbg :: A.TerminatorInstWithDbg -> MM (I.Tinst I.Gname, [I.Dbg I.Gname])
convert_TerminatorInstWithDbg (A.TerminatorInstWithDbg term dbgs) = 
  do { term0 <- convert_TerminatorInst term 
     ; dbgs0 <- mapM convert_Dbg dbgs
     ; return (term0, dbgs0)
     }


toSingleNodeGraph :: A.Block -> MM (H.Graph (I.Node I.Gname ()) H.C H.C)
toSingleNodeGraph (A.Block f  phi ms l) =
  do { f'  <- toFirst f
     ; phi' <- mapM toPhi phi
     ; ms' <- mapM toMid ms
     ; l'  <- toLast l
     ; return $ H.mkFirst f' H.<*> H.mkMiddles phi' H.<*> H.mkMiddles ms' H.<*> H.mkLast l'
     }

toFirst :: A.BlockLabel -> MM (I.Node I.Gname () H.C H.O)
toFirst x = Md.liftM I.Lnode (convert_BlockLabel x)

toPhi :: A.PhiInstWithDbg -> MM (I.Node I.Gname () H.O H.O)
toPhi phi = Md.liftM (uncurry I.Pnode) (convert_PhiInstWithDbg phi)

toMid :: A.ComputingInstWithDbg -> MM (I.Node I.Gname () H.O H.O)
toMid inst = convert_CInstWithDbg inst

toLast :: A.TerminatorInstWithDbg -> MM (I.Node I.Gname () H.O H.C)
toLast inst = Md.liftM (uncurry I.Tnode) (convert_TerminatorInstWithDbg inst)

-- | the head must be the entry block
getEntryAndAlist :: [A.Block] -> MM (H.Label, [A.LabelId])
getEntryAndAlist [] = error "Parsed procedures should not be empty"
getEntryAndAlist bs =
  do { l <- convert_BlockLabel $ A.blockLabel $ head bs
     ; let ord = map (\b -> case A.blockLabel b of
                         A.ImplicitBlockLabel p -> error $ "irrefutable implicitblock " 
                                                   ++ show p ++ " should be normalized in AstSimplify" 
                         A.ExplicitBlockLabel x -> x 
                     ) bs
     ; return (l, ord)
     }

toGraph :: [A.Block] -> MM (H.Graph (I.Node I.Gname ()) H.C H.C)
toGraph bs =
  {-
    It's more likely that only reachable blocks are pulled out and used to create
    a graph, the unreachable blocks are left.
  -}
  do { g <- foldl (Md.liftM2 (H.|*><*|)) (return H.emptyClosedGraph) (map toSingleNodeGraph bs)
     ; getBody g
     }

getBody :: forall n. H.Graph n H.C H.C -> MM (H.Graph n H.C H.C)
getBody graph = lift (LabelMapM f)
  where f m = return (m, graph)


blockToGraph :: A.FunctionPrototype -> [A.Block] -> MM (H.Label, H.Graph (I.Node I.Gname ()) H.C H.C)
blockToGraph fn blocks =
  do { (entry, labels) <- getEntryAndAlist blocks
     ; body <- toGraph blocks 
     ; return (entry, body)
     }
  

convert_TlAlias :: A.TlAlias -> MM (I.TlAlias I.Gname)
convert_TlAlias (A.TlAlias  g v dll tlm na l a) = 
  convert_Aliasee a >>= return . (I.TlAlias (specializeGlobalId g) v dll tlm na l)
  
convert_TlUnamedMd :: A.TlUnamedMd -> MM (I.TlUnamedMd I.Gname)
convert_TlUnamedMd (A.TlUnamedMd s tv) = do { mc <- convert_MetaKindedConst tv 
                                            ; return $ specializeUnamedMd (I.TlUnamedMd s mc) 
                                            }

convert_TlNamedMd :: A.TlNamedMd -> (MM I.TlNamedMd)
convert_TlNamedMd (A.TlNamedMd m ns) = do { nsa <- mapM convert_MdNode ns
                                          ; return $ I.TlNamedMd m nsa
                                          }
                               
convert_TlDeclare :: A.TlDeclare -> MM (I.TlDeclare I.Gname)
convert_TlDeclare (A.TlDeclare f) = convert_FunctionDeclareType f >>= return . I.TlDeclare
  
convert_TlDefine :: A.TlDefine -> MM (I.TlDefine I.Gname ())
convert_TlDefine  (A.TlDefine f b) = let (A.FunctionPrototype _ _ _ _ _ _ gid _ _ _ _ _ _ _ _ _) = f 
                                     in do { fa <- convert_FunctionInterface f
                                           ; (e, g) <- withFunName (specializeGlobalId gid) (blockToGraph f b)
                                           ; return $ I.TlDefine fa e g
                                           }

convert_TlGlobal :: A.TlGlobal -> MM (I.TlGlobal I.Gname)
convert_TlGlobal (A.TlGlobal a1 a2 a3 a4 a5 a6 a7 a8 a8a a9 a10 a11 a12 a13) =
  do { mp <- typeDefs
     ; let (a9a::I.Utype) = tconvert mp a9
     ; a10a <- maybeM convert_Const a10
     ; case a9a of 
       I.UtypeOpaqueD _ -> return $ I.TlGlobalOpaque (specializeGlobalId $ fromJust a1) 
                           a2 a3 a4 a5 a6 (fmap (tconvert mp) a7) 
                           a8 a8a (dcast FLC a9a) a10a a11 (fmap convert_Comdat a12) a13
       _ -> return $ I.TlGlobalDtype (specializeGlobalId $ fromJust a1) 
            a2 a3 a4 a5 a6 (fmap (tconvert mp) a7) 
            a8 a8a (dcast FLC a9a) a10a a11 (fmap convert_Comdat a12) a13
     }
  
convert_TlTypeDef :: A.TlTypeDef -> MM I.TlTypeDef
convert_TlTypeDef (A.TlTypeDef lid t) = 
  do { mp <- typeDefs
     ; let (ta::I.Utype) = tconvert mp t
     ; case ta of
       I.UtypeFunX _ -> return (I.TlFunTypeDef (sLid lid) (dcast FLC ta))
       I.UtypeOpaqueD _-> return (I.TlOpqTypeDef (sLid lid) (dcast FLC ta))
       _ -> return (I.TlDatTypeDef (sLid lid) (dcast FLC ((tconvert mp t)::I.Utype)))
     }
  
convert_TlDepLibs :: A.TlDepLibs -> MM I.TlDepLibs
convert_TlDepLibs (A.TlDepLibs s) = return (I.TlDepLibs s)
  
convert_TlUnamedType :: A.TlUnamedType -> (MM I.TlUnamedType)
convert_TlUnamedType (A.TlUnamedType i t) = do { mp <- typeDefs
                                               ; let (ta::I.Dtype) = dcast FLC ((tconvert mp t)::I.Utype)
                                               ; return (I.TlUnamedType i ta)
                                               }
  
convert_TlModuleAsm :: A.TlModuleAsm -> MM I.TlModuleAsm
convert_TlModuleAsm (A.TlModuleAsm s) = return (I.TlModuleAsm s)

convert_TlAttribute :: A.TlAttribute -> MM I.TlAttribute
convert_TlAttribute (A.TlAttribute n l) = return (I.TlAttribute n l)
  
convert_TlComdat :: A.TlComdat -> MM (I.TlComdat I.Gname)
convert_TlComdat (A.TlComdat l s) = return (I.TlComdat (specializeDollarId l) s)

convert_Comdat :: A.Comdat -> I.Comdat I.Gname
convert_Comdat (A.Comdat n) = I.Comdat (fmap specializeDollarId n)


toplevel2IrP1 :: A.Toplevel -> MM (A.FunctionPrototype, I.Toplevel I.Gname ())
toplevel2IrP1 (A.ToplevelDefine f@(A.TlDefine fp _)) = do { f0 <- convert_TlDefine f
                                                          ; return (fp, I.ToplevelDefine f0)
                                                          }



toplevel2IrP2 :: M.Map A.FunctionPrototype (I.Toplevel I.Gname ()) -> A.Toplevel -> MM (I.Toplevel I.Gname ())
toplevel2IrP2 _ (A.ToplevelAlias q) = Md.liftM I.ToplevelAlias (convert_TlAlias q)
toplevel2IrP2 _ (A.ToplevelUnamedMd s) = Md.liftM I.ToplevelUnamedMd (convert_TlUnamedMd s)
toplevel2IrP2 _ (A.ToplevelNamedMd m) = Md.liftM I.ToplevelNamedMd (convert_TlNamedMd m)
toplevel2IrP2 _ (A.ToplevelDeclare f) = 
  do { fx <- convert_TlDeclare f
     ; case specializeDeclareIntrinsic fx of
       Just fy -> return $ I.ToplevelDeclareIntrinsic fy
       Nothing -> return $ I.ToplevelDeclare fx
     }

toplevel2IrP2 mp (A.ToplevelDefine (A.TlDefine fp _)) = return $ fromJust $ M.lookup fp mp 
toplevel2IrP2 _ (A.ToplevelGlobal g) = do { tlg <- convert_TlGlobal g  
                                          ; case specializeTlGlobal tlg of
                                            Just tli -> return $ I.ToplevelIntrinsic tli
                                            Nothing -> return $ I.ToplevelGlobal tlg
                                          }
toplevel2IrP2 _ (A.ToplevelTypeDef t) = Md.liftM I.ToplevelTypeDef (convert_TlTypeDef t)
toplevel2IrP2 _ (A.ToplevelDepLibs qs) = Md.liftM I.ToplevelDepLibs (convert_TlDepLibs qs)
toplevel2IrP2 _ (A.ToplevelUnamedType i) = Md.liftM I.ToplevelUnamedType (convert_TlUnamedType i)
toplevel2IrP2 _ (A.ToplevelModuleAsm q) = Md.liftM I.ToplevelModuleAsm (convert_TlModuleAsm q)
toplevel2IrP2 _ (A.ToplevelAttribute n) = Md.liftM I.ToplevelAttribute (convert_TlAttribute n)
toplevel2IrP2 _ (A.ToplevelComdat l) = Md.liftM I.ToplevelComdat (convert_TlComdat l)



filterOutDataLayoutAndTriple :: [A.Toplevel] -> ((A.DataLayout, A.TargetTriple), [A.Toplevel])
filterOutDataLayoutAndTriple tls = 
  let [A.ToplevelTriple (A.TlTriple triple)] = filter (\x -> case x of
                                                          A.ToplevelTriple a -> True
                                                          _ -> False) tls
      [A.ToplevelDataLayout (A.TlDataLayout dl)] = filter (\x -> case x of
                                                              A.ToplevelDataLayout a -> True
                                                              _ -> False) tls
  in ((dl, triple), filter (\x -> case x of
                               A.ToplevelTriple _ -> False
                               A.ToplevelDataLayout _ -> False
                               _ -> True) tls)

asmToHir :: Target -> A.Module -> 
            H.SimpleUniqueMonad (IdLabelMap, I.SpecializedModule I.Gname ())
asmToHir tg@(Target dlm) m@(A.Module ts) = 
  let (lbMap,_) = H.runSimpleUniqueMonad (compMapping tg m)
  in 
   let ((A.DataLayout dl, tt), ts0) = filterOutDataLayoutAndTriple ts
       td = M.fromList $ A.typeDefOfModule m
   in if matchLayoutSpecAndTriple dlm dl tt then
        runLabelMapM emptyIdLabelMap 
        $ (runReaderT (do { let defs0 = filter (\x -> case x of
                                                   A.ToplevelDefine _ -> True
                                                   _ -> False) ts0
                          ; defs <- mapM toplevel2IrP1 defs0
                          ; l <- mapM (toplevel2IrP2 (M.fromList defs))  ts0
                          ; return $ I.SpecializedModule tg $ I.Module l
                          }
                      ) (ReaderData td (I.Gname (errorLoc FLC $ "<fatal error>")) lbMap))
      else 
        error $ show (dl,tt) ++ " does not match " ++ show dlm