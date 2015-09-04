{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module Llvm.Hir.Data.Type
       (module Llvm.Asm.SharedEntity
       ,module Llvm.Hir.Data.Type
       ,module Data.Word
       )
       where

import Llvm.Asm.SharedEntity (Packing, FunAttr, RetAttr, AlignInByte, LocalId, VarArgParam)
import Llvm.ErrorLoc
import Data.Word (Word32, Word64)

#define FLC   (FileLoc $(srcLoc))

data ScalarB -- Scalar Storage Block
data VectorB -- Vector Storage Block
data FirstClassB  -- First Class Storage Block
data RecordB -- Record (struct and array) Storage Block
data CodeFunB -- Code Fun Storage Block
data CodeLabelB -- Code Label Storage Block
data OpaqueB -- Opaque Storage Block
data NoB -- No Storage Block

data I -- Integer representation
data F -- Float representation
data P -- Pointer representation
data D -- Data (I,F,P) representation
data U -- Unknown representation
data X -- Executable representation

data Type sto rep where {
  TpI :: Word32 -> Type ScalarB I;
  TpF :: Word32 -> Type ScalarB F;
  TpV :: Word32 -> Type ScalarB I;
  TpHalf :: Type ScalarB F;
  TpFloat :: Type ScalarB F;
  TpDouble :: Type ScalarB F;
  TpFp128 :: Type ScalarB F;
  TpX86Fp80 :: Type ScalarB F;
  TpPpcFp128 :: Type ScalarB F;
  TpX86Mmx :: Type ScalarB I;
  TvectorI :: Word32 -> Type ScalarB I -> Type VectorB I;
  TvectorF :: Word32 -> Type ScalarB F -> Type VectorB F;
  TvectorP :: Word32 -> Type ScalarB P -> Type VectorB P;
  TpNull :: Type ScalarB I;
  TpLabel :: Type CodeLabelB X;
  Tvoid :: Type NoB U;
  Topaque :: Type OpaqueB D;

  {- first class aggregate types -}
  Tfirst_class_array :: Word64 -> ScalarType -> Type FirstClassB D;
  Tfirst_class_struct :: Packing -> [ScalarType] -> Type FirstClassB D;
  Tfirst_class_name :: String -> Type FirstClassB D;
  Tarray :: Word64 -> Dtype -> Type RecordB D;
  Tstruct :: Packing -> [Dtype] -> Type RecordB D;

  Topaque_struct :: Packing -> [Either Dtype (Type OpaqueB D)] -> Type OpaqueB D;
  Topaque_array :: Word64 -> Type OpaqueB D -> Type OpaqueB D;

  Tpointer :: Etype -> AddrSpace -> Type ScalarB P;
  Tfunction :: (Rtype, [RetAttr]) -> [(Mtype, Maybe AlignInByte)]
               -> Maybe VarArgParam -> Type CodeFunB X;
  {- reference types -}

  {- referee is Scalar -}
  TnameScalarI :: String -> Type ScalarB I;
  TnameScalarF :: String -> Type ScalarB F;
  TnameScalarP :: String -> Type ScalarB P;
  {- referee is Vector -}
  TnameVectorI :: String -> Type VectorB I;
  TnameVectorF :: String -> Type VectorB F;
  TnameVectorP :: String -> Type VectorB P;
  {- referee is Large Block -}
  TnameRecordD :: String -> Type RecordB D;
  {- referee is Code Fun Block -}
  TnameCodeFunX :: String -> Type CodeFunB X;
  {- referee is Opaque -}
  TnameOpaqueD :: String -> Type OpaqueB D;
  }


instance Show (Type s r) where
  show x = case x of
    TpI n -> "TpI " ++ show n
    TpF n -> "TpF " ++ show n
    TpV n -> "TpV " ++ show n
    Tvoid -> "Tvoid"
    TpHalf -> "TpHalf"
    TpFloat -> "TpFloat"
    TpDouble -> "TpDouble"
    TpFp128 -> "TpFp128"
    TpX86Fp80 -> "TpX86Fp80"
    TpPpcFp128 -> "TpPpcFp128"
    TpX86Mmx -> "TpX86Mmx"
    TpNull -> "TpNull"
    TpLabel -> "TpLabel"
    Topaque -> "Topaque"
    Tarray n d -> "Tarray " ++ show n ++ " " ++ show d

    TvectorI n d -> "TvectorI " ++ show n ++ " " ++ show d
    TvectorF n d -> "TvectorF " ++ show n ++ " " ++ show d
    TvectorP n d -> "TvectorP " ++ show n ++ " " ++ show d

    Tstruct p ds -> "Tstruct " ++ show p ++ " " ++ show ds
    Tpointer e as -> "Tpointer " ++ show e ++ " " ++ show as
    Tfunction rt tp mv -> "Tfunction " ++ show rt ++ " " ++ show tp ++ " " ++ show mv
    {- Scalar -}
    TnameScalarI s -> "TnameScalarI " ++ show s
    TnameScalarF s -> "TnameScalarF " ++ show s
    TnameScalarP s -> "TnameScalarP " ++ show s
    {- Vector -}
    TnameVectorI s -> "TnameVectorI " ++ show s
    TnameVectorF s -> "TnameVectorF " ++ show s
    TnameVectorP s -> "TnameVectorP " ++ show s
    {- Large -}
    TnameRecordD s -> "TnameRecordD " ++ show s
    {- Code -}
    TnameCodeFunX s -> "TnameCodeFunX " ++ show s
    {- Opaque -}
    TnameOpaqueD s -> "TnameOpaqueD " ++ show s
    Topaque_struct pk l -> "Topaque_struct " ++ show pk ++ " " ++ show l
    Topaque_array n e -> "Topaque_array " ++ show n ++ " " ++ show e

    Tfirst_class_array n e -> "Tfirst_class_array " ++ show n ++ " " ++ show e
    Tfirst_class_struct pk l -> "Tfirst_class_struct " ++ show pk ++ " " ++ show l
    Tfirst_class_name s -> "Tfirst_class_name " ++ show s

instance Eq (Type s r) where
  (==) x1 x2 = case (x1, x2) of
    (TpI n, TpI n1) -> n == n1
    (TpF n, TpF n1) -> n == n1
    (TpV n, TpV n1) -> n == n1
    (Tvoid, Tvoid) -> True
    (TpHalf, TpHalf) -> True
    (TpFloat, TpFloat) -> True
    (TpDouble, TpDouble) -> True
    (TpFp128, TpFp128) -> True
    (TpX86Fp80, TpX86Fp80) -> True
    (TpPpcFp128, TpPpcFp128) -> True
    (TpX86Mmx, TpX86Mmx) -> True
    (TpNull, TpNull) -> error "Eq: comparing TpNull"
    (TpLabel, TpLabel) -> error "Eq: comparing TpLabel"
    (Topaque, Topaque) -> error "Eq: comparing Topaque"


    (TvectorI n d, TvectorI n1 d1) -> (n,d) == (n1,d1)
    (TvectorF n d, TvectorF n1 d1) -> (n,d) == (n1,d1)
    (TvectorP n d, TvectorP n1 d1) -> (n,d) == (n1,d1)

    (Tarray n d, Tarray n1 d1) -> (n,d) == (n1,d1)
    (Tstruct p ds, Tstruct p1 ds1) -> (p,ds) == (p1,ds1)

    (Tfirst_class_array n d, Tfirst_class_array n1 d1) -> (n,d) == (n1,d1)
    (Tfirst_class_struct p ds, Tfirst_class_struct p1 ds1) -> (p,ds) == (p1,ds1)
    (Tfirst_class_name s, Tfirst_class_name s1) -> s == s1

    (Tpointer e as, Tpointer e1 as1) -> (e,as) == (e1,as1)
    (Tfunction rt tp mv, Tfunction rt1 tp1 mv1) -> (rt,tp,mv) == (rt1,tp1,mv1)

    {- Scalar -}
    (TnameScalarI s, TnameScalarI s1) -> s == s1
    (TnameScalarF s, TnameScalarF s1) -> s == s1
    (TnameScalarP s, TnameScalarP s1) -> s == s1
    {- Vector -}
    (TnameVectorI s, TnameVectorI s1) -> s == s1
    (TnameVectorF s, TnameVectorF s1) -> s == s1
    (TnameVectorP s, TnameVectorP s1) -> s == s1
    {- Record -}
    (TnameRecordD s, TnameRecordD s1) -> s == s1
    {- Fun Code -}
    (TnameCodeFunX s, TnameCodeFunX s1) -> s == s1
    {- Opaque -}
    (TnameOpaqueD s, TnameOpaqueD s1) -> s == s1 -- errorLoc FLC "comparing opaque types"
    (Topaque_struct pk l, Topaque_struct pk1 l1) -> pk == pk1 && l == l1 -- errorLoc FLC "comparing opaque types"
    (Topaque_array n l, Topaque_array n1 l1) -> n == n1 && l == l1 -- errorLoc FLC "comparing opaque types"

    (_,_) -> False



instance Ord (Type s r) where
  compare x1 x2 = case (x1, x2) of
    (TpI n, TpI n1) -> compare n n1
    (TpF n, TpF n1) -> compare n n1
    (TpV n, TpV n1) -> compare n n1
    (Tvoid, Tvoid) -> EQ
    (TpHalf, TpHalf) -> EQ
    (TpFloat, TpFloat) -> EQ
    (TpDouble, TpDouble) -> EQ
    (TpFp128, TpFp128) -> EQ
    (TpX86Fp80, TpX86Fp80) -> EQ
    (TpPpcFp128, TpPpcFp128) -> EQ
    (TpX86Mmx, TpX86Mmx) -> EQ
    (TpNull, TpNull) -> error "Ord: comparing TpNull"
    (TpLabel, TpLabel) -> error "Ord: comparing TpLabel"
    (Topaque, Topaque) -> error "Ord: comparing Topaque"

    (TvectorI n d, TvectorI n1 d1) -> compare (n,d) (n1,d1)
    (TvectorF n d, TvectorF n1 d1) -> compare (n,d) (n1,d1)
    (TvectorP n d, TvectorP n1 d1) -> compare (n,d) (n1,d1)

    (Tarray n d, Tarray n1 d1) -> compare (n,d) (n1,d1)
    (Tstruct p ds, Tstruct p1 ds1) -> compare (p,ds) (p1,ds1)

    (Tfirst_class_array n d, Tfirst_class_array n1 d1) -> compare (n,d) (n1,d1)
    (Tfirst_class_struct p ds, Tfirst_class_struct p1 ds1) -> compare (p,ds) (p1,ds1)
    (Tfirst_class_name s, Tfirst_class_name s1) -> compare s s1

    (Tpointer e as, Tpointer e1 as1) -> compare (e,as) (e1,as1)
    (Tfunction rt tp mv, Tfunction rt1 tp1 mv1) -> compare (rt,tp,mv) (rt1, tp1, mv1)

    {- Scalar -}
    (TnameScalarI s, TnameScalarI s1) -> compare s s1
    (TnameScalarF s, TnameScalarF s1) -> compare s s1
    (TnameScalarP s, TnameScalarP s1) -> compare s s1
    {- Vector -}
    (TnameVectorI s, TnameVectorI s1) -> compare s s1
    (TnameVectorF s, TnameVectorF s1) -> compare s s1
    (TnameVectorP s, TnameVectorP s1) -> compare s s1
    {- Large -}
    (TnameRecordD s, TnameRecordD s1) -> compare s s1
    {- Code -}
    (TnameCodeFunX s, TnameCodeFunX s1) -> compare s s1
    {- Opaque -}
    (TnameOpaqueD s, TnameOpaqueD s1) -> compare s s1 -- this might cause false negative: two equal types return non-equal
    (Topaque_struct p1 sl1, Topaque_struct p2 sl2 ) -> compare (p1, sl1) (p2, sl2)
    (Topaque_array n1 t1, Topaque_array n2 t2) -> compare (n1, t1) (n2, t2)

    (_,_) -> compare (show x1) (show x2)

data TypeParamList = TypeParamList [Dtype] (Maybe VarArgParam) deriving (Eq,Ord,Show)


type AddrSpace = Word32

data MetaKind = MKtype Utype
              | MKmetadata
              deriving (Eq, Ord,Show)


{- short hand notations -}

data Utype = UtypeScalarI (Type ScalarB I)
           | UtypeScalarF (Type ScalarB F)
           | UtypeScalarP (Type ScalarB P)
           | UtypeVectorI (Type VectorB I)
           | UtypeVectorF (Type VectorB F)
           | UtypeVectorP (Type VectorB P)
           | UtypeFirstClassD (Type FirstClassB D)
           | UtypeRecordD (Type RecordB D)
           | UtypeOpaqueD (Type OpaqueB D)
           | UtypeVoidU (Type NoB U)
           | UtypeFunX (Type CodeFunB X)
           | UtypeLabelX (Type CodeLabelB X)
           deriving (Eq,Ord,Show)

data Etype = EtypeScalarI (Type ScalarB I)
           | EtypeScalarF (Type ScalarB F)
           | EtypeScalarP (Type ScalarB P)
           | EtypeVectorI (Type VectorB I)
           | EtypeVectorF (Type VectorB F)
           | EtypeVectorP (Type VectorB P)
           | EtypeFirstClassD (Type FirstClassB D)
           | EtypeRecordD (Type RecordB D)
           | EtypeOpaqueD (Type OpaqueB D)
           | EtypeFunX (Type CodeFunB X)
           deriving (Eq, Ord, Show)

{- this will be replicated in multiple nodes to reduce the depth of AST, a lower depth AST is
   more friendly for manual AST construction
-}
data Rtype = RtypeScalarI (Type ScalarB I)
           | RtypeScalarF (Type ScalarB F)
           | RtypeScalarP (Type ScalarB P)
           | RtypeVectorI (Type VectorB I)
           | RtypeVectorF (Type VectorB F)
           | RtypeVectorP (Type VectorB P)
           | RtypeFirstClassD (Type FirstClassB D)
           | RtypeRecordD (Type RecordB D)
           | RtypeVoidU (Type NoB U)
           deriving (Eq, Ord, Show)

data Mtype = MtypeAsRet Dtype
           | MtypeData Dtype
           | MtypeByVal Dtype
           | MtypeExt Ext Dtype
           | MtypeLabel (Type CodeLabelB X)
           deriving (Eq, Ord, Show)

data Ext = Sign | Zero deriving (Eq, Ord, Show)

data Dtype = DtypeScalarI (Type ScalarB I)
           | DtypeScalarF (Type ScalarB F)
           | DtypeScalarP (Type ScalarB P)
           | DtypeVectorI (Type VectorB I)
           | DtypeVectorF (Type VectorB F)
           | DtypeVectorP (Type VectorB P)
           | DtypeFirstClassD (Type FirstClassB D)
           | DtypeRecordD (Type RecordB D)
           deriving (Eq, Ord, Show)

{- First class type -}
data Ftype = FtypeScalarI (Type ScalarB I)
           | FtypeScalarF (Type ScalarB F)
           | FtypeScalarP (Type ScalarB P)
           | FtypeVectorI (Type VectorB I)
           | FtypeVectorF (Type VectorB F)
           | FtypeVectorP (Type VectorB P)
           | FtypeFirstClassD (Type FirstClassB D)
           deriving (Eq, Ord, Show)


data ScalarType = ScalarTypeI (Type ScalarB I)
                | ScalarTypeF (Type ScalarB F)
                | ScalarTypeP (Type ScalarB P)
                deriving (Eq, Ord, Show)

data IntOrPtrType s = IntOrPtrTypeI (Type s I)
                    | IntOrPtrTypeP (Type s P)
                    deriving (Eq, Ord, Show)

