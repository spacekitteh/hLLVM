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

import Llvm.Asm.SharedEntity (Packing, FunAttr, ParamAttr, Alignment, Fparam, ParamAttr,VarArgParam)
import Llvm.ErrorLoc
import Data.Word (Word32)

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
  Tfirst_class_array :: Word32 -> ScalarType -> Type FirstClassB D;
  Tfirst_class_struct :: Packing -> [ScalarType] -> Type FirstClassB D;
  Tfirst_class_name :: String -> Type FirstClassB D;
  Tfirst_class_quoteName :: String -> Type FirstClassB D;
  Tfirst_class_no :: Word32 -> Type FirstClassB D;

  Tarray :: Word32 -> Dtype -> Type RecordB D;
  Tstruct :: Packing -> [Dtype] -> Type RecordB D;

  Topaque_struct :: Packing -> [Either Dtype (Type OpaqueB D)] -> Type OpaqueB D;
  Topaque_array :: Word32 -> Type OpaqueB D -> Type OpaqueB D;

  Tpointer :: Etype -> AddrSpace -> Type ScalarB P;
  Tfunction :: Rtype -> TypeParamList -> [FunAttr] -> Type CodeFunB X;
  {- reference types -}

  {- referee is Scalar -}
  TnameScalarI :: String -> Type ScalarB I;
  TquoteNameScalarI :: String -> Type ScalarB I;
  TnoScalarI :: Word32 -> Type ScalarB I;

  TnameScalarF :: String -> Type ScalarB F;
  TquoteNameScalarF :: String -> Type ScalarB F;
  TnoScalarF :: Word32 -> Type ScalarB F;

  TnameScalarP :: String -> Type ScalarB P;
  TquoteNameScalarP :: String -> Type ScalarB P;
  TnoScalarP :: Word32 -> Type ScalarB P;

  {- referee is Vector -}
  TnameVectorI :: String -> Type VectorB I;
  TquoteNameVectorI :: String -> Type VectorB I;
  TnoVectorI :: Word32 -> Type VectorB I;

  TnameVectorF :: String -> Type VectorB F;
  TquoteNameVectorF :: String -> Type VectorB F;
  TnoVectorF :: Word32 -> Type VectorB F;

  TnameVectorP :: String -> Type VectorB P;
  TquoteNameVectorP :: String -> Type VectorB P;
  TnoVectorP :: Word32 -> Type VectorB P;

  {- referee is Large Block -}
  TnameRecordD :: String -> Type RecordB D;
  TquoteNameRecordD :: String -> Type RecordB D;
  TnoRecordD :: Word32 -> Type RecordB D;

  {- referee is Code Fun Block -}
  TnameCodeFunX :: String -> Type CodeFunB X;
  TquoteNameCodeFunX :: String -> Type CodeFunB X;
  TnoCodeFunX :: Word32 -> Type CodeFunB X;

  {- referee is Opaque -}
  TnameOpaqueD :: String -> Type OpaqueB D;
  TquoteNameOpaqueD :: String -> Type OpaqueB D;
  TnoOpaqueD :: Word32 -> Type OpaqueB D;
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
    Tfunction rt tp fa -> "Tfunction " ++ show rt ++ " " ++ show tp ++ " " ++ show fa
    {- Scalar -}
    TnameScalarI s -> "TnameScalarI " ++ show s
    TquoteNameScalarI s -> "TquoteNameScalarI " ++ show s
    TnoScalarI n -> "TnoScalarI " ++ show n

    TnameScalarF s -> "TnameScalarF " ++ show s
    TquoteNameScalarF s -> "TquoteNameScalarF " ++ show s
    TnoScalarF n -> "TnoScalarF " ++ show n

    TnameScalarP s -> "TnameScalarP " ++ show s
    TquoteNameScalarP s -> "TquoteNameScalarP " ++ show s
    TnoScalarP n -> "TnoScalarP " ++ show n

    {- Vector -}
    TnameVectorI s -> "TnameVectorI " ++ show s
    TquoteNameVectorI s -> "TquoteNameVectorI " ++ show s
    TnoVectorI n -> "TnoVectorI " ++ show n

    TnameVectorF s -> "TnameVectorF " ++ show s
    TquoteNameVectorF s -> "TquoteNameVectorF " ++ show s
    TnoVectorF n -> "TnoVectorF " ++ show n

    TnameVectorP s -> "TnameVectorP " ++ show s
    TquoteNameVectorP s -> "TquoteNameVectorP " ++ show s
    TnoVectorP n -> "TnoVectorP " ++ show n

    {- Large -}
    TnameRecordD s -> "TnameRecordD " ++ show s
    TquoteNameRecordD s -> "TquoteNameRecordD " ++ show s
    TnoRecordD n -> "TnoRecordD " ++ show n

    {- Code -}
    TnameCodeFunX s -> "TnameCodeFunX " ++ show s
    TquoteNameCodeFunX s -> "TquoteNameCodeFunX " ++ show s
    TnoCodeFunX n -> "TnoCodeFunX " ++ show n

    {- Opaque -}
    TnameOpaqueD s -> "TnameOpaqueD " ++ show s
    TquoteNameOpaqueD s -> "TquoteNameOpaqueD " ++ show s
    TnoOpaqueD n -> "TnoOpaqueD " ++ show n

    Topaque_struct pk l -> "Topaque_struct " ++ show pk ++ " " ++ show l
    Topaque_array n e -> "Topaque_array " ++ show n ++ " " ++ show e

    Tfirst_class_array n e -> "Tfirst_class_array " ++ show n ++ " " ++ show e
    Tfirst_class_struct pk l -> "Tfirst_class_struct " ++ show pk ++ " " ++ show l
    Tfirst_class_name s -> "Tfirst_class_name " ++ show s
    Tfirst_class_quoteName s -> "Tfirst_class_quoteName " ++ show s
    Tfirst_class_no s -> "Tfirst_class_no " ++ show s


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
    (Tfirst_class_quoteName s, Tfirst_class_quoteName s1) -> s == s1
    (Tfirst_class_no s, Tfirst_class_no s1) -> s == s1


    (Tpointer e as, Tpointer e1 as1) -> (e,as) == (e1,as1)
    (Tfunction rt tp fa, Tfunction rt1 tp1 fa1) -> (rt,tp,fa) == (rt1,tp1,fa1)

    {- Scalar -}
    (TnameScalarI s, TnameScalarI s1) -> s == s1
    (TquoteNameScalarI s, TquoteNameScalarI s1) -> s == s1
    (TnoScalarI n, TnoScalarI n1) -> n == n1

    (TnameScalarF s, TnameScalarF s1) -> s == s1
    (TquoteNameScalarF s, TquoteNameScalarF s1) -> s == s1
    (TnoScalarF n, TnoScalarF n1) -> n == n1

    (TnameScalarP s, TnameScalarP s1) -> s == s1
    (TquoteNameScalarP s, TquoteNameScalarP s1) -> s == s1
    (TnoScalarP n, TnoScalarP n1) -> n == n1

    {- Vector -}
    (TnameVectorI s, TnameVectorI s1) -> s == s1
    (TquoteNameVectorI s, TquoteNameVectorI s1) -> s == s1
    (TnoVectorI n, TnoVectorI n1) -> n == n1

    (TnameVectorF s, TnameVectorF s1) -> s == s1
    (TquoteNameVectorF s, TquoteNameVectorF s1) -> s == s1
    (TnoVectorF n, TnoVectorF n1) -> n == n1

    (TnameVectorP s, TnameVectorP s1) -> s == s1
    (TquoteNameVectorP s, TquoteNameVectorP s1) -> s == s1
    (TnoVectorP n, TnoVectorP n1) -> n == n1

    {- Record -}
    (TnameRecordD s, TnameRecordD s1) -> s == s1
    (TquoteNameRecordD s, TquoteNameRecordD s1) -> s == s1
    (TnoRecordD n, TnoRecordD n1) -> n == n1

    {- Fun Code -}
    (TnameCodeFunX s, TnameCodeFunX s1) -> s == s1
    (TquoteNameCodeFunX s, TquoteNameCodeFunX s1) -> s == s1
    (TnoCodeFunX n, TnoCodeFunX n1) -> n == n1

    {- Opaque -}
    (TnameOpaqueD s, TnameOpaqueD s1) -> s == s1 -- errorLoc FLC "comparing opaque types"
    (TquoteNameOpaqueD s, TquoteNameOpaqueD s1) -> s == s1 -- errorLoc FLC "comparing opaque types"
    (TnoOpaqueD n, TnoOpaqueD n1) -> n == n1 -- errorLoc FLC "comparing opaque types"
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
    (Tfirst_class_quoteName s, Tfirst_class_quoteName s1) -> compare s s1
    (Tfirst_class_no s, Tfirst_class_no s1) -> compare s s1


    (Tpointer e as, Tpointer e1 as1) -> compare (e,as) (e1,as1)
    (Tfunction rt tp fa, Tfunction rt1 tp1 fa1) -> compare (rt,tp,fa) (rt1, tp1, fa1)

    {- Scalar -}
    (TnameScalarI s, TnameScalarI s1) -> compare s s1
    (TquoteNameScalarI s, TquoteNameScalarI s1) -> compare s s1
    (TnoScalarI n, TnoScalarI n1) -> compare n n1

    (TnameScalarF s, TnameScalarF s1) -> compare s s1
    (TquoteNameScalarF s, TquoteNameScalarF s1) -> compare s s1
    (TnoScalarF n, TnoScalarF n1) -> compare n n1

    (TnameScalarP s, TnameScalarP s1) -> compare s s1
    (TquoteNameScalarP s, TquoteNameScalarP s1) -> compare s s1
    (TnoScalarP n, TnoScalarP n1) -> compare n n1

    {- Vector -}
    (TnameVectorI s, TnameVectorI s1) -> compare s s1
    (TquoteNameVectorI s, TquoteNameVectorI s1) -> compare s s1
    (TnoVectorI n, TnoVectorI n1) -> compare n n1

    (TnameVectorF s, TnameVectorF s1) -> compare s s1
    (TquoteNameVectorF s, TquoteNameVectorF s1) -> compare s s1
    (TnoVectorF n, TnoVectorF n1) -> compare n n1

    (TnameVectorP s, TnameVectorP s1) -> compare s s1
    (TquoteNameVectorP s, TquoteNameVectorP s1) -> compare s s1
    (TnoVectorP n, TnoVectorP n1) -> compare n n1

    {- Large -}
    (TnameRecordD s, TnameRecordD s1) -> compare s s1
    (TquoteNameRecordD s, TquoteNameRecordD s1) -> compare s s1
    (TnoRecordD n, TnoRecordD n1) -> compare n n1

    {- Code -}
    (TnameCodeFunX s, TnameCodeFunX s1) -> compare s s1
    (TquoteNameCodeFunX s, TquoteNameCodeFunX s1) -> compare s s1
    (TnoCodeFunX n, TnoCodeFunX n1) -> compare n n1

    {- Opaque -}
    (TnameOpaqueD s, TnameOpaqueD s1) -> compare s s1 -- this might cause false negative: two equal types return non-equal
    (TquoteNameOpaqueD _, TquoteNameOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (TnoOpaqueD n, TnoOpaqueD n1) -> compare n n1 -- this might cause false negative: two equal types returns non-equal
    (Topaque_struct p1 sl1, Topaque_struct p2 sl2 ) -> compare (p1, sl1) (p2, sl2)
    (Topaque_array n1 t1, Topaque_array n2 t2) -> compare (n1, t1) (n2, t2)

    (_,_) -> compare (show x1) (show x2)

data FormalParam = FormalParamData Dtype [ParamAttr] (Maybe Alignment) Fparam
                 | FormalParamByVal Dtype [ParamAttr] (Maybe Alignment) Fparam
                 | FormalParamMeta MetaKind Fparam
                 deriving (Eq,Ord,Show)

data FormalParamList = FormalParamList [FormalParam] (Maybe VarArgParam) [FunAttr]
                     deriving (Eq,Ord,Show)

data TypeParamList = TypeParamList [Dtype] (Maybe VarArgParam) deriving (Eq,Ord,Show)


type AddrSpace = Word32

data MetaKind = Mtype Utype
              | Mmetadata
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

