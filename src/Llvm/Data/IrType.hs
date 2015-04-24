{-# OPTIONS_GHC -cpp -fwarn-incomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Llvm.Data.IrType
       (module Llvm.Data.Shared.AtomicEntity
       ,module Llvm.Data.IrType
       ,module Llvm.Data.Shared.Util
       ,module Data.Word
       )
       where

import Llvm.Data.Shared.AtomicEntity
import Llvm.Data.Shared.Util
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

instance Mangle (Type s r) where
  mangle x = case x of
    TpI n -> "i" ++ show n
    TpF n -> "f" ++ show n
    TpV n -> "vi" ++ show n
    Tvoid -> "void"
    TpHalf -> "half"
    TpFloat -> "float"
    TpDouble -> "double"
    TpFp128 -> "fp128"
    TpX86Fp80 -> "x86fp80"
    TpPpcFp128 -> "ppcfp128"
    TpX86Mmx -> "x86mmx"
    TpNull -> "null"
    TpLabel -> "label"
    Topaque -> "opaque"
    Tarray n d -> "a_" ++ show n ++ "_" ++ mangle d

    TvectorI n d -> "vi_" ++ show n ++ "_" ++ mangle d
    TvectorF n d -> "vf_" ++ show n ++ "_" ++ mangle d
    TvectorP n d -> "vp_" ++ show n ++ "_" ++ mangle d

    Tstruct p ds -> "s_" ++ show p ++ "_" ++ mangle ds
    Tpointer e as -> "ptr_" ++ mangle e ++ "_" ++ show as
    Tfunction rt tp fa -> "fun_" ++ mangle rt ++ " " ++ mangle tp ++ " " ++ show fa
    {- Scalar -}
    TnameScalarI s -> show s
    TquoteNameScalarI s -> show s
    TnoScalarI n -> show n

    TnameScalarF s -> show s
    TquoteNameScalarF s -> show s
    TnoScalarF n -> show n

    TnameScalarP s -> show s
    TquoteNameScalarP s -> show s
    TnoScalarP n -> show n

    {- Vector -}
    TnameVectorI s -> show s
    TquoteNameVectorI s -> show s
    TnoVectorI n -> show n

    TnameVectorF s -> show s
    TquoteNameVectorF s -> show s
    TnoVectorF n -> show n

    TnameVectorP s -> show s
    TquoteNameVectorP s -> show s
    TnoVectorP n -> show n

    {- Large -}
    TnameRecordD s -> show s
    TquoteNameRecordD s -> show s
    TnoRecordD n -> show n

    {- Code -}
    TnameCodeFunX s -> show s
    TquoteNameCodeFunX s -> show s
    TnoCodeFunX n -> show n

    {- Opaque -}
    TnameOpaqueD s -> show s
    TquoteNameOpaqueD s -> show s
    TnoOpaqueD n -> show n
    
    Topaque_struct pk l -> "os_" ++ show pk ++ "_" ++ show l
    Topaque_array n e -> "oa_" ++ show n ++ "_" ++ show e 
    
    Tfirst_class_array n e -> "1ca_" ++ show n ++ " " ++ show e
    Tfirst_class_struct pk l -> "1cs_" ++ show pk ++ " " ++ show l
    Tfirst_class_name s -> show s
    Tfirst_class_quoteName s -> show s
    Tfirst_class_no s -> show s

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


    (TvectorI n d, TvectorI n1 d1) -> eq2 (n,n1) (d,d1)
    (TvectorF n d, TvectorF n1 d1) -> eq2 (n,n1) (d,d1)
    (TvectorP n d, TvectorP n1 d1) -> eq2 (n,n1) (d,d1)

    (Tarray n d, Tarray n1 d1) -> eq2 (n,n1) (d,d1)
    (Tstruct p ds, Tstruct p1 ds1) -> eq2 (p,p1) (ds,ds1)
    
    (Tfirst_class_array n d, Tfirst_class_array n1 d1) -> eq2 (n,n1) (d,d1)
    (Tfirst_class_struct p ds, Tfirst_class_struct p1 ds1) -> eq2 (p,p1) (ds,ds1)
    (Tfirst_class_name s, Tfirst_class_name s1) -> s == s1
    (Tfirst_class_quoteName s, Tfirst_class_quoteName s1) -> s == s1    
    (Tfirst_class_no s, Tfirst_class_no s1) -> s == s1    
    
    
    (Tpointer e as, Tpointer e1 as1) -> eq2 (e,e1) (as,as1)
    (Tfunction rt tp fa, Tfunction rt1 tp1 fa1) -> eq3 (rt,rt1) (tp,tp1) (fa,fa1)

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
    (TnameOpaqueD _, TnameOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (TquoteNameOpaqueD _, TquoteNameOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (TnoOpaqueD _, TnoOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (Topaque_struct _ _, Topaque_struct _ _) -> errorLoc FLC "comparing opaque types"
    (Topaque_array _ _, Topaque_array _ _) -> errorLoc FLC "comparing opaque types"

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


    (TvectorI n d, TvectorI n1 d1) -> compare2 (n,n1) (d,d1)
    (TvectorF n d, TvectorF n1 d1) -> compare2 (n,n1) (d,d1)
    (TvectorP n d, TvectorP n1 d1) -> compare2 (n,n1) (d,d1)

    (Tarray n d, Tarray n1 d1) -> compare2 (n,n1) (d,d1)
    (Tstruct p ds, Tstruct p1 ds1) -> compare2 (p,p1) (ds,ds1)
    
    (Tfirst_class_array n d, Tfirst_class_array n1 d1) -> compare2 (n,n1) (d,d1)
    (Tfirst_class_struct p ds, Tfirst_class_struct p1 ds1) -> compare2 (p,p1) (ds,ds1)
    (Tfirst_class_name s, Tfirst_class_name s1) -> compare s s1
    (Tfirst_class_quoteName s, Tfirst_class_quoteName s1) -> compare s s1    
    (Tfirst_class_no s, Tfirst_class_no s1) -> compare s s1


    (Tpointer e as, Tpointer e1 as1) -> compare2 (e,e1) (as,as1)
    (Tfunction rt tp fa, Tfunction rt1 tp1 fa1) -> compare3 (rt,rt1) (tp,tp1) (fa,fa1)

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
    (TnameOpaqueD _, TnameOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (TquoteNameOpaqueD _, TquoteNameOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (TnoOpaqueD _, TnoOpaqueD _) -> errorLoc FLC "comparing opaque types"
    (Topaque_struct _ _, Topaque_struct _ _) -> errorLoc FLC "comparing opaque types"
    (Topaque_array _ _, Topaque_array _ _) -> errorLoc FLC "comparing opaque types"    

    (_,_) -> compare (show x1) (show x2)

data FormalParam = FormalParamData Dtype [ParamAttr] (Maybe Alignment) Fparam [ParamAttr]
                 | FormalParamMeta MetaKind Fparam
                 deriving (Eq,Ord,Show)

data FormalParamList = FormalParamList [FormalParam] (Maybe VarArgParam) [FunAttr]
                     deriving (Eq,Ord,Show)

data TypeParamList = TypeParamList [Dtype] (Maybe VarArgParam) deriving (Eq,Ord,Show)

instance Mangle TypeParamList where
  mangle (TypeParamList l va) = "(" ++ mangle l ++ show va ++ ")"

type AddrSpace = Word32

data MetaKind = Mtype Utype
              | Mmetadata
              deriving (Eq, Ord,Show)


{- short hand notations -}
uc :: Ucast a b => a -> b
uc x = ucast x

dc :: Dcast a b => FileLoc -> String -> a -> b
dc lc s x = dcast lc x

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

{- Type s r, which represents all types,  ucast to Utype -}
instance Ucast (Type s r) (Type s r) where
  ucast = id
  
instance Ucast (Type s r) Utype where
  ucast x = case x of
    TpI _ -> UtypeScalarI x
    TpF _ -> UtypeScalarF x
    TpV _ -> UtypeScalarI x
    Tvoid -> UtypeVoidU x
    TpHalf -> UtypeScalarF x
    TpFloat -> UtypeScalarF x
    TpDouble -> UtypeScalarF x
    TpFp128 -> UtypeScalarF x
    TpX86Fp80 -> UtypeScalarF x
    TpPpcFp128 -> UtypeScalarF x
    TpX86Mmx -> UtypeScalarI x
    TpNull -> UtypeScalarI x
    TpLabel -> UtypeLabelX x
    Topaque -> UtypeOpaqueD x

    TvectorI _ _ -> UtypeVectorI x
    TvectorF _ _ -> UtypeVectorF x
    TvectorP _ _ -> UtypeVectorP x
  
    Tfirst_class_array _ _ -> UtypeFirstClassD x
    Tfirst_class_struct _ _ -> UtypeFirstClassD x
    Tfirst_class_name _ -> UtypeFirstClassD x
    Tfirst_class_quoteName _ -> UtypeFirstClassD x    
    Tfirst_class_no _ -> UtypeFirstClassD x    
    
    Tarray _ _ -> UtypeRecordD x
    Tstruct _ _ -> UtypeRecordD x
    
    Topaque_struct _ _ -> UtypeOpaqueD x
    Topaque_array _ _ -> UtypeOpaqueD x    
    
    Tpointer _ _ -> UtypeScalarP x
    Tfunction _ _ _ -> UtypeFunX x

    {- Scalar -}
    TnameScalarI _ -> UtypeScalarI x
    TquoteNameScalarI _ -> UtypeScalarI x
    TnoScalarI _ -> UtypeScalarI x

    TnameScalarF _ -> UtypeScalarF x
    TquoteNameScalarF _ -> UtypeScalarF x
    TnoScalarF _ -> UtypeScalarF x

    TnameScalarP _ -> UtypeScalarP x
    TquoteNameScalarP _ -> UtypeScalarP x
    TnoScalarP _ -> UtypeScalarP x

    {- Vector -}
    TnameVectorI _ -> UtypeVectorI x
    TquoteNameVectorI _ -> UtypeVectorI x
    TnoVectorI _ -> UtypeVectorI x

    TnameVectorF _ -> UtypeVectorF x
    TquoteNameVectorF _ -> UtypeVectorF x
    TnoVectorF _ -> UtypeVectorF x

    TnameVectorP _ -> UtypeVectorP x
    TquoteNameVectorP _ -> UtypeVectorP x
    TnoVectorP _ -> UtypeVectorP x

    {- Large -}
    TnameRecordD _ -> UtypeRecordD x
    TquoteNameRecordD _ -> UtypeRecordD x
    TnoRecordD _ -> UtypeRecordD x

    {- Code -}
    TnameCodeFunX _ -> UtypeFunX x
    TquoteNameCodeFunX _ -> UtypeFunX x
    TnoCodeFunX _ -> UtypeFunX x

    {- Opaque -}
    TnameOpaqueD _ -> UtypeOpaqueD x
    TquoteNameOpaqueD _ -> UtypeOpaqueD x
    TnoOpaqueD _ -> UtypeOpaqueD x

instance Dcast Utype (Type ScalarB I) where
  dcast lc e = case e of
    UtypeScalarI x -> x
    _ -> dcastError lc "Type ScalarB I" e

instance Dcast Utype (Type ScalarB F) where
  dcast lc e = case e of
    UtypeScalarF x -> x
    _ -> dcastError lc "Type ScalarB F" e

instance Dcast Utype (Type ScalarB P) where
  dcast lc e = case e of
    UtypeScalarP x -> x
    _ -> dcastError lc "Type ScalarB P" e

instance Dcast Utype (Type VectorB I) where
  dcast lc e = case e of
    UtypeVectorI x -> x
    _ -> dcastError lc "Type VectorB I" e

instance Dcast Utype (Type VectorB F) where
  dcast lc e = case e of
    UtypeVectorF x -> x
    _ -> dcastError lc "Type VectorB F" e

instance Dcast Utype (Type VectorB P) where
  dcast lc e = case e of
    UtypeVectorP x -> x
    _ -> dcastError lc "Type VectorB P" e

instance Dcast Utype (Type FirstClassB D) where
  dcast lc e = case e of
    UtypeFirstClassD x -> x
    _ -> dcastError lc "Type FirstClassB D" e

instance Dcast Utype (Type RecordB D) where
  dcast lc e = case e of
    UtypeRecordD x -> x
    _ -> dcastError lc "Type RecordB D" e


instance Dcast Utype (Type CodeFunB X) where
  dcast lc e = case e of
    UtypeFunX x -> x
    _ -> dcastError lc "Type CodeFunB X" e

instance Dcast Utype (Type CodeLabelB X) where
  dcast lc e = case e of
    UtypeLabelX x -> x
    _ -> dcastError lc "Type CodeLabelB X" e


instance Dcast Utype (Type OpaqueB D) where
  dcast lc e = case e of
    UtypeOpaqueD x -> x
    _ -> dcastError lc "Type OpaqueB D" e


{- Etype's dcast, ucast, dcast -}
instance Dcast Utype Etype where
  dcast lc x = case x of
    UtypeScalarI e -> EtypeScalarI e
    UtypeScalarF e -> EtypeScalarF e
    UtypeScalarP e -> EtypeScalarP e
    UtypeVectorI e -> EtypeVectorI e
    UtypeVectorF e -> EtypeVectorF e
    UtypeVectorP e -> EtypeVectorP e
    UtypeFirstClassD e -> EtypeFirstClassD e    
    UtypeRecordD e -> EtypeRecordD e
    UtypeOpaqueD e -> EtypeOpaqueD e
    UtypeFunX e -> EtypeFunX e
    _ -> dcastError lc "Etype" x

instance Ucast Etype Utype where
  ucast x = case x of
    EtypeScalarI e -> UtypeScalarI e
    EtypeScalarF e -> UtypeScalarF e
    EtypeScalarP e -> UtypeScalarP e
    EtypeVectorI e -> UtypeVectorI e
    EtypeVectorF e -> UtypeVectorF e
    EtypeVectorP e -> UtypeVectorP e
    EtypeFirstClassD e -> UtypeFirstClassD e
    EtypeRecordD e -> UtypeRecordD e
    EtypeOpaqueD e -> UtypeOpaqueD e
    EtypeFunX e -> UtypeFunX e

instance Dcast Etype Dtype where
  dcast lc x = case x of
    EtypeScalarI e -> DtypeScalarI e
    EtypeScalarF e -> DtypeScalarF e
    EtypeScalarP e -> DtypeScalarP e
    EtypeVectorI e -> DtypeVectorI e
    EtypeVectorF e -> DtypeVectorF e
    EtypeVectorP e -> DtypeVectorP e
    EtypeFirstClassD e -> DtypeFirstClassD e
    EtypeRecordD e -> DtypeRecordD e
    _ -> dcastError lc "Dtype" x

instance Dcast (Type s r) Etype where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Ucast (Type x I) Etype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x F) Etype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x P) Etype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type FirstClassB x) Etype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type RecordB D) Etype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type CodeFunB X) Etype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1


{- Rtype's dcast, ucast, dcast -}
instance Dcast Utype Rtype where
  dcast lc x = case x of
    UtypeScalarI e -> RtypeScalarI e
    UtypeScalarF e -> RtypeScalarF e
    UtypeScalarP e -> RtypeScalarP e
    UtypeVectorI e -> RtypeVectorI e
    UtypeVectorF e -> RtypeVectorF e
    UtypeVectorP e -> RtypeVectorP e
    UtypeRecordD e -> RtypeRecordD e
    UtypeVoidU e -> RtypeVoidU e
    _ -> dcastError lc "Rtype" x

instance Ucast Rtype Utype where
  ucast x = case x of
    RtypeScalarI e -> UtypeScalarI e
    RtypeScalarF e -> UtypeScalarF e
    RtypeScalarP e -> UtypeScalarP e
    RtypeVectorI e -> UtypeVectorI e
    RtypeVectorF e -> UtypeVectorF e
    RtypeVectorP e -> UtypeVectorP e
    RtypeRecordD e -> UtypeRecordD e
    RtypeFirstClassD e -> UtypeFirstClassD e
    RtypeVoidU e -> UtypeVoidU e

instance Dcast Rtype Dtype where
  dcast lc x = case x of
    RtypeScalarI e -> DtypeScalarI e
    RtypeScalarF e -> DtypeScalarF e
    RtypeScalarP e -> DtypeScalarP e
    RtypeVectorI e -> DtypeVectorI e
    RtypeVectorF e -> DtypeVectorF e
    RtypeVectorP e -> DtypeVectorP e
    RtypeRecordD e -> DtypeRecordD e
    _ -> dcastError lc "Rtype" x

instance Dcast (Type s r) Rtype where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Ucast (Type x I) Rtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x F) Rtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x P) Rtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type FirstClassB D) Rtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type RecordB D) Rtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type NoB U) Rtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

{- Dtype's dcast, ucast, dcast -}
instance Dcast Utype Dtype where
  dcast lc x = case x of
    UtypeScalarI e -> DtypeScalarI e
    UtypeScalarF e -> DtypeScalarF e
    UtypeScalarP e -> DtypeScalarP e
    UtypeVectorI e -> DtypeVectorI e
    UtypeVectorF e -> DtypeVectorF e
    UtypeVectorP e -> DtypeVectorP e
    UtypeFirstClassD e -> DtypeFirstClassD e    
    UtypeRecordD e -> DtypeRecordD e
    _ -> dcastError lc "Dtype" x

instance Ucast Dtype Utype where
  ucast x = case x of
    DtypeScalarI e -> UtypeScalarI e
    DtypeScalarF e -> UtypeScalarF e
    DtypeScalarP e -> UtypeScalarP e
    DtypeVectorI e -> UtypeVectorI e
    DtypeVectorF e -> UtypeVectorF e
    DtypeVectorP e -> UtypeVectorP e
    DtypeFirstClassD e -> UtypeFirstClassD e    
    DtypeRecordD e -> UtypeRecordD e

instance Ucast Dtype Etype where
  ucast x = case x of
    DtypeScalarI e -> EtypeScalarI e
    DtypeScalarF e -> EtypeScalarF e
    DtypeScalarP e -> EtypeScalarP e
    DtypeVectorI e -> EtypeVectorI e
    DtypeVectorF e -> EtypeVectorF e
    DtypeVectorP e -> EtypeVectorP e
    DtypeFirstClassD e -> EtypeFirstClassD e    
    DtypeRecordD e -> EtypeRecordD e

instance Dcast (Type s r) Dtype where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Ucast (Type x I) Dtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x F) Dtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x P) Dtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type RecordB D) Dtype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Dcast Dtype (Type ScalarB P) where
  dcast lc x = case x of
    DtypeScalarP e -> e
    _ -> dcastError lc "Type ScalarB P" x

instance Dcast Dtype (Type ScalarB I) where
  dcast lc x = case x of
    DtypeScalarI e -> e
    _ -> dcastError lc "Type ScalarB I" x

{- Ftype's dcast, ucast, dcast -}
instance Dcast Utype Ftype where
  dcast lc x = case x of
    UtypeScalarI e -> FtypeScalarI e
    UtypeScalarF e -> FtypeScalarF e
    UtypeScalarP e -> FtypeScalarP e
    UtypeVectorI e -> FtypeVectorI e
    UtypeVectorF e -> FtypeVectorF e
    UtypeVectorP e -> FtypeVectorP e
    UtypeFirstClassD e -> FtypeFirstClassD e
    _ -> dcastError lc "Ftype" x

instance Ucast Ftype Utype where
  ucast x = case x of
    FtypeScalarI e -> UtypeScalarI e
    FtypeScalarF e -> UtypeScalarF e
    FtypeScalarP e -> UtypeScalarP e
    FtypeVectorI e -> UtypeVectorI e
    FtypeVectorF e -> UtypeVectorF e
    FtypeVectorP e -> UtypeVectorP e
    FtypeFirstClassD e -> UtypeFirstClassD e    

instance Dcast (Type s r) Ftype where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Ucast (Type x I) Ftype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x F) Ftype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x P) Ftype where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1


{- ScalarType's dcast, ucast, dcast -}
instance Dcast Utype ScalarType where
  dcast lc x = case x of
    UtypeScalarI e -> ScalarTypeI e
    UtypeScalarF e -> ScalarTypeF e
    UtypeScalarP e -> ScalarTypeP e
    _ -> dcastError lc "ScalarType" x

instance Ucast ScalarType Utype where
  ucast x = case x of
    ScalarTypeI e -> UtypeScalarI e
    ScalarTypeF e -> UtypeScalarF e
    ScalarTypeP e -> UtypeScalarP e

instance Dcast Dtype ScalarType where
  dcast lc x = case x of
    DtypeScalarI e -> ScalarTypeI e
    DtypeScalarF e -> ScalarTypeF e
    DtypeScalarP e -> ScalarTypeP e
    _ -> dcastError lc "ScalarType" x

instance Ucast ScalarType Dtype where
  ucast x = case x of
    ScalarTypeI e -> DtypeScalarI e
    ScalarTypeF e -> DtypeScalarF e
    ScalarTypeP e -> DtypeScalarP e

instance Dcast (Type s r) ScalarType where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Ucast (Type x I) ScalarType where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x F) ScalarType where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type x P) ScalarType where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1


{- IntOrPtrType's dcast, ucast, dcast -}
instance Dcast Utype (IntOrPtrType ScalarB)  where
  dcast lc x = case x of
    UtypeScalarI e -> IntOrPtrTypeI e
    UtypeScalarP e -> IntOrPtrTypeP e
    _ -> dcastError lc "IntOrPtrType ScalarB" x

instance Dcast Utype (IntOrPtrType VectorB)  where
  dcast lc x = case x of
    UtypeVectorI e -> IntOrPtrTypeI e
    UtypeVectorP e -> IntOrPtrTypeP e
    _ -> dcastError lc "IntOrPtrType VectorB" x

instance Ucast (IntOrPtrType ScalarB) Utype where
  ucast x = case x of
    IntOrPtrTypeI e -> UtypeScalarI e
    IntOrPtrTypeP e -> UtypeScalarP e

instance Ucast (IntOrPtrType VectorB) Utype where
  ucast x = case x of
    IntOrPtrTypeI e -> UtypeVectorI e
    IntOrPtrTypeP e -> UtypeVectorP e


instance Dcast (Type s r) (IntOrPtrType ScalarB) where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Dcast (Type s r) (IntOrPtrType VectorB) where
  dcast lc x = let (x1::Utype) = ucast x
               in dcast lc x1

instance Ucast (Type ScalarB I) (IntOrPtrType ScalarB) where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type ScalarB P) (IntOrPtrType ScalarB) where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type VectorB I) (IntOrPtrType VectorB) where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1

instance Ucast (Type VectorB P) (IntOrPtrType VectorB) where
  ucast x = let (x1::Utype) = ucast x
            in dcast (FileLoc "irrefutable") x1


squeeze :: FileLoc -> Type RecordB D -> Type FirstClassB D
squeeze loc x = case x of
  Tstruct pk dl -> Tfirst_class_struct pk (fmap (dcast loc) dl)
  Tarray n el -> Tfirst_class_array n (dcast loc el)
  TnameRecordD e -> Tfirst_class_name e
  TquoteNameRecordD e -> Tfirst_class_quoteName e
  TnoRecordD e -> Tfirst_class_no e
  
  
void :: Type NoB U
void = Tvoid

i1 :: Type ScalarB I
i1 = TpI 1

i8 :: Type ScalarB I
i8 = TpI 8

i16 :: Type ScalarB I
i16 = TpI 16

i32 :: Type ScalarB I
i32 = TpI 32

i64 :: Type ScalarB I
i64 = TpI 64

i128 :: Type ScalarB I
i128 = TpI 128

ptr0 :: Type s r -> Type ScalarB P
ptr0 t = Tpointer (dcast FLC t) 0

half :: Type ScalarB F
half = TpHalf

float :: Type ScalarB F
float = TpFloat

double :: Type ScalarB F
double = TpDouble

fp128 :: Type ScalarB F
fp128 = TpFp128

x86_fp80 :: Type ScalarB F
x86_fp80 = TpX86Fp80


instance Mangle Dtype where
  mangle t = let (t0::Utype) = ucast t 
             in mangle t0
    
instance Mangle Rtype where
  mangle t = let (t0::Utype) = ucast t 
             in mangle t0
    
instance Mangle Etype where
  mangle t = let (t0::Utype) = ucast t 
             in mangle t0

instance Mangle Utype where    
  mangle t = case t of
    UtypeScalarI e -> mangle e
    UtypeScalarF e -> mangle e
    UtypeScalarP e -> mangle e
    UtypeVectorI e -> mangle e
    UtypeVectorF e -> mangle e
    UtypeVectorP e -> mangle e
    UtypeFirstClassD e -> mangle e
    UtypeRecordD e -> mangle e
    UtypeOpaqueD e -> mangle e
    UtypeVoidU e -> mangle e
    UtypeFunX e -> mangle e
    UtypeLabelX e -> mangle e
