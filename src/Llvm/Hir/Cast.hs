{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances, GADTs
, RecordWildCards, FunctionalDependencies, CPP, TemplateHaskell #-}

module Llvm.Hir.Cast where
import Llvm.Hir.Data.Inst
import Llvm.Hir.Data.Type
import Llvm.Hir.Data.Module
import Data.Int
import Llvm.ErrorLoc

#define FLC (FileLoc $(srcLoc))

{- 

This module contains the functions to cast one Ir data type to
another. Only compatable data types should be involved in casting. 

In some cases, casting is statically safe, which is normally known as
up casting (or denoted as Ucast). In some cases, casting can only be
decided at runtime, which is normally known as down casting (or
denoted as Dcast). Dcast might throw out irrefutable fatal errors if
incompatible types are involved. In such a situation, your code should
filter out incompatible types before feeding them to Dcast.

-}

{- up casting -}
class Ucast l1 l2 where
  ucast :: l1 -> l2

{- down casting -}
class Dcast l1 l2 where
  dcast :: FileLoc -> l1 -> l2
  

instance Ucast l l where
  ucast = id
  
instance Dcast l l where  
  dcast _ = id
  
{- A datum that can be casted to a constant -}
class Cvalue x where
  toConst :: x -> Const g

instance Cvalue Int32 where
  toConst = C_s32
  
instance Cvalue Int64 where
  toConst = C_s64

instance Cvalue Word32 where  
  toConst = C_u32

instance Cvalue Word64 where  
  toConst = C_u64

{- A data that can be casted to a value -}
class Rvalue x where
  toRvalue :: x -> Value g

instance Rvalue Lname where 
  toRvalue = Val_ssa


{-
instance Rvalue (GlobalId g) g where
  toRvalue g = Val_const (C_globalAddr g)

instance Rvalue (Const g) g where
  toRvalue = Val_const 
-}

instance Rvalue Int32 where
  toRvalue = Val_const . C_s32 
  
instance Rvalue Word32 where
  toRvalue = Val_const . C_u32

instance Rvalue Word64 where
  toRvalue = Val_const . C_u64


{- Typed Integer Constant, Typed Integer Constants are often used in
 indexing memory elements. 
-}
class TC x where
  toTC :: x -> T (Type ScalarB I) (Const g)

instance TC Word32 where
  toTC x = T (TpI 32) (toConst x)

instance TC Word64 where
  toTC x = T (TpI 64) (toConst x)

instance TC Int32 where
  toTC x = T (TpI 32) (toConst x)


{- Typed Integer Value. -}
class TV x where
  toTV :: x -> T (Type ScalarB I) (Value g)

instance TV Word32 where
  toTV x = T (TpI 32) (toRvalue x)

instance TV Int32 where
  toTV x = T (TpI 32) (toRvalue x)
  
  
toTVs :: TV x => [x] -> [T (Type ScalarB I) (Value g)]   
toTVs l = fmap toTV l

toTCs :: TC x => [x] -> [T (Type ScalarB I) (Const g)]   
toTCs l = fmap toTC l

i32sToTvs :: [Int32] -> [T (Type ScalarB I) (Value g)]
i32sToTvs = toTVs
  
i32sToTcs :: [Int32] -> [T (Type ScalarB I) (Const g)]
i32sToTcs = toTCs

u32sToTvs :: [Word32] -> [T (Type ScalarB I) (Value g)]
u32sToTvs = toTVs

u32sToTcs :: [Word32] -> [T (Type ScalarB I) (Const g)]
u32sToTcs = toTCs

i32ToTv :: Int32 -> T (Type ScalarB I) (Value g)
i32ToTv = toTV
            
u32ToTv :: Word32 -> T (Type ScalarB I) (Value g)            
u32ToTv = toTV



instance Ucast (Const g) (Value g) where
  ucast = Val_const

instance Show g => Dcast (Value g) (Const g) where
  dcast lc x = case x of
    Val_const v -> v
    _ -> dcastError lc "Const" x

instance (Ucast t s, Ucast u v) => Ucast (T t u) (T s v) where
  ucast (T t u) = T (ucast t) (ucast u)

instance (Dcast s t, Dcast v u) => Dcast (T s v) (T t u) where
  dcast lc (T s v) = T (dcast lc s) (dcast lc v)




{- Type s r, which represents all types,  ucast to Utype -}
  
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
    
    Tarray _ _ -> UtypeRecordD x
    Tstruct _ _ -> UtypeRecordD x
    
    Topaque_struct _ _ -> UtypeOpaqueD x
    Topaque_array _ _ -> UtypeOpaqueD x    
    
    Tpointer _ _ -> UtypeScalarP x
    Tfunction _ _ _ -> UtypeFunX x

    {- Scalar -}
    TnameScalarI _ -> UtypeScalarI x
    TnameScalarF _ -> UtypeScalarF x
    TnameScalarP _ -> UtypeScalarP x
    {- Vector -}
    TnameVectorI _ -> UtypeVectorI x
    TnameVectorF _ -> UtypeVectorF x
    TnameVectorP _ -> UtypeVectorP x
    {- Large -}
    TnameRecordD _ -> UtypeRecordD x
    {- Code -}
    TnameCodeFunX _ -> UtypeFunX x
    {- Opaque -}
    TnameOpaqueD _ -> UtypeOpaqueD x

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


instance Ucast Dtype Rtype where
  ucast x = case x of
    DtypeScalarI e -> RtypeScalarI e
    DtypeScalarF e -> RtypeScalarF e
    DtypeScalarP e -> RtypeScalarP e
    DtypeVectorI e -> RtypeVectorI e
    DtypeVectorF e -> RtypeVectorF e
    DtypeVectorP e -> RtypeVectorP e
    DtypeFirstClassD e -> RtypeFirstClassD e    
    DtypeRecordD e -> RtypeRecordD e

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

instance Ucast (Type FirstClassB D) Dtype where
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

instance Dcast Dtype (Type RecordB D) where
  dcast lc x = case x of
    DtypeRecordD e -> e
    _ -> dcastError lc "Type RecordB D" x

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

instance Ucast Ftype Dtype where
  ucast x = case x of
    FtypeScalarI e -> DtypeScalarI e
    FtypeScalarF e -> DtypeScalarF e
    FtypeScalarP e -> DtypeScalarP e
    FtypeVectorI e -> DtypeVectorI e
    FtypeVectorF e -> DtypeVectorF e
    FtypeVectorP e -> DtypeVectorP e
    FtypeFirstClassD e -> DtypeFirstClassD e    


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

uc :: Ucast a b => a -> b
uc x = ucast x

dc :: Dcast a b => FileLoc -> String -> a -> b
dc lc s x = dcast lc x

mapUtype :: (Type CodeFunB X -> Type CodeFunB X) -> Dtype -> Dtype
mapUtype f x = case x of
  DtypeScalarI n -> x
  DtypeScalarF n -> x
  DtypeScalarP n -> DtypeScalarP $ mapType f n
  DtypeVectorI n -> x
  DtypeVectorF n -> x
  DtypeVectorP n -> DtypeVectorP $ mapType f n
  DtypeFirstClassD n -> DtypeFirstClassD $ mapType f n
  DtypeRecordD n -> DtypeRecordD $ mapType f n

mapEtype :: (Type CodeFunB X -> Type CodeFunB X) -> Etype -> Etype
mapEtype f x = case x of
  EtypeScalarI n -> x
  EtypeScalarF n -> x
  EtypeScalarP n -> EtypeScalarP $ mapType f n
  EtypeVectorI n -> x
  EtypeVectorF n -> x
  EtypeVectorP n -> EtypeVectorP $ mapType f n
  EtypeFirstClassD n ->  EtypeFirstClassD $ mapType f n
  EtypeRecordD n -> EtypeRecordD $ mapType f n
  EtypeOpaqueD n -> x
  EtypeFunX n -> EtypeFunX $ mapType f n

mapRtype :: (Type CodeFunB X -> Type CodeFunB X) -> Rtype -> Rtype
mapRtype f x = case x of
  RtypeScalarI n -> x
  RtypeScalarF n -> x
  RtypeScalarP n -> RtypeScalarP $ mapType f n
  RtypeVectorI n -> x
  RtypeVectorF n -> x
  RtypeVectorP n -> RtypeVectorP $ mapType f n
  RtypeFirstClassD n -> RtypeFirstClassD $ mapType f n
  RtypeRecordD n -> RtypeRecordD $ mapType f n
  RtypeVoidU n -> x

mapMtype :: (Type CodeFunB X -> Type CodeFunB X) -> Mtype -> Mtype
mapMtype f x = case x of
  MtypeAsRet n -> MtypeAsRet (mapUtype f n)
  MtypeData n -> MtypeData (mapUtype f n)
  MtypeByVal n -> MtypeByVal (mapUtype f n)
  MtypeExt e n -> MtypeExt e (mapUtype f n)
  MtypeLabel ft -> MtypeLabel ft

mapStype :: (Type CodeFunB X -> Type CodeFunB X) -> ScalarType -> ScalarType
mapStype f x = case x of
  ScalarTypeI n -> x
  ScalarTypeF n -> x
  ScalarTypeP n -> ScalarTypeP $ mapType f n

mapType :: (Type CodeFunB X -> Type CodeFunB X) -> Type s r -> Type s r
mapType f x = case x of
    TpI n -> x
    TpF n -> x
    TpV n -> x
    Tvoid -> x
    TpHalf -> x
    TpFloat -> x
    TpDouble -> x
    TpFp128 -> x
    TpX86Fp80 -> x
    TpPpcFp128 -> x
    TpX86Mmx -> x
    TpNull -> x
    TpLabel -> x
    Topaque -> x
    Tarray n d -> Tarray n (mapUtype f d)
    TvectorI n d -> x      
    TvectorF n d -> x
    TvectorP n d -> TvectorP n (mapType f d)
    Tstruct p ds -> Tstruct p (fmap (mapUtype f) ds)
    Tpointer e as -> Tpointer (mapEtype f e) as
    Tfunction (rt,ra) tp mv -> 
      let t = Tfunction (mapRtype f rt, ra) (fmap (\(mt, ma) -> (mapMtype f mt, ma)) tp) mv
      in f t
    {- Scalar -}
    TnameScalarI s -> x
    TnameScalarF s -> x
    TnameScalarP s -> x
    {- Vector -}
    TnameVectorI s -> x
    TnameVectorF s -> x
    TnameVectorP s -> x
    {- Large -}
    TnameRecordD s -> x
    {- Code -}
    TnameCodeFunX s -> x
    {- Opaque -}
    TnameOpaqueD s -> x
    Topaque_struct pk l -> x
    Topaque_array n e -> x

    Tfirst_class_array n e -> Tfirst_class_array n (mapStype f e)
    Tfirst_class_struct pk l -> Tfirst_class_struct pk (fmap (mapStype f) l)
    Tfirst_class_name s -> x