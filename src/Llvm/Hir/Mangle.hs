{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Llvm.Hir.Mangle where

import Llvm.Hir.Data.Inst
import Llvm.Hir.Data.Type
import Llvm.Hir.Cast

class Mangle a where
  mangle :: a -> String
  
replaceDq :: String -> String
replaceDq s = fmap (\x -> if x == '"' then '_' else x) s
  

instance Mangle a => Mangle ([a]) where  
  mangle l = foldl (\p e -> p ++ (mangle e)) "" l
  
  
instance Mangle Const where
  mangle c = fmap (\c -> if c == '"' then '_' else c) $ show c


instance Mangle Dtype where
  mangle t = let (t0::Utype) = ucast t 
             in replaceDq $ mangle t0
    
instance Mangle Rtype where
  mangle t = let (t0::Utype) = ucast t 
             in replaceDq $ mangle t0
    
instance Mangle Etype where
  mangle t = let (t0::Utype) = ucast t 
             in replaceDq $ mangle t0

instance Mangle Utype where    
  mangle t = let s = case t of
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
             in replaceDq s

instance Mangle TypeParamList where
  mangle (TypeParamList l va) = "(" ++ mangle l ++ show va ++ ")"


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
