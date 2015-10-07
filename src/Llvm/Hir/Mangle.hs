{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards #-}
module Llvm.Hir.Mangle where

import Llvm.Hir.Data.Inst
import Llvm.Hir.Cast
import Llvm.Hir.Print

class Mangle a where
  mangle :: a -> String
  
replaceDq :: String -> String
replaceDq s = fmap (\x -> case x of 
                       '"' -> '_' 
                       ' ' -> '_'
                       _ -> x) s

instance Mangle a => Mangle [a] where  
  mangle l = concat $ fmap mangle l
  
instance Mangle a => Mangle (Maybe a) where  
  mangle x = case x of
    Nothing -> ""
    Just e -> mangle e

instance (Mangle l, Mangle r) => Mangle (Either l r) where  
  mangle e = case e of
    Left l -> mangle l
    Right r -> mangle r
    
instance (Mangle l, Mangle r) => Mangle (l, r) where  
  mangle (l,r) = mangle l ++ mangle r

instance (IrPrint g, Mangle g) => Mangle (Const g) where
  mangle c = replaceDq $ render $ printIr c

instance Mangle Dtype where
  mangle t = let (t0::Utype) = ucast t 
             in replaceDq $ mangle t0

instance Mangle Gname where
  mangle x = case x of
    Gname n -> n

instance Mangle Ext where
  mangle e = case e of
    Sign -> "s"
    Zero -> "z"
    
instance Mangle Mtype where
  mangle t = case t of
    MtypeAsRet dt -> "sret" ++ mangle dt
    MtypeByVal dt -> "byval" ++ mangle dt
    MtypeExt e dt -> mangle e ++ mangle dt
    MtypeData dt -> mangle dt
    MtypeLabel dt -> mangle dt

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

instance (IrPrint g, Mangle g) => Mangle (FunPtr g) where
  mangle t = case t of
    FunId n -> "f." ++ mangle n
    FunIdBitcast tv d -> "f." ++ (render $ printIr tv) ++ mangle d
    FunIdInttoptr tv d -> "f." ++ (render $ printIr tv) ++ mangle d
    FunSsa x -> "f." ++ mangle x
    Fun_null -> "f.null"
    Fun_undef -> "f.undef"

instance Mangle Lname where  
  mangle (Lname t) = replaceDq t

instance Mangle ScalarType where
  mangle t = case t of
    ScalarTypeI x -> mangle x
    ScalarTypeF x -> mangle x
    ScalarTypeP x -> mangle x

instance Mangle Word8 where
  mangle x = show x
  
instance Mangle Word32 where
  mangle x = show x

instance Mangle Word64 where
  mangle x = show x

instance Mangle TailCall where
  mangle x = render $ printIr x
  
instance Mangle CallConv where
  mangle x = render $ printIr x
  
instance Mangle AlignInByte where
  mangle (AlignInByte x) = "a:" ++ show x
  
instance Mangle CallSiteType where
  mangle x = case x of
    CallSiteTypeRet t -> render $ printIr t
    CallSiteTypeFun t as -> render $ printIr t
  
instance Mangle Packing where
  mangle x = case x of
    Packed -> "PK"
    Unpacked -> "UNPK"
  
instance Mangle VarArgParam where
  mangle _ = "Z"
  
{-
instance Mangle TypeParamList where
  mangle (TypeParamList l va) = "(" ++ mangle l ++ mangle va ++ ")"
-}

instance Mangle FunAttr where
  mangle x = render $ printIr x
  
instance Mangle ParamAttr where
  mangle x = render $ printIr x
  
instance Mangle PAttr where
  mangle x = render $ printIr x


instance Mangle RetAttr where
  mangle x = render $ printIr x

instance Mangle a => Mangle (FunOperand a) where
  mangle x = case x of
    FunOperandData d atts align a -> 
      mangle d ++ mangle atts ++ mangle align ++ mangle a
    FunOperandExt e d atts align a -> 
      "ext" ++ mangle e ++ mangle d ++ mangle atts ++ mangle align ++ mangle a
    FunOperandByVal d atts align a -> 
      "byval" ++ mangle d ++ mangle atts ++ mangle align ++ mangle a    
    FunOperandAsRet d atts align a -> 
      "sret" ++ mangle d ++ mangle atts ++ mangle align ++ mangle a        
    FunOperandLabel d atts align a -> 
      mangle d ++ mangle atts ++ mangle align ++ mangle a

instance Mangle (Type s r) where
  mangle x = case x of
    TpI n -> "i" ++ mangle n
    TpF n -> "f" ++ mangle n
    TpV n -> "v" ++ mangle n
    Tvoid -> "void"
    TpHalf -> "F1_"
    TpFloat -> "f"
    TpDouble -> "d"
    TpFp128 -> "F4_"
    TpX86Fp80 -> "F5_"
    TpPpcFp128 -> "F6_"
    TpX86Mmx -> "F7_"
    TpNull -> "null"
    TpLabel -> "label"
    Topaque -> "opaque"
    Tarray n d -> "A_" ++ mangle n ++ "_" ++ mangle d

    TvectorI n d -> "vi_" ++ mangle n ++ "_" ++ mangle d
    TvectorF n d -> "vf_" ++ mangle n ++ "_" ++ mangle d
    TvectorP n d -> "vp_" ++ mangle n ++ "_" ++ mangle d

    Tstruct p ds -> "S_" ++ mangle p ++ "_" ++ mangle ds
    Tpointer e as -> "P_" ++ mangle e ++ "_" ++ show as
    Tfunction (rt,atts) tp mv -> "F_" ++ mangle rt ++ mangle atts ++ "_" ++ mangle tp ++ "_" ++ mangle mv
    {- Scalar -}
    TnameScalarI s -> show s
    TnameScalarF s -> show s
    TnameScalarP s -> show s
    {- Vector -}
    TnameVectorI s -> show s
    TnameVectorF s -> show s
    TnameVectorP s -> show s
    {- Large -}
    TnameRecordD s -> show s
    {- Code -}
    TnameCodeFunX s -> show s
    {- Opaque -}
    TnameOpaqueD s -> show s
    
    Topaque_struct pk l -> "os_" ++ mangle pk ++ "_" ++ mangle l
    Topaque_array n e -> "oa_" ++ mangle n ++ "_" ++ show e 
    
    Tfirst_class_array n e -> "1ca_" ++ mangle n ++ " " ++ mangle e
    Tfirst_class_struct pk l -> "1cs_" ++ mangle pk ++ " " ++ mangle l
    Tfirst_class_name s -> show s

instance Mangle () where
  mangle () = ""
  
instance Mangle a => Mangle (FunSignature a) where
  mangle FunSignature { fs_callConv = cc
                      , fs_type = typ
                      , fs_params = pas
                      } = mangle cc ++ mangle typ ++ mangle pas