
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Llvm.AsmHirConversion.TypeConversion where

import qualified Llvm.Asm.Data.Type as A
import qualified Llvm.Hir.Data.Type as I
import Llvm.Hir.Cast 
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either 
import Llvm.ErrorLoc

#define FLC   (FileLoc $(srcLoc))

type MP = M.Map A.LocalId A.Type

class TypeConversion mp l1 l2 where
  tconvert :: mp -> l1 -> l2
  
{-Ast Type to Ir type conversion -}

instance TypeConversion () I.Mtype A.Type where
  tconvert mp t = case t of
    I.MtypeAsRet d -> tconvert mp d
    I.MtypeData d -> tconvert mp d
    I.MtypeByVal d -> tconvert mp d
    I.MtypeLabel d -> tconvert mp d
    I.MtypeExt _ d -> tconvert mp d
    
instance TypeConversion () I.Dtype A.Type where
  tconvert _ t = case t of
    I.DtypeScalarI x -> tconvert () x
    I.DtypeScalarF x -> tconvert () x
    I.DtypeScalarP x -> tconvert () x
    I.DtypeVectorI x -> tconvert () x
    I.DtypeVectorF x -> tconvert () x
    I.DtypeVectorP x -> tconvert () x
    I.DtypeRecordD x -> tconvert () x
    I.DtypeFirstClassD x -> tconvert () x

instance TypeConversion () I.ScalarType A.Type where
  tconvert _ t = case t of
    I.ScalarTypeI x -> tconvert () x
    I.ScalarTypeF x -> tconvert () x
    I.ScalarTypeP x -> tconvert () x

instance TypeConversion () (I.IntOrPtrType I.ScalarB) A.Type where
  tconvert _ t = case t of
    I.IntOrPtrTypeI x -> tconvert () x
    I.IntOrPtrTypeP x -> tconvert () x

instance TypeConversion () (I.IntOrPtrType I.VectorB) A.Type where
  tconvert _ t = case t of
    I.IntOrPtrTypeI x -> tconvert () x
    I.IntOrPtrTypeP x -> tconvert () x

instance TypeConversion () (Either I.Rtype (I.Type I.CodeFunB I.X)) A.Type where
  tconvert _ (Left e) = tconvert () e
  tconvert _ (Right e) = tconvert () (I.Tpointer (ucast e) 0)

appendAlignment :: Maybe A.AlignInByte -> [A.ParamAttr] -> [A.ParamAttr]
appendAlignment Nothing l = l
appendAlignment (Just (A.AlignInByte n)) l = l ++ [A.PaAlign n]



instance TypeConversion () I.Rtype A.Type where  
  tconvert _ t = case t of
    I.RtypeScalarI x -> tconvert () x
    I.RtypeScalarF x -> tconvert () x    
    I.RtypeScalarP x -> tconvert () x    
    I.RtypeVectorI x -> tconvert () x
    I.RtypeVectorF x -> tconvert () x
    I.RtypeVectorP x -> tconvert () x
    I.RtypeRecordD x -> tconvert () x
    I.RtypeFirstClassD x -> tconvert () x    
    I.RtypeVoidU _ -> A.Tvoid 
    
instance TypeConversion () I.Etype A.Type where
  tconvert _ t = case t of
    I.EtypeScalarI x -> tconvert () x 
    I.EtypeScalarF x -> tconvert () x     
    I.EtypeScalarP x -> tconvert () x     
    I.EtypeVectorI x -> tconvert () x
    I.EtypeVectorF x -> tconvert () x    
    I.EtypeVectorP x -> tconvert () x    
    I.EtypeRecordD x -> tconvert () x
    I.EtypeOpaqueD x -> tconvert () x
    I.EtypeFunX x -> tconvert () x
    
instance TypeConversion () I.Ftype A.Type where
  tconvert _ t = case t of
    I.FtypeScalarI x -> tconvert () x
    I.FtypeScalarF x -> tconvert () x    
    I.FtypeScalarP x -> tconvert () x    
    I.FtypeVectorI x -> tconvert () x
    I.FtypeVectorF x -> tconvert () x    
    I.FtypeVectorP x -> tconvert () x    
    I.FtypeFirstClassD x -> tconvert () x


instance TypeConversion MP [A.Type] [I.Dtype] where
  tconvert mp l = 
    let (l1::[I.Utype]) = fmap (tconvert mp) l
    in (fmap (dcast FLC) l1)

instance TypeConversion () [I.Mtype] [A.Type] where 
  tconvert _ l = fmap (tconvert ()) l

instance TypeConversion MP [A.Type] [I.Mtype] where
  tconvert mp l = 
    let (l1::[I.Utype]) = fmap (tconvert mp) l
    in fmap (\x -> I.MtypeData (dcast FLC x)) l1

instance TypeConversion MP A.Type I.Utype where
  tconvert _ (A.Tprimitive et) = case et of 
    A.TpI n -> I.UtypeScalarI $ I.TpI n
    A.TpF n -> I.UtypeScalarF $ I.TpF n
    A.TpV n -> I.UtypeScalarI $ I.TpV n
    A.TpHalf -> I.UtypeScalarF $ I.TpHalf 
    A.TpFloat -> I.UtypeScalarF $ I.TpFloat
    A.TpDouble -> I.UtypeScalarF $ I.TpDouble
    A.TpFp128 -> I.UtypeScalarF $ I.TpFp128 
    A.TpX86Fp80 -> I.UtypeScalarF $ I.TpX86Fp80
    A.TpPpcFp128 -> I.UtypeScalarF $ I.TpPpcFp128
    A.TpX86Mmx -> I.UtypeScalarI $ I.TpX86Mmx
    A.TpLabel -> I.UtypeLabelX $ I.TpLabel
  tconvert _ A.Tvoid = ucast I.Tvoid
  tconvert mp (A.Tarray n et) = 
    let (eta::I.Utype) = tconvert mp et
    in case eta of
      I.UtypeOpaqueD _ -> I.UtypeOpaqueD $ I.Topaque_array n (dcast FLC eta)
      _ -> I.UtypeRecordD $ I.Tarray n (dcast FLC eta)
  tconvert mp x@(A.Tvector n et) = case matchType mp et of
    Tk_ScalarI -> let et1 = dcast FLC $ ((tconvert mp et)::I.Utype)
                  in I.UtypeVectorI $ I.TvectorI n et1
    Tk_ScalarF -> let et1 = dcast FLC $ ((tconvert mp et)::I.Utype)
                  in I.UtypeVectorF $ I.TvectorF n et1
    Tk_ScalarP -> let et1 = dcast FLC $ ((tconvert mp et)::I.Utype)
                  in I.UtypeVectorP $ I.TvectorP n et1
    _ -> errorLoc FLC $ show x
  tconvert mp (A.Tstruct pk dts) = 
    let (dts0::[I.Utype]) = fmap (tconvert mp) dts
        dts1 = fmap (\e -> case e of
                        I.UtypeOpaqueD ea -> Right ea
                        _ -> Left ((dcast FLC e)::I.Dtype)
                    ) dts0
    in if any isRight dts1 then I.UtypeOpaqueD (I.Topaque_struct pk dts1)
       else I.UtypeRecordD (I.Tstruct pk $ lefts dts1)
  tconvert mp (A.Tpointer et as) = let n1 = case as of
                                         A.AddrSpace n -> n
                                         A.AddrSpaceUnspecified -> 0 
                                       et1 = dcast FLC ((tconvert mp et)::I.Utype)  
                                   in I.UtypeScalarP $ I.Tpointer et1 n1
  tconvert mp A.Topaque = ucast $ I.Topaque
  tconvert mp tn@(A.Tname s) = case getTk mp (castTnameToLocalId tn) of 
    Tk_ScalarI -> ucast $ I.TnameScalarI s
    Tk_ScalarF -> ucast $ I.TnameScalarF s
    Tk_ScalarP -> ucast $ I.TnameScalarP s    
    Tk_VectorI -> ucast $ I.TnameVectorI s
    Tk_VectorF -> ucast $ I.TnameVectorF s
    Tk_VectorP -> ucast $ I.TnameVectorP s
    Tk_RecordD -> ucast $ I.TnameRecordD s
    Tk_CodeFunX -> ucast $ I.TnameCodeFunX s
    Tk_Opaque -> ucast $ I.TnameOpaqueD s
  tconvert mp tn@(A.TquoteName s) = case getTk mp (castTnameToLocalId tn) of 
    Tk_ScalarI -> ucast $ I.TquoteNameScalarI s
    Tk_ScalarF -> ucast $ I.TquoteNameScalarF s    
    Tk_ScalarP -> ucast $ I.TquoteNameScalarP s    
    Tk_VectorI -> ucast $ I.TquoteNameVectorI s
    Tk_VectorF -> ucast $ I.TquoteNameVectorF s    
    Tk_VectorP -> ucast $ I.TquoteNameVectorP s    
    Tk_RecordD -> ucast $ I.TquoteNameRecordD s
    Tk_CodeFunX -> ucast $ I.TquoteNameCodeFunX s
    Tk_Opaque -> ucast $ I.TquoteNameOpaqueD s    
  tconvert mp tn@(A.Tno s) = case getTk mp (castTnameToLocalId tn) of 
    Tk_ScalarI -> ucast $ I.TnoScalarI s
    Tk_ScalarF -> ucast $ I.TnoScalarF s    
    Tk_ScalarP -> ucast $ I.TnoScalarP s    
    Tk_VectorI -> ucast $ I.TnoVectorI s
    Tk_VectorF -> ucast $ I.TnoVectorF s
    Tk_VectorP -> ucast $ I.TnoVectorP s
    Tk_RecordD -> ucast $ I.TnoRecordD s
    Tk_CodeFunX -> ucast $ I.TnoCodeFunX s
    Tk_Opaque -> ucast $ I.TnoOpaqueD s
  tconvert mp (A.Tfunction rt (A.TypeParamList tp mv) fa) = 
    let rt1 = dcast FLC ((tconvert mp rt)::I.Utype)
    in I.UtypeFunX (I.Tfunction (rt1,[]) (fmap (\x -> (x, Nothing)) $ tconvert mp tp) mv)
                                    
instance TypeConversion MP A.AddrSpace I.AddrSpace where
  tconvert _ (A.AddrSpace n) = n
  tconvert _ (A.AddrSpaceUnspecified) = 0

instance TypeConversion () I.AddrSpace A.AddrSpace where    
  tconvert _ 0 = A.AddrSpaceUnspecified
  tconvert _ n = A.AddrSpace n
  
instance TypeConversion MP A.MetaKind I.MetaKind where
  tconvert mp x = case x of
    A.Mtype e -> I.MKtype (tconvert mp e)
    A.Mmetadata -> I.MKmetadata

  
instance TypeConversion () I.MetaKind A.MetaKind where
  tconvert _ x = case x of
    I.MKtype e -> A.Mtype (tconvert () e)
    I.MKmetadata -> A.Mmetadata


instance TypeConversion () (I.Type s r) A.Type where
  tconvert _ t = case t of
    I.TpI n -> A.Tprimitive $ A.TpI n
    I.TpF n -> A.Tprimitive $ A.TpF n
    I.TpV n -> A.Tprimitive $ A.TpV n
    I.TpHalf -> A.Tprimitive $ A.TpHalf 
    I.TpFloat -> A.Tprimitive $ A.TpFloat
    I.TpDouble -> A.Tprimitive $ A.TpDouble
    I.TpFp128 -> A.Tprimitive $ A.TpFp128 
    I.TpX86Fp80 -> A.Tprimitive $ A.TpX86Fp80
    I.TpPpcFp128 -> A.Tprimitive $ A.TpPpcFp128
    I.TpX86Mmx -> A.Tprimitive $ A.TpX86Mmx
    I.Tpointer el 0 -> A.Tpointer (tconvert () el) A.AddrSpaceUnspecified
    I.Tpointer el as -> A.Tpointer (tconvert () el) (A.AddrSpace as)
    
    I.Tarray n dt -> A.Tarray n (tconvert () dt)
    I.Tstruct p dts -> A.Tstruct p (fmap (tconvert ()) dts)
    
    I.Tfirst_class_array n dt -> A.Tarray n (tconvert () dt)
    I.Tfirst_class_struct p dts -> A.Tstruct p (fmap (tconvert ()) dts)
    
    I.Tfirst_class_no n -> A.Tno n
    I.Tfirst_class_name s -> A.Tname s
    I.Tfirst_class_quoteName s -> A.TquoteName s

    I.Topaque_struct p dts -> A.Tstruct p (fmap (either (tconvert()) (tconvert ())) dts)
    I.Topaque_array n dt -> A.Tarray n (tconvert () dt)
    
    I.TvectorI n dt -> A.Tvector n (tconvert () dt)
    I.TvectorF n dt -> A.Tvector n (tconvert () dt)    
    I.TvectorP n dt -> A.Tvector n (tconvert () dt)    
    I.Tvoid -> A.Tvoid
    
    I.TnoScalarI n -> A.Tno n
    I.TnameScalarI s -> A.Tname s
    I.TquoteNameScalarI s -> A.TquoteName s
    
    I.TnoScalarF n -> A.Tno n
    I.TnameScalarF s -> A.Tname s
    I.TquoteNameScalarF s -> A.TquoteName s

    I.TnoScalarP n -> A.Tno n
    I.TnameScalarP s -> A.Tname s
    I.TquoteNameScalarP s -> A.TquoteName s

    I.TnoVectorI n -> A.Tno n
    I.TnameVectorI s -> A.Tname s
    I.TquoteNameVectorI s -> A.TquoteName s
    
    I.TnoVectorF n -> A.Tno n
    I.TnameVectorF s -> A.Tname s
    I.TquoteNameVectorF s -> A.TquoteName s

    I.TnoVectorP n -> A.Tno n
    I.TnameVectorP s -> A.Tname s
    I.TquoteNameVectorP s -> A.TquoteName s

    I.TnoRecordD n -> A.Tno n
    I.TnameRecordD s -> A.Tname s
    I.TquoteNameRecordD s -> A.TquoteName s
    
    I.TnoCodeFunX n -> A.Tno n
    I.TnameCodeFunX s -> A.Tname s
    I.TquoteNameCodeFunX s -> A.TquoteName s

    I.TnoOpaqueD n -> A.Tno n
    I.TnameOpaqueD s -> A.Tname s
    I.TquoteNameOpaqueD s -> A.TquoteName s

    I.Tfunction (rt,_) tp ma -> 
      A.Tfunction (tconvert () rt) (A.TypeParamList (tconvert () $ fmap fst tp) ma) []
    I.TpNull -> A.Tprimitive A.TpNull
    I.TpLabel -> A.Tprimitive A.TpLabel
    I.Topaque -> A.Topaque

instance TypeConversion () I.Utype A.Type where
  tconvert _ t = case t of
    I.UtypeScalarI x -> tconvert () x
    I.UtypeScalarF x -> tconvert () x
    I.UtypeScalarP x -> tconvert () x
    I.UtypeVectorI x -> tconvert () x
    I.UtypeVectorF x -> tconvert () x
    I.UtypeVectorP x -> tconvert () x
    I.UtypeRecordD x -> tconvert () x
    I.UtypeFunX x -> tconvert () x
    I.UtypeLabelX x -> tconvert () x
    I.UtypeOpaqueD x -> tconvert () x
    I.UtypeVoidU x -> tconvert () x

data Tk = Tk_ScalarI
        | Tk_ScalarP
        | Tk_ScalarF
        | Tk_VectorI
        | Tk_VectorF          
        | Tk_VectorP          
        | Tk_RecordD
        | Tk_CodeFunX
        | Tk_CodeLabelX
        | Tk_Opaque
             
             
getTk :: MP -> A.LocalId -> Tk
getTk mp lid = case M.lookup lid mp of 
  Nothing -> error $ "undefined " ++ show lid
  Just e -> matchType mp e

matchType :: MP -> A.Type -> Tk
matchType mp t = case t of
  A.Tprimitive e -> case e of
    A.TpI _ -> Tk_ScalarI
    A.TpF _ -> Tk_ScalarF
    A.TpV _ -> Tk_ScalarI
    A.TpHalf -> Tk_ScalarF
    A.TpFloat -> Tk_ScalarF
    A.TpDouble -> Tk_ScalarF
    A.TpFp128 -> Tk_ScalarF
    A.TpX86Fp80 -> Tk_ScalarF
    A.TpPpcFp128 -> Tk_ScalarF
    A.TpX86Mmx -> Tk_ScalarI
    A.TpLabel -> Tk_CodeLabelX
    A.TpNull ->  errorLoc FLC "TpNull"
  A.Tarray _ _ -> Tk_RecordD
  A.Tstruct _ _ -> Tk_RecordD
  A.Tpointer _ _ -> Tk_ScalarP
  A.Tfunction _ _ _ -> Tk_CodeFunX
  A.Tvector _ e -> let ek = matchType mp e
                   in case ek of
                     Tk_ScalarI -> Tk_VectorI
                     Tk_ScalarF -> Tk_VectorF
                     Tk_ScalarP -> Tk_VectorP
                     _ -> error $ "TypeConversion.matchType"
  A.Tname _ -> getTk mp (castTnameToLocalId t)
  A.TquoteName _ -> getTk mp (castTnameToLocalId t)
  A.Tno _ -> getTk mp (castTnameToLocalId t)
  A.Topaque -> Tk_Opaque
  _ -> errorLoc FLC (show t)


castTnameToLocalId :: A.Type -> A.LocalId    
castTnameToLocalId x = case x of
  A.Tname s -> A.LocalIdAlphaNum s
  A.TquoteName s -> A.LocalIdDqString s
  A.Tno s -> A.LocalIdNum s  