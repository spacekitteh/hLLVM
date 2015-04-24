{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Llvm.Pass.RewriterEx where

import Control.Monad
import Data.Maybe
import Prelude hiding (succ)

import qualified Compiler.Hoopl as H

import Llvm.Data.Ir
import Llvm.Query.TypeConstValue
import Llvm.Util.Monadic (maybeM)
import Debug.Trace
import Control.Monad

import Llvm.Pass.Changer

type MaybeChange a = a -> Maybe a


f2t :: (a -> Maybe a) -> (T t a, T t a) -> Maybe (T t a, T t a) 
f2t f ((T t1 a1), (T t2 a2)) = case (f a1, f a2) of
  (Nothing, Nothing) -> Nothing
  (a1', a2') -> Just (T t1 $ fromMaybe a1 a1', T t2 $ fromMaybe a2 a2')


changeT2 :: (a -> Maybe a) -> (b -> Maybe b) -> (a, b) -> Maybe (a, b) 
changeT2 fa fb (a, b) = case (fa a, fb b) of
  (Nothing, Nothing) -> Nothing
  (a1, b1) -> Just (fromMaybe a a1, fromMaybe b b1)


f3 :: (a -> Maybe a) -> (T t a, T t a, T t a) -> Maybe (T t a, T t a, T t a) 
f3 f (T t1 a1, T t2 a2, T t3 a3) = case (f a1, f a2, f a3) of
  (Nothing, Nothing, Nothing) -> Nothing
  (a1', a2', a3') -> Just (T t1 $ fromMaybe a1 a1'
                          , T t2 $ fromMaybe a2 a2'
                          , T t3 $ fromMaybe a3 a3')


fs :: (Eq t, Eq a) => (a -> Maybe a) -> [T t a] -> Maybe [T t a]
fs f ls = let ls' = map (\(T t x) -> T t (fromMaybe x (f x))) ls
          in if ls == ls' then Nothing else Just ls'
 
                                            
change_List :: (Eq t, Eq a) => (a -> Maybe a) -> [T t a] -> Maybe [T t a]
change_List f ls = let ls' = map (\(T t x) -> T t (fromMaybe x (f x))) ls
                   in if ls == ls' then Nothing else Just ls'

change :: (a -> Maybe a) -> a -> a
change c a = fromMaybe a (c a)


mchange_TypedConstOrNull :: Changer -> MaybeChange TypedConstOrNull
mchange_TypedConstOrNull chg@Changer{..} x = case x of
  TypedConst (T dt c) -> case mchange_Const chg c of
    Just c0 -> Just $ TypedConst (T dt c0)
    Nothing -> Nothing
  UntypedNull -> Nothing


mchange_Const :: Changer -> MaybeChange Const
mchange_Const chg@Changer{..} cst = 
  let cst1 = case cst of
        C_struct pk l -> C_struct pk (fmap (change (mchange_TypedConstOrNull chg)) l)
        C_vector l -> C_vector (fmap (change (mchange_TypedConstOrNull chg)) l)
        C_vectorN n e -> C_vectorN n (change (mchange_TypedConstOrNull chg) e)
        C_array l -> C_array (fmap (change (mchange_TypedConstOrNull chg)) l)
        C_arrayN n e -> C_arrayN n (change (mchange_TypedConstOrNull chg) e)
        C_add n t c1 c2 -> change2c (C_add n t) c1 c2
        C_sub n t c1 c2 -> change2c (C_sub n t) c1 c2
        C_mul n t c1 c2 -> change2c (C_mul n t) c1 c2
        C_udiv n t c1 c2 -> change2c (C_udiv n t) c1 c2 
        C_sdiv n t c1 c2 -> change2c (C_sdiv n t) c1 c2
        C_urem t c1 c2 -> change2c (C_urem t) c1 c2
        C_srem t c1 c2 -> change2c (C_srem t) c1 c2
        C_shl n t c1 c2 -> change2c (C_shl n t) c1 c2
        C_lshr n t c1 c2 -> change2c (C_lshr n t) c1 c2
        C_ashr n t c1 c2 -> change2c (C_ashr n t) c1 c2
        C_and t c1 c2 -> change2c (C_and t) c1 c2
        C_or t c1 c2 -> change2c (C_or t) c1 c2 
        C_xor t c1 c2 -> change2c (C_xor t) c1 c2

        C_add_V n t c1 c2 -> change2c (C_add_V n t) c1 c2
        C_sub_V n t c1 c2 -> change2c (C_sub_V n t) c1 c2
        C_mul_V n t c1 c2 -> change2c (C_mul_V n t) c1 c2
        C_udiv_V n t c1 c2 -> change2c (C_udiv_V n t) c1 c2
        C_sdiv_V n t c1 c2 -> change2c (C_sdiv_V n t) c1 c2
        C_urem_V t c1 c2 -> change2c (C_urem_V t) c1 c2
        C_srem_V t c1 c2 -> change2c (C_srem_V t) c1 c2
        C_shl_V n t c1 c2 -> change2c (C_shl_V n t) c1 c2
        C_lshr_V n t c1 c2 -> change2c (C_lshr_V n t) c1 c2
        C_ashr_V n t c1 c2 -> change2c (C_ashr_V n t) c1 c2
        C_and_V t c1 c2 -> change2c (C_and_V t) c1 c2
        C_or_V t c1 c2 -> change2c (C_or_V t) c1 c2
        C_xor_V t c1 c2 -> change2c (C_xor_V t) c1 c2

        C_fadd n t c1 c2 -> change2c (C_fadd n t) c1 c2
        C_fsub n t c1 c2 -> change2c (C_fsub n t) c1 c2
        C_fmul n t c1 c2 -> change2c (C_fmul n t) c1 c2
        C_fdiv n t c1 c2 -> change2c (C_fdiv n t) c1 c2
        C_frem n t c1 c2 -> change2c (C_frem n t) c1 c2

        C_fadd_V n t c1 c2 -> change2c (C_fadd_V n t) c1 c2 
        C_fsub_V n t c1 c2 -> change2c (C_fsub_V n t) c1 c2 
        C_fmul_V n t c1 c2 -> change2c (C_fmul_V n t) c1 c2
        C_fdiv_V n t c1 c2 -> change2c (C_fdiv_V n t) c1 c2
        C_frem_V n t c1 c2 -> change2c (C_frem_V n t) c1 c2

        C_trunc tc tdest -> change1Tc chg (\x -> C_trunc x tdest) tc
        C_zext tc tdest -> change1Tc chg (\x -> C_zext x tdest) tc
        C_sext tc tdest -> change1Tc chg (\x -> C_sext x tdest) tc
        C_fptrunc tc tdest -> change1Tc chg (\x -> C_fptrunc x tdest) tc
        C_fpext tc tdest -> change1Tc chg (\x -> C_fpext x tdest) tc
        C_fptoui tc tdest -> change1Tc chg (\x -> C_fptoui x tdest) tc
        C_fptosi tc tdest -> change1Tc chg (\x -> C_fptosi x tdest) tc
        C_uitofp tc tdest -> change1Tc chg (\x -> C_uitofp x tdest) tc
        C_sitofp tc tdest -> change1Tc chg (\x -> C_sitofp x tdest) tc
        C_ptrtoint tc tdest -> change1Tc chg (\x -> C_ptrtoint x tdest) tc
        C_inttoptr tc tdest -> change1Tc chg (\x -> C_inttoptr x tdest) tc
        C_bitcast tc tdest -> change1Tc chg (\x -> C_bitcast x tdest) tc
        C_addrspacecast tc tdest -> change1Tc chg (\x -> C_addrspacecast x tdest) tc

        C_trunc_V tc tdest -> change1Tc chg (\x -> C_trunc_V x tdest) tc
        C_zext_V tc tdest -> change1Tc chg (\x -> C_zext_V x tdest) tc
        C_sext_V tc tdest -> change1Tc chg (\x -> C_sext_V x tdest) tc
        C_fptrunc_V tc tdest -> change1Tc chg (\x -> C_fptrunc_V x tdest) tc
        C_fpext_V tc tdest -> change1Tc chg (\x -> C_fpext_V x tdest) tc
        C_fptoui_V tc tdest -> change1Tc chg (\x -> C_fptoui_V x tdest) tc
        C_fptosi_V tc tdest -> change1Tc chg (\x -> C_fptosi_V x tdest) tc
        C_uitofp_V tc tdest -> change1Tc chg (\x -> C_uitofp_V x tdest) tc
        C_sitofp_V tc tdest -> change1Tc chg (\x -> C_sitofp_V x tdest) tc
        C_ptrtoint_V tc tdest -> change1Tc chg (\x -> C_ptrtoint_V x tdest) tc
        C_inttoptr_V tc tdest -> change1Tc chg (\x -> C_inttoptr_V x tdest) tc
        C_addrspacecast_V tc tdest -> change1Tc chg (\x -> C_addrspacecast_V x tdest) tc

        C_getelementptr ib (T t c) l -> cst
        C_getelementptr_V ib (T t c) l -> cst

        C_select_I _ -> cst
        C_select_F _ -> cst
        C_select_P _ -> cst

        C_select_First _ _ _ -> cst

        C_select_VI _ -> cst
        C_select_VF _ -> cst
        C_select_VP _ -> cst

        C_icmp _ -> cst
        C_icmp_V _ -> cst

        C_fcmp _ -> cst
        C_fcmp_V _ -> cst

        C_shufflevector_I _ -> cst
        C_shufflevector_F _ -> cst
        C_shufflevector_P _ -> cst

        C_extractelement_I _ -> cst
        C_extractelement_F _ -> cst
        C_extractelement_P _ -> cst

        C_insertelement_I _ -> cst
        C_insertelement_F _ -> cst
        C_insertelement_P _ -> cst

        C_extractvalue _ -> cst
        C_insertvalue _ -> cst
        _ -> cst
        
  in let cst2 = change_Const cst1
     in if cst == cst2 then Nothing
        else Just cst2
  where change2c cf a b = cf (change (mchange_Const chg) a) (change (mchange_Const chg) b)

        
change1Tc :: Changer -> (T t Const -> b) -> (T t Const) -> b
change1Tc chg cf (T t a) = cf (T t (change (mchange_Const chg) a))


mchange_FunctionPrototype :: Changer -> MaybeChange FunctionPrototype 
mchange_FunctionPrototype chg fp@FunctionPrototype {..} = 
  if (change_GlobalId chg) fp_fun_name == fp_fun_name then Nothing
  else Just $ fp { fp_fun_name = (change_GlobalId chg) fp_fun_name }

mchange_Toplevel :: Changer -> MaybeChange (Toplevel a)
mchange_Toplevel chg@Changer{..} tpl = case tpl of
  ToplevelNamedMd m -> liftM ToplevelNamedMd (mchange_TlNamedMd chg m)
  ToplevelStandaloneMd sd -> liftM ToplevelStandaloneMd (mchange_TlStandaloneMd chg sd)
  _ -> Nothing

mchange_TlNamedMd :: Changer -> MaybeChange TlNamedMd
mchange_TlNamedMd chg@Changer{..} md = Nothing

mchange_TlStandaloneMd :: Changer -> MaybeChange TlStandaloneMd
mchange_TlStandaloneMd chg@Changer{..} (TlStandaloneMd s mk) = liftM (TlStandaloneMd s) (mchange_MetaKindedConst chg mk)

mchange_MetaKindedConst :: Changer -> MaybeChange MetaKindedConst
mchange_MetaKindedConst chg@Changer{..} mk = case mk of
  MetaKindedConst mk mc -> liftM (MetaKindedConst mk) (mchange_MetaConst chg mc)
  UnmetaKindedNull -> Nothing
  
  
mchange_FunPtr :: Changer -> MaybeChange FunPtr  
mchange_FunPtr chg@Changer{..} fp = case fp of
  FunId g -> let g0 = change_GlobalId g
             in if g0 == g then Nothing
                else Just $ FunId g0
  FunIdBitcast (T st c) dt -> case mchange_Const chg c of
    Nothing -> Nothing
    Just c0 -> Just $ FunIdBitcast (T st c0) dt
  FunIdInttoptr (T st c) dt -> case mchange_Const chg c of
    Nothing -> Nothing
    Just c0 -> Just $ FunIdInttoptr (T st c0) dt
  Fun_null -> Nothing
  Fun_undef -> Nothing


  
mchange_MetaConst :: Changer -> MaybeChange MetaConst  
mchange_MetaConst chg@Changer{..} mc = case mc of
  McStruct l -> let l1 = fmap (change (mchange_MetaKindedConst chg)) l
                in if l1 == l then Nothing
                   else Just $ McStruct l1
  McString s -> Nothing
  McMn n -> Nothing
  McMv s -> Nothing
  McRef s -> Nothing
  McSimple c -> liftM McSimple (mchange_Const chg c)

mchange_Module :: Changer -> MaybeChange (Module a)
mchange_Module chg (Module l) = let l1 = fmap (mchange_Toplevel chg) l
                                in if all isNothing l1 then Nothing
                                   else Just $ Module (fmap (\(x,y) -> fromMaybe y x) (zip l1 l))