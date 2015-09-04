{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell,FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

#define FLC   (FileLoc $(srcLoc))
{-
  This module compute the use, def, and Addr of CoreIr
-}
module Llvm.Query.Uda where
import Llvm.Hir.Data.Inst
import qualified Data.Set as S
import Data.Monoid
import Prelude (Show, Eq, Ord, fst, (.), ($), map, maybe, Maybe, (++), show, Bool(False), undefined, foldl,error
               ,fmap)
#ifdef DEBUG
import Debug.Trace
#endif
import Llvm.ErrorLoc
import Data.Either


class Uda a g where
  use :: a -> S.Set (Either Lname g)
  def :: g {- a hack to force restricting the type parameter g -} -> a -> S.Set Lname -- defined ssa variables
  
  storeTo :: a -> S.Set (Value g)
  storeTo _ = S.empty
  
  loadFrom :: a -> S.Set (Value g)
  loadFrom _ = S.empty 
  
instance Ord g => Uda (Const g) g where
  use c = case c of
    C_globalAddr i -> S.insert (Right i) S.empty
    _ -> S.empty
  def _ _ = S.empty

instance (Ord g, Uda v g,  Uda idx g) => Uda (GetElementPtr s v idx) g where
  use (GetElementPtr _ (T _ ptr) indices) = use ptr `S.union` (foldl (\p e -> S.union p (use e)) S.empty indices)
  def _ _ = S.empty
  
instance (Ord g, Uda v g) => Uda (Maybe v) g where  
  use x = maybe S.empty use x
  def g x  = maybe S.empty (def g) x
  storeTo x = maybe S.empty storeTo x
  
instance Uda v g => Uda (T t v) g where
  use (T _ x) = use x
  def g (T _ x) = def g x
  storeTo (T _ x) = storeTo x
  
instance Ord g => Uda Lname g where
  use x = S.insert (Left x) S.empty
  def g x = S.insert x S.empty
  storeTo x = S.insert (Val_ssa x) S.empty
  loadFrom x = S.insert (Val_ssa x) S.empty
  
instance Ord g => Uda (Value g) g where
  use x = case x of
    Val_ssa l -> S.insert (Left l) S.empty
    Val_const c -> use c
  def _ = error $ "cannot happen"
  storeTo x = S.insert x S.empty
  loadFrom x = S.insert x S.empty
 
  
instance (Ord g, Uda x g) => Uda [x] g where  
  use l = S.unions (fmap use l)
  def g l = S.unions (fmap (def g) l)
  storeTo l = S.unions (fmap storeTo l)
  loadFrom l = S.unions (fmap loadFrom l)
  
instance Uda a g => Uda (FunOperand a) g where  
  use x = case x of
    FunOperandAsRet _ _ _ a -> use a    
    FunOperandData _ _ _ a -> use a
    FunOperandExt _  _ _ _ a -> use a    
    FunOperandByVal _ _ _ a -> use a
    FunOperandLabel _ _ _ l -> use l
  storeTo x = case x of  
    FunOperandAsRet _ _ _ a -> storeTo a
    _ -> S.empty

instance (Ord g, Uda a g) => Uda (FunSignature a) g where
  use FunSignature{..} = use fs_params
  def g FunSignature{..} = S.empty
  storeTo FunSignature{..} = storeTo fs_params
  loadFrom FunSignature{..} = S.empty

instance Ord g => Uda (CallFunInterface g) g where
  use CallFunInterface{..} = use cfi_signature 
  def _ _ = S.empty

instance Ord g => Uda (InvokeFunInterface g) g where
  use InvokeFunInterface{..} = use ifi_signature
  def _ _ = S.empty  

instance Ord g => Uda (CallAsmInterface g) g where
  use CallAsmInterface{..} = use cai_actualParams
  def _ _ = S.empty


instance (Ord g, Show g) => Uda (Cinst g) g where 
  use ci = case ci of
    I_alloca{..} -> use size
    I_load{..} -> use pointer
    I_loadatomic{..} -> use pointer
    I_store{..} -> S.union (use storedvalue) (use pointer)
    I_storeatomic{..} -> S.union (use storedvalue) (use pointer)
    I_fence{..} -> S.empty
    I_cmpxchg_I{..} -> S.unions [use pointer,use cmpi,use newi]
    I_cmpxchg_F{..} -> S.unions [use pointer,use cmpf,use newf]
    I_cmpxchg_P{..} -> S.unions [use pointer,use cmpp,use newp]
    I_atomicrmw{..} -> S.unions [use pointer,use val]
    I_call_fun{..} -> use call_fun_interface
    I_call_asm{..} -> use call_asm_interface
    I_extractelement_I{..} -> S.unions [use vectorI, use index]
    I_extractelement_F{..} -> S.unions [use vectorF, use index]
    I_extractelement_P{..} -> S.unions [use vectorP, use index]
    I_insertelement_I{..} -> S.unions [use vectorI, use elementI, use index]
    I_insertelement_F{..} -> S.unions [use vectorF, use elementF, use index]    
    I_insertelement_P{..} -> S.unions [use vectorP, use elementP, use index]
    I_shufflevector_I{..} -> S.unions [use vector1I, use vector2I, use vectorIdx]
    I_shufflevector_F{..} -> S.unions [use vector1F, use vector2F, use vectorIdx]
    I_shufflevector_P{..} -> S.unions [use vector1P, use vector2P, use vectorIdx]
    I_extractvalue{..} -> S.unions [use record]
    I_insertvalue{..} -> S.unions [use record, use element]  
    I_va_arg{..} -> use dv
    I_llvm_va_start{..} -> use arglist
    I_llvm_va_end{..} -> use arglist
    I_landingpad{..} -> errorLoc FLC $ "undefined yet"
    I_getelementptr{..} -> use pointer `mappend` (foldl (\p e -> p `mappend` (use e)) S.empty indices)
    I_getelementptr_V{..} -> use vpointer `mappend` (foldl (\p e -> p `mappend` (use e)) S.empty vindices)
    I_icmp{..} -> S.unions [use operand1, use operand1]
    I_icmp_V{..} -> S.unions [use operand1, use operand1]
    I_fcmp{..} -> S.unions [use operand1, use operand1]
    I_fcmp_V{..} -> S.unions [use operand1, use operand1]
    I_add{..} -> S.unions [use operand1, use operand2]
    I_sub{..} -> S.unions [use operand1, use operand2]    
    I_mul{..} -> S.unions [use operand1, use operand2]    
    I_udiv{..} -> S.unions [use operand1, use operand2]    
    I_sdiv{..} -> S.unions [use operand1, use operand2]    
    I_urem{..} -> S.unions [use operand1, use operand2]
    I_srem{..} -> S.unions [use operand1, use operand2]
    I_shl{..} -> S.unions [use operand1, use operand2]
    I_lshr{..} -> S.unions [use operand1, use operand2]
    I_ashr{..} -> S.unions [use operand1, use operand2]
    I_and{..} -> S.unions [use operand1, use operand2]    
    I_or{..} -> S.unions [use operand1, use operand2]        
    I_xor{..} -> S.unions [use operand1, use operand2]    
    
    I_add_V{..} -> S.unions [use operand1, use operand2]
    I_sub_V{..} -> S.unions [use operand1, use operand2]    
    I_mul_V{..} -> S.unions [use operand1, use operand2]    
    I_udiv_V{..} -> S.unions [use operand1, use operand2]    
    I_sdiv_V{..} -> S.unions [use operand1, use operand2]    
    I_urem_V{..} -> S.unions [use operand1, use operand2]
    I_srem_V{..} -> S.unions [use operand1, use operand2]
    I_shl_V{..} -> S.unions [use operand1, use operand2]
    I_lshr_V{..} -> S.unions [use operand1, use operand2]
    I_ashr_V{..} -> S.unions [use operand1, use operand2]
    I_and_V{..} -> S.unions [use operand1, use operand2]    
    I_or_V{..} -> S.unions [use operand1, use operand2]        
    I_xor_V{..} -> S.unions [use operand1, use operand2]    
    
    I_fadd{..} -> S.unions [use operand1, use operand2]    
    I_fsub{..} -> S.unions [use operand1, use operand2]        
    I_fmul{..} -> S.unions [use operand1, use operand2]        
    I_fdiv{..} -> S.unions [use operand1, use operand2]        
    I_frem{..} -> S.unions [use operand1, use operand2]    
    
    I_fadd_V{..} -> S.unions [use operand1, use operand2]    
    I_fsub_V{..} -> S.unions [use operand1, use operand2]        
    I_fmul_V{..} -> S.unions [use operand1, use operand2]        
    I_fdiv_V{..} -> S.unions [use operand1, use operand2]        
    I_frem_V{..} -> S.unions [use operand1, use operand2] 
    
    I_trunc{..} -> use srcI
    I_zext{..} -> use srcI
    I_sext{..} -> use srcI
    I_fptrunc{..} -> use srcF              
    I_fpext{..} -> use srcF
    I_fptoui{..} -> use srcF
    I_fptosi{..} -> use srcF
    I_uitofp{..} -> use srcI
    I_ptrtoint{..} -> use srcP
    I_inttoptr{..} -> use srcI
    I_addrspacecast{..} -> use srcP
    
    I_trunc_V{..} -> use srcVI
    I_zext_V{..} -> use srcVI
    I_sext_V{..} -> use srcVI
    I_fptrunc_V{..} -> use srcVF              
    I_fpext_V{..} -> use srcVF
    I_fptoui_V{..} -> use srcVF
    I_fptosi_V{..} -> use srcVF
    I_uitofp_V{..} -> use srcVI
    I_ptrtoint_V{..} -> use srcVP
    I_inttoptr_V{..} -> use srcVI
    I_addrspacecast_V{..} -> use srcVP

    I_bitcast{..} -> use srcP
    I_bitcast_D{..} -> use srcD
    
    I_select_I{..} -> S.unions [use trueI, use falseI]
    I_select_F{..} -> S.unions [use trueF, use falseF]
    I_select_P{..} -> S.unions [use trueP, use falseP]
    
    I_select_VI{..} -> S.unions [use trueVI, use falseVI]
    I_select_VF{..} -> S.unions [use trueVF, use falseVF]
    I_select_VP{..} -> S.unions [use trueVP, use falseVP]
    I_select_First{..} -> S.unions [use trueFirst, use falseFirst]
    I_llvm_memcpy{..} -> S.unions [use dest, use src, use len, use align]
    I_llvm_memmove{..} -> S.unions [use dest, use src, use len, use align]
    _ -> errorLoc FLC $ "unsupported " ++ show ci
    
  def g ci = case ci of
    I_alloca{..} -> def g result
    I_load{..} -> def g result
    I_loadatomic{..} -> def g result
    I_store{..} -> S.empty
    I_storeatomic{..} -> S.empty
    I_fence{..} -> S.empty
    I_cmpxchg_I{..} -> def g result
    I_cmpxchg_F{..} -> def g result    
    I_cmpxchg_P{..} -> def g result
    I_atomicrmw{..} -> S.empty
    I_call_fun{..} -> def g call_return
    I_call_asm{..} -> def g call_return
    I_extractelement_I{..} -> def g result
    I_extractelement_F{..} -> def g result    
    I_extractelement_P{..} -> def g result
    I_insertelement_I{..} -> def g result
    I_insertelement_F{..} -> def g result    
    I_insertelement_P{..} -> def g result
    I_shufflevector_I{..} -> def g result
    I_shufflevector_F{..} -> def g result    
    I_shufflevector_P{..} -> def g result    
    I_extractvalue{..} -> def g result
    I_insertvalue{..} -> def g result                          
    I_va_arg{..} -> def g result    
    I_llvm_va_start{..} -> S.empty
    I_llvm_va_end{..} -> S.empty
    I_landingpad _ _ _ _ _ r -> def g r
    I_getelementptr{..} -> def g result    
    I_getelementptr_V{..} -> def g result    
    I_icmp{..} -> def g result
    I_icmp_V{..} -> def g result
    I_fcmp{..} -> def g result
    I_fcmp_V{..} -> def g result

    I_add{..} -> def g result
    I_sub{..} -> def g result
    I_mul{..} -> def g result
    I_udiv{..} -> def g result
    I_sdiv{..} -> def g result
    I_urem{..} -> def g result
    I_srem{..} -> def g result
    I_shl{..} -> def g result
    I_lshr{..} -> def g result
    I_ashr{..} -> def g result    
    I_and{..} -> def g result    
    I_or{..} -> def g result        
    I_xor{..} -> def g result 

    I_add_V{..} -> def g result
    I_sub_V{..} -> def g result
    I_mul_V{..} -> def g result
    I_udiv_V{..} -> def g result
    I_sdiv_V{..} -> def g result
    I_urem_V{..} -> def g result
    I_srem_V{..} -> def g result
    I_shl_V{..} -> def g result
    I_lshr_V{..} -> def g result
    I_ashr_V{..} -> def g result    
    I_and_V{..} -> def g result    
    I_or_V{..} -> def g result        
    I_xor_V{..} -> def g result 
    
    I_fadd{..} -> def g result
    I_fsub{..} -> def g result    
    I_fmul{..} -> def g result
    I_fdiv{..} -> def g result    
    I_frem{..} -> def g result        

    I_fadd_V{..} -> def g result
    I_fsub_V{..} -> def g result    
    I_fmul_V{..} -> def g result
    I_fdiv_V{..} -> def g result    
    I_frem_V{..} -> def g result        


    I_trunc{..} -> def g result
    I_zext{..} -> def g result
    I_sext{..} -> def g result
    I_fptrunc{..} -> def g result
    I_fpext{..} -> def g result
    I_fptoui{..} -> def g result
    I_fptosi{..} -> def g result
    I_uitofp{..} -> def g result
    I_ptrtoint{..} -> def g result
    I_inttoptr{..} -> def g result
    I_addrspacecast{..} -> def g result
    
    I_trunc_V{..} -> def g result
    I_zext_V{..} -> def g result
    I_sext_V{..} -> def g result
    I_fptrunc_V{..} -> def g result              
    I_fpext_V{..} -> def g result
    I_fptoui_V{..} -> def g result
    I_fptosi_V{..} -> def g result
    I_uitofp_V{..} -> def g result
    I_ptrtoint_V{..} -> def g result
    I_inttoptr_V{..} -> def g result
    I_addrspacecast_V{..} -> def g result

    I_bitcast{..} -> def g result
    I_bitcast_D{..} -> def g result
    
    I_select_I{..} -> def g result
    I_select_F{..} -> def g result
    I_select_P{..} -> def g result
    
    I_select_VI{..} -> def g result
    I_select_VF{..} -> def g result
    I_select_VP{..} -> def g result
    I_select_First{..} -> def g result
    
    I_llvm_memcpy{..} -> S.empty
    I_llvm_memmove{..} -> S.empty
    _ -> errorLoc FLC $ "unsupported " ++ show ci    
  storeTo ci = case ci of
    I_store{..} -> storeTo pointer
    I_storeatomic{..} -> storeTo pointer
    I_atomicrmw{..} -> storeTo pointer
    I_llvm_va_start tv -> storeTo tv
    I_llvm_va_end tv -> storeTo tv
    I_llvm_memcpy{..} -> storeTo dest
    I_llvm_memmove{..} -> storeTo dest    
    _ -> S.empty
  loadFrom ci = case ci of
    I_load{..} -> loadFrom pointer
    I_loadatomic{..} -> loadFrom pointer
    I_atomicrmw{..} -> loadFrom pointer
    I_llvm_memcpy{..} -> loadFrom src
    I_llvm_memmove{..} -> loadFrom src    
    _ -> S.empty