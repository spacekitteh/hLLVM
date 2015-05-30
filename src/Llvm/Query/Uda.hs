{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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


class Uda a where
  use :: a -> S.Set GlobalOrLocalId
  def :: a -> S.Set LocalId -- defined ssa variables
  
  storeTo :: a -> S.Set Value
  storeTo _ = S.empty
  
  loadFrom :: a -> S.Set Value
  loadFrom _ = S.empty 
  
instance Uda Const where
  use c = case c of
    C_globalAddr i -> S.insert (GolG i) S.empty
    _ -> S.empty
  def _ = S.empty

instance Uda v => Uda (GetElementPtr s v) where
  use (GetElementPtr _ (T _ ptr) indices) = use ptr `S.union` (foldl (\p e -> S.union p (use e)) S.empty indices)
  def _ = S.empty
  
instance Uda v => Uda (Maybe v) where  
  use x = maybe S.empty use x
  def x = maybe S.empty def x
  storeTo x = maybe S.empty storeTo x
  
instance Uda v => Uda (T t v) where  
  use (T _ x) = use x
  def (T _ x) = def x
  storeTo (T _ x) = storeTo x
  
instance Uda LocalId where
  use x = S.insert (GolL x) S.empty
  def x = S.insert x S.empty
  storeTo x = S.insert (Val_ssa x) S.empty
  loadFrom x = S.insert (Val_ssa x) S.empty
  
instance Uda Value where
  use x = case x of
    Val_ssa l -> S.insert (GolL l) S.empty
    Val_const c -> use c
  def _ = error $ "cannot happen"
  storeTo x = S.insert x S.empty
  loadFrom x = S.insert x S.empty
 
instance Uda CallOperand where
  use ap = case ap of
    CallOperandData _ _ _ v -> use v
    CallOperandByVal _ _ _ v -> use v
    CallOperandLabel _ _ _ _ -> S.empty 
  def _ = errorLoc FLC $ "cannot happen"
  storeTo _ = errorLoc FLC $ "cannot happen"
  loadFrom _ = errorLoc FLC $ "cannot happen"
  
  
instance Uda x => Uda [x] where  
  use l = S.unions (fmap use l)
  def l = S.unions (fmap def l)
  storeTo l = S.unions (fmap storeTo l)
  loadFrom l = S.unions (fmap loadFrom l)


instance Uda CallFunInterface where
  use CallFunInterface{..} = use cfi_actualParams
  def _ = S.empty

instance Uda InvokeFunInterface where
  use InvokeFunInterface{..} = use ifi_actualParams
  def _ = S.empty  

instance Uda CallAsmInterface where
  use CallAsmInterface{..} = use cai_actualParams
  def _ = S.empty


instance Uda Cinst where 
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
    
  def ci = case ci of
    I_alloca{..} -> def result
    I_load{..} -> def result
    I_loadatomic{..} -> def result
    I_store{..} -> S.empty
    I_storeatomic{..} -> S.empty
    I_fence{..} -> S.empty
    I_cmpxchg_I{..} -> def result
    I_cmpxchg_F{..} -> def result    
    I_cmpxchg_P{..} -> def result
    I_atomicrmw{..} -> S.empty
    I_call_fun{..} -> def call_return
    I_call_asm{..} -> def call_return
    I_extractelement_I{..} -> def result
    I_extractelement_F{..} -> def result    
    I_extractelement_P{..} -> def result
    I_insertelement_I{..} -> def result
    I_insertelement_F{..} -> def result    
    I_insertelement_P{..} -> def result
    I_shufflevector_I{..} -> def result
    I_shufflevector_F{..} -> def result    
    I_shufflevector_P{..} -> def result    
    I_extractvalue{..} -> def result
    I_insertvalue{..} -> def result                          
    I_va_arg{..} -> def result    
    I_llvm_va_start{..} -> S.empty
    I_llvm_va_end{..} -> S.empty
    I_landingpad _ _ _ _ _ r -> def r
    I_getelementptr{..} -> def result    
    I_getelementptr_V{..} -> def result    
    I_icmp{..} -> def result
    I_icmp_V{..} -> def result
    I_fcmp{..} -> def result
    I_fcmp_V{..} -> def result

    I_add{..} -> def result
    I_sub{..} -> def result
    I_mul{..} -> def result
    I_udiv{..} -> def result
    I_sdiv{..} -> def result
    I_urem{..} -> def result
    I_srem{..} -> def result
    I_shl{..} -> def result
    I_lshr{..} -> def result
    I_ashr{..} -> def result    
    I_and{..} -> def result    
    I_or{..} -> def result        
    I_xor{..} -> def result 

    I_add_V{..} -> def result
    I_sub_V{..} -> def result
    I_mul_V{..} -> def result
    I_udiv_V{..} -> def result
    I_sdiv_V{..} -> def result
    I_urem_V{..} -> def result
    I_srem_V{..} -> def result
    I_shl_V{..} -> def result
    I_lshr_V{..} -> def result
    I_ashr_V{..} -> def result    
    I_and_V{..} -> def result    
    I_or_V{..} -> def result        
    I_xor_V{..} -> def result 
    
    I_fadd{..} -> def result
    I_fsub{..} -> def result    
    I_fmul{..} -> def result
    I_fdiv{..} -> def result    
    I_frem{..} -> def result        

    I_fadd_V{..} -> def result
    I_fsub_V{..} -> def result    
    I_fmul_V{..} -> def result
    I_fdiv_V{..} -> def result    
    I_frem_V{..} -> def result        


    I_trunc{..} -> def result
    I_zext{..} -> def result
    I_sext{..} -> def result
    I_fptrunc{..} -> def result
    I_fpext{..} -> def result
    I_fptoui{..} -> def result
    I_fptosi{..} -> def result
    I_uitofp{..} -> def result
    I_ptrtoint{..} -> def result
    I_inttoptr{..} -> def result
    I_addrspacecast{..} -> def result
    
    I_trunc_V{..} -> def result
    I_zext_V{..} -> def result
    I_sext_V{..} -> def result
    I_fptrunc_V{..} -> def result              
    I_fpext_V{..} -> def result
    I_fptoui_V{..} -> def result
    I_fptosi_V{..} -> def result
    I_uitofp_V{..} -> def result
    I_ptrtoint_V{..} -> def result
    I_inttoptr_V{..} -> def result
    I_addrspacecast_V{..} -> def result

    I_bitcast{..} -> def result
    I_bitcast_D{..} -> def result
    
    I_select_I{..} -> def result
    I_select_F{..} -> def result
    I_select_P{..} -> def result
    
    I_select_VI{..} -> def result
    I_select_VF{..} -> def result
    I_select_VP{..} -> def result
    I_select_First{..} -> def result
    
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