{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

#define FLC   (FileLoc $(srcLoc))
{-
  This module compute the use, def, and Addr of CoreIr
-}
module Llvm.Pass.Uda where
import Llvm.Data.CoreIr
import qualified Data.Set as Ds
import Data.Monoid
import Prelude (Show, Eq, Ord, fst, (.), ($), map, maybe, Maybe, (++), show, Bool(False), undefined, foldl,error
               ,fmap)
#ifdef DEBUG
import Debug.Trace
#endif

import Llvm.Query.TypeConstValue
import Llvm.Query.IrCxt
import qualified Llvm.Query.TypeConstValue as Tc

class Uda a where
  use :: a -> Ds.Set GlobalOrLocalId
  def :: a -> Ds.Set LocalId -- defined ssa variables
  
  storeTo :: a -> Ds.Set Value
  storeTo _ = Ds.empty
  
  loadFrom :: a -> Ds.Set Value
  loadFrom _ = Ds.empty 
  
instance Uda Const where
  use c = case c of
    C_globalAddr i -> Ds.insert (GolG i) Ds.empty
    _ -> Ds.empty
  def _ = Ds.empty

instance Uda v => Uda (GetElementPtr s v) where
  use (GetElementPtr _ (T _ ptr) indices) = use ptr `Ds.union` (foldl (\p e -> Ds.union p (use e)) Ds.empty indices)
  def _ = Ds.empty
  
instance Uda v => Uda (Maybe v) where  
  use x = maybe Ds.empty use x
  def x = maybe Ds.empty def x
  storeTo x = maybe Ds.empty storeTo x
  
instance Uda v => Uda (T t v) where  
  use (T _ x) = use x
  def (T _ x) = def x
  storeTo (T _ x) = storeTo x
  
instance Uda LocalId where
  use x = Ds.insert (GolL x) Ds.empty
  def x = Ds.insert x Ds.empty
  storeTo x = Ds.insert (Val_ssa x) Ds.empty
  
instance Uda Value where
  use x = case x of
    Val_ssa l -> Ds.insert (GolL l) Ds.empty
    Val_const c -> use c
  def _ = error $ "cannot happen"
  storeTo x = Ds.insert x Ds.empty
 
instance Uda ActualParam where
  use ap = case ap of
    ActualParamData _ _ _ v _ -> use v
    ActualParamLabel _ _ _ v _ -> use v
    ActualParamMeta _ -> Ds.empty
  def _ = errorLoc FLC $ "cannot happen"
  storeTo _ = errorLoc FLC $ "cannot happen"
  loadFrom _ = errorLoc FLC $ "cannot happen"
  
  
instance Uda x => Uda [x] where  
  use l = Ds.unions (fmap use l)
  def l = Ds.unions (fmap def l)
  storeTo l = Ds.unions (fmap storeTo l)
  loadFrom l = Ds.unions (fmap loadFrom l)

instance Uda CallSite where
  use x = case x of
    CsFun _ _ _ _ l _ -> use l
    CsAsm _ _ _ _ _ _ l _ -> use l
    CsConversion _ _ _ l _ -> use l

instance Uda CInst where 
  use ci = case ci of
    I_alloca{..} -> use size
    I_load{..} -> use pointer
    I_loadatomic{..} -> use pointer
    I_store{..} -> Ds.union (use storedvalue) (use pointer)
    I_storeatomic{..} -> Ds.union (use storedvalue) (use pointer)
    I_fence{..} -> Ds.empty
    I_cmpxchg_I{..} -> Ds.unions [use pointer,use cmpi,use newi]
    I_cmpxchg_F{..} -> Ds.unions [use pointer,use cmpf,use newf]
    I_cmpxchg_P{..} -> Ds.unions [use pointer,use cmpp,use newp]
    I_atomicrmw{..} -> Ds.unions [use pointer,use val]
    I_call_fun{..} -> use actualParams
    I_call_other{..} -> use callSite
    I_extractelement_I{..} -> Ds.unions [use vectorI, use index]
    I_extractelement_F{..} -> Ds.unions [use vectorF, use index]
    I_extractelement_P{..} -> Ds.unions [use vectorP, use index]
    I_bitcast{..} -> use srcP
    I_ptrtoint{..} -> use srcP
    I_inttoptr{..} -> use srcI
    I_add{..} -> use operand1 `mappend` use operand2
    I_getelementptr{..} -> use pointer `mappend` (foldl (\p e -> p `mappend` (use e)) Ds.empty indices)
    I_getelementptr_V{..} -> use vpointer `mappend` (foldl (\p e -> p `mappend` (use e)) Ds.empty vindices)
    I_llvm_dbg_declare{..} -> Ds.empty
    I_icmp{..} -> use operand1 `mappend` use operand2
    I_icmp_V{..} -> use operand1 `mappend` use operand2
    I_va_end{..} -> use pointer
    I_va_start{..} -> use pointer
    I_va_arg tv _ _ -> use tv
    _ -> errorLoc FLC $ "unsupported " ++ show ci
  def ci = case ci of
    I_alloca{..} -> def result
    I_load{..} -> def result
    I_store{..} -> Ds.empty
    I_bitcast{..} -> def result
    I_ptrtoint{..} -> def result
    I_inttoptr{..} -> def result
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
    I_getelementptr{..} -> def result
    I_llvm_dbg_declare{..} -> Ds.empty
    I_icmp{..} -> def result
    I_icmp_V{..} -> def result
    I_trunc{..} -> def result
    I_sext{..} -> def result
    I_va_start{..} -> Ds.empty
    I_va_arg{..} -> def result
    I_va_end{..} -> Ds.empty
    _ -> errorLoc FLC $ "unsupported " ++ show ci    
  storeTo ci = case ci of
    I_store{..} -> storeTo pointer
    I_va_start tv -> storeTo tv
    _ -> Ds.empty
  loadFrom ci = case ci of
    I_load{..} -> let T _ v = pointer
                  in Ds.insert v Ds.empty
    _ -> Ds.empty


instance Uda CInstWithDbg where  
  use (CInstWithDbg ci _) = use ci
  def (CInstWithDbg ci _) = def ci
  storeTo (CInstWithDbg ci _) = storeTo ci
  loadFrom (CInstWithDbg ci _) = loadFrom ci
  
  
                              