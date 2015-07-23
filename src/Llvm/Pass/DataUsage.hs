{-# LANGUAGE CPP, ScopedTypeVariables, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses #-}

module Llvm.Pass.DataUsage where
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as Dm
import Llvm.Query.HirCxt

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Hir.Data
import Llvm.Hir.Print
import Llvm.ErrorLoc
import Llvm.Hir.Mangle

#define FLC (FileLoc $(srcLoc))
{-|

This is a backward analysis that collects how data/ssa variables are
used, including but not limited to how pointers are passed around.
The information is needed for many analyses, so I generalize it as a
pass. The field names in DataUsage should be stable such that the
consumers of this pass won't be broken when we extend this pass to
collect different data/ssa variable usage aspects. Because of this
backward compatibility requirement, I use a very elaborate and
descriptive naming conventions:

[stack_]addrs[_storing_<what>|_captured|passed_to_va_start]

-}

data DataUsage g = DataUsage { -- | The addresses that store pointer parameters
                             addrs_storing_ptr_params :: S.Set (Value g)
                             -- | The local addresses that store pointer parameters
                           , stack_addrs_storing_ptr_params :: S.Set LocalId
                             -- | An address is captured if it's stored on stack or heap
                           , addrs_captured :: S.Set (Value g)
                             -- | An address of a local variable is stored on stack or heap
                           , stack_addrs_captured :: S.Set LocalId
                             -- | The stack or heap addresses that store another addresses
                           , addrs_storing_ptrs :: S.Set (Value g)
                             -- | The addresses of local variables that stores another addresses
                           , stack_addrs_storing_ptrs :: S.Set LocalId
                             -- | The addresses are passed to va_start
                           , addrs_passed_to_va_start :: S.Set (Value g)
                             -- | The addresses of a local variables that are passed to va_start
                           , stack_addrs_passed_to_va_start :: S.Set LocalId
                             -- | The addresses of all stores
                           , addrs_storing_values :: S.Set (Value g)
                             -- | The addresses of local variable that store values
                           , stack_addrs_storing_values :: S.Set LocalId
                             -- | The addresses that are involved in pointer arithmatic
                           , addrs_involving_pointer_arithmatic :: S.Set (Value g)
                             -- | The stack addresses that are involved in pointer arithmatic
                           , stack_addrs_involving_pointer_arithmatic :: S.Set LocalId
                             -- | The constants used in a function
                           , constants :: S.Set (Const g)
                             -- | The call function info set
                           , callFunInfoSet :: S.Set (FunPtr g, CallFunInterface g)
                           , invokeFunInfoSet :: S.Set (FunPtr g, InvokeFunInterface g)
                           , callAsmInfoSet :: S.Set (CallAsmInterface g)
                           } deriving (Eq, Ord, Show)


class DataUsageUpdator g a where
  update :: a -> DataUsage g -> DataUsage g
  
instance DataUsageUpdator Gname () where
  update _ = id

addAddrStoringPtrParam :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrStoringPtrParam v du@DataUsage{..} =
  du { addrs_storing_ptr_params = S.insert v addrs_storing_ptr_params }

addStackAddrStoringPtrParam :: LocalId -> DataUsage g -> DataUsage g
addStackAddrStoringPtrParam v du@DataUsage{..} =
  du { stack_addrs_storing_ptr_params = S.insert v stack_addrs_storing_ptr_params }

addAddrCaptured :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrCaptured v du@DataUsage{..} =
  du { addrs_captured = S.insert v addrs_captured }

addStackAddrCaptured :: LocalId -> DataUsage g -> DataUsage g
addStackAddrCaptured v du@DataUsage{..} =
  du { stack_addrs_captured = S.insert v stack_addrs_captured }

addAddrStoringPtr :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrStoringPtr v du@DataUsage{..} =
  du { addrs_storing_ptrs = S.insert v addrs_storing_ptrs }

addStackAddrStoringPtr :: LocalId -> DataUsage g -> DataUsage g
addStackAddrStoringPtr v du@DataUsage{..} =
  du { stack_addrs_storing_ptrs = S.insert v stack_addrs_storing_ptrs }

addAddrPassedToVaStart :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrPassedToVaStart v du@DataUsage{..} =
  du { addrs_passed_to_va_start = S.insert v addrs_passed_to_va_start }

addStackAddrPassedToVaStart :: LocalId -> DataUsage g -> DataUsage g
addStackAddrPassedToVaStart v du@DataUsage{..} =
  du { stack_addrs_passed_to_va_start = S.insert v stack_addrs_passed_to_va_start }

addAddrStoringValue :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrStoringValue v du@DataUsage{..} =
  du { addrs_storing_values = S.insert v addrs_storing_values }

addStackAddrStoringValue :: LocalId -> DataUsage g -> DataUsage g
addStackAddrStoringValue v du@DataUsage{..} =
  du { stack_addrs_storing_values = S.insert v stack_addrs_storing_values }

addAddrInvolvingPtrArithm :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrInvolvingPtrArithm v du@DataUsage{..} =
  du { addrs_involving_pointer_arithmatic = S.insert v addrs_involving_pointer_arithmatic }

addStackAddrInvolvingPtrArithm :: LocalId -> DataUsage g -> DataUsage g
addStackAddrInvolvingPtrArithm v du@DataUsage{..} =
  du { stack_addrs_involving_pointer_arithmatic = S.insert v stack_addrs_involving_pointer_arithmatic }

addConst :: Ord g => Value g -> DataUsage g -> DataUsage g
addConst (Val_const c) du@DataUsage{..} = du { constants = S.insert c constants }
addConst (Val_ssa _) du = du

addMaybeConst :: Ord g => Maybe (Value g) -> DataUsage g -> DataUsage g
addMaybeConst (Just c) du = addConst c du
addMaybeConst Nothing du = du


aTv :: (Value g -> DataUsage g -> DataUsage g) -> T t (Value g) -> DataUsage g -> DataUsage g
aTv  f (T _ v) du = f v du

applyToMaybe :: (a -> DataUsage g -> DataUsage g) -> Maybe a -> DataUsage g -> DataUsage g
applyToMaybe f (Just x) du = f x du
applyToMaybe f Nothing du = du

bubbleUp :: Ord x => x -> x -> S.Set x -> S.Set x
bubbleUp dest src s = if S.member dest s then S.insert src s else s

filterAlloca :: Ord g => LocalId -> (DataUsage g -> S.Set (Value g)) 
                -> (LocalId -> DataUsage g -> DataUsage g) -> DataUsage g -> DataUsage g
filterAlloca ssa src addf du = if S.member (Val_ssa ssa) (src du) then addf ssa du else du

bubbleUp2 :: Ord g => Value g -> (DataUsage g -> S.Set (Value g)) 
             -> Value g -> (Value g -> DataUsage g -> DataUsage g) -> DataUsage g -> DataUsage g
bubbleUp2 dest setf src addf du = if S.member dest (setf du) then addf src du else du

propogateUpPtrUsage :: Ord g => LocalId -> Value g -> DataUsage g -> DataUsage g
propogateUpPtrUsage dest src du =
  bubbleUp2 (Val_ssa dest) addrs_storing_ptr_params src addAddrStoringPtrParam
  $ bubbleUp2 (Val_ssa dest) addrs_captured src addAddrCaptured
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
  $ bubbleUp2 (Val_ssa dest) addrs_passed_to_va_start src addAddrPassedToVaStart
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
  $ bubbleUp2 (Val_ssa dest) addrs_storing_values src addAddrStoringValue
  $ bubbleUp2 (Val_ssa dest) addrs_involving_pointer_arithmatic src addAddrInvolvingPtrArithm
  du

emptyDataUsage :: DataUsage g
emptyDataUsage =
  DataUsage 
  S.empty S.empty S.empty S.empty S.empty
  S.empty S.empty S.empty S.empty S.empty
  S.empty S.empty S.empty S.empty S.empty 
  S.empty 

instance (IrPrint t1, IrPrint t2, IrPrint t3) => IrPrint (t1, t2, t3) where
  printIr (t1, t2, t3) = parens (printIr t1 <+> printIr t2 <+> printIr t3)

instance IrPrint g => IrPrint (DataUsage g) where
  printIr DataUsage{..} =
    text "addrs_storing_ptr_params:" <+> printIr addrs_storing_ptr_params
    $+$ text "stack_addrs_storing_ptr_params:" <+> printIr stack_addrs_storing_ptr_params
    $+$ text "addrs_captured:" <+> printIr addrs_captured
    $+$ text "stack_addrs_captured:" <+> printIr stack_addrs_captured
    $+$ text "addrs_storing_ptrs:" <+> printIr addrs_storing_ptrs
    $+$ text "stack_addrs_storing_ptrs:" <+> printIr stack_addrs_storing_ptrs
    $+$ text "addrs_passed_to_va_start:" <+> printIr addrs_passed_to_va_start
    $+$ text "stack_addrs_passed_to_va_start:" <+> printIr stack_addrs_passed_to_va_start
    $+$ text "addrs_storing_values:" <+> printIr addrs_storing_values
    $+$ text "stack_addrs_storing_values:" <+> printIr stack_addrs_storing_values
    $+$ text "addrs_involving_pointer_arithmatic:" <+> printIr addrs_involving_pointer_arithmatic
    $+$ text "stack_addrs_involving_pointer_arithmatic:" <+> printIr stack_addrs_involving_pointer_arithmatic
    $+$ text "constants:" <+> printIr constants
    $+$ text "callInfoSet:" <+> printIr callFunInfoSet
    $+$ text "callAsmSet:" <+> printIr callAsmInfoSet

usageLattice :: Ord g => H.DataflowLattice (DataUsage g)
usageLattice = H.DataflowLattice
              { H.fact_name = "Data and Ssa variable Usage"
              , H.fact_bot = emptyDataUsage
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
            where
              j = unionDataUsage old new
              ch = H.changeIf (j /= old)

unionDataUsage :: Ord g => DataUsage g -> DataUsage g -> DataUsage g
unionDataUsage (DataUsage s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16)
  (DataUsage t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16)
  = DataUsage
    (s1 `S.union` t1)   (s2 `S.union` t2)   (s3 `S.union` t3)
    (s4 `S.union` t4)   (s5 `S.union` t5)   (s6 `S.union` t6)
    (s7 `S.union` t7)   (s8 `S.union` t8)   (s9 `S.union` t9)
    (s10 `S.union` t10) (s11 `S.union` t11) (s12 `S.union` t12)
    (s13 `S.union` t13) (s14 `S.union` t14) (s15 `S.union` t15)
    (s16 `S.union` t16)

bwdScan :: forall g.forall a.forall m. (Show g, Ord g, Show a, DataUsageUpdator g a, H.FuelMonad m) => 
           S.Set LocalId -> H.BwdPass m (Node g a) (DataUsage g)
bwdScan formalParams = H.BwdPass { H.bp_lattice = usageLattice
                                 , H.bp_transfer = H.mkBTransfer (bwdTran formalParams)
                                 , H.bp_rewrite = H.noBwdRewrite
                                 }
  where
    bwdTran :: (Show g, Ord g) => S.Set LocalId -> Node g a e x -> H.Fact x (DataUsage g) -> DataUsage g
    bwdTran fp n f = case n of
      Tnode tinst _ ->
        let f0 = foldl (\p l -> p `unionDataUsage` (fromMaybe emptyDataUsage $ H.lookupFact l f)) emptyDataUsage (H.successors n)
        in case tinst of
          T_ret_void -> f0
          T_return vs -> foldl (flip (aTv addConst)) f0 vs
          T_invoke{..} ->
            let vals = getValuesFromParams (fs_params $ ifi_signature invoke_fun_interface)
            in foldl (\p e -> addConst e $ addAddrCaptured e $ addAddrStoringPtr e
                              $ addAddrStoringValue e p
                     ) (f0 { invokeFunInfoSet = S.insert (invoke_ptr, invoke_fun_interface) (invokeFunInfoSet f0) }) 
               (S.toList vals)
          T_invoke_asm{..} ->
            let vals = getValuesFromParams (cai_actualParams invoke_asm_interface)
            in foldl (\p e -> addConst e $ addAddrCaptured e $ addAddrStoringPtr e
                              $ addAddrStoringValue e p
                     ) (f0 { callAsmInfoSet = S.insert invoke_asm_interface (callAsmInfoSet f0)}) (S.toList vals)
          _ -> f0
      Lnode _ -> f
      Pnode (Pinst{..}) _ -> foldl (\p (e, _) -> propogateUpPtrUsage flowout e p) f flowins
      Enode x _ -> update x f
      Comment _ -> f
      Cnode comp _ -> case comp of
        I_alloca{..} ->
          applyToMaybe (aTv addConst) size
          $ filterAlloca result addrs_storing_ptr_params addStackAddrStoringPtrParam
          $ filterAlloca result addrs_storing_ptrs addStackAddrStoringPtr
          $ filterAlloca result addrs_storing_values addStackAddrStoringValue
          $ filterAlloca result addrs_captured addStackAddrCaptured
          $ filterAlloca result addrs_passed_to_va_start addStackAddrPassedToVaStart
          $ filterAlloca result addrs_involving_pointer_arithmatic addStackAddrInvolvingPtrArithm f
        I_load{..} ->
          aTv addConst pointer
          $ aTv addAddrStoringValue pointer f
        I_loadatomic{..} ->
          aTv addConst pointer
          $ aTv addAddrStoringValue pointer f
        I_store{..} ->
          aTv addConst storedvalue
          $ aTv addConst pointer
          $ aTv addAddrStoringValue pointer
          $ aTv addAddrStoringValue pointer $ case storedvalue of
            T (DtypeScalarP _) sv ->
              case sv of
                Val_ssa v | S.member v fp -> aTv addAddrStoringPtrParam pointer $ addAddrCaptured sv f
                _ -> aTv addAddrStoringPtr pointer $ addAddrCaptured sv f
            _ -> f
        I_storeatomic{..} ->
          aTv addConst storedvalue
          $ aTv addConst pointer
          $ aTv addAddrStoringValue pointer
          $ aTv addAddrStoringValue pointer $ case storedvalue of
            T (DtypeScalarP _) sv ->
              case sv of
                Val_ssa v | S.member v fp -> aTv addAddrStoringPtrParam pointer $ addAddrCaptured sv f
                _ -> aTv addAddrStoringPtr pointer $ addAddrCaptured sv f
            _ -> f
        I_fence{..} -> f
        I_cmpxchg_I{..} -> aTv addConst cmpi
                           $ aTv addConst newi
                           $ aTv (propogateUpPtrUsage result) cmpi
                           $ aTv (propogateUpPtrUsage result) newi f
        I_cmpxchg_F{..} -> aTv addConst cmpf
                           $ aTv addConst newf f
        I_cmpxchg_P{..} -> aTv addConst cmpp
                           $ aTv addConst newp
                           $ aTv (propogateUpPtrUsage result) cmpp
                           $ aTv (propogateUpPtrUsage result) newp f
        I_atomicrmw{..} -> aTv addConst pointer
                           $ aTv addConst val
                           $ aTv addAddrStoringValue pointer
                           $ aTv (propogateUpPtrUsage result) val f
        I_extractelement_I {..} -> aTv addConst vectorI
                                   $ aTv (propogateUpPtrUsage result) vectorI f
        I_extractelement_F {..} -> aTv addConst vectorF f
        I_extractelement_P {..} -> aTv addConst vectorP
                                   $ aTv (propogateUpPtrUsage result) vectorP f
        I_insertelement_I {..} -> aTv addConst vectorI
                                  $ aTv addConst elementI
                                  $ aTv (propogateUpPtrUsage result) vectorI
                                  $ aTv (propogateUpPtrUsage result) elementI f
        I_insertelement_F {..} -> aTv addConst vectorF
                                  $ aTv addConst elementF f
        I_insertelement_P {..} -> aTv (propogateUpPtrUsage result) vectorP
                                  $ aTv (propogateUpPtrUsage result) elementP f
        I_shufflevector_I{..} -> aTv addConst vector1I $ aTv addConst vector2I
                                 $ aTv (propogateUpPtrUsage result) vector1I
                                 $ aTv (propogateUpPtrUsage result) vector2I f
        I_shufflevector_F{..} -> aTv addConst vector1F $ aTv addConst vector2F f
        I_shufflevector_P{..} -> aTv addConst vector1P $ aTv addConst vector2P
                                 $ aTv (propogateUpPtrUsage result) vector1P
                                 $ aTv (propogateUpPtrUsage result) vector2P f
        I_extractvalue{..} -> aTv addConst record $ aTv (propogateUpPtrUsage result) record f
        I_insertvalue{..} -> aTv addConst record $ aTv addConst element
                             $ aTv (propogateUpPtrUsage result) record
                             $ aTv (propogateUpPtrUsage result) element f
        I_landingpad{..} -> f
        I_getelementptr{..} -> aTv addConst pointer
                               $ aTv (propogateUpPtrUsage result) pointer
                               $ addAddrInvolvingPtrArithm (Val_ssa result) f

        I_getelementptr_V{..} -> aTv addConst vpointer
                                 $ aTv (propogateUpPtrUsage result) vpointer
                                 $ addAddrInvolvingPtrArithm (Val_ssa result) f
        I_icmp{..} -> f
        I_icmp_V{..} -> f
        I_fcmp{..} -> f
        I_fcmp_V{..} -> f
        I_add{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_sub{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_mul{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_udiv{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_sdiv{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_urem{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_srem{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_shl{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_lshr{..} -> addConst operand2 $ addConst operand1
                      $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_ashr{..} -> addConst operand2 $ addConst operand1
                      $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_and{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_or{..} -> addConst operand2 $ addConst operand1
                    $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_xor{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_add_V{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_sub_V{..} -> addConst operand2 $ addConst operand1
                     $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_mul_V{..} -> addConst operand2 $ addConst operand1
                       $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_udiv_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_sdiv_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_urem_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_srem_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_shl_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_lshr_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_ashr_V{..} -> addConst operand2 $ addConst operand1
                        $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_and_V{..} -> addConst operand2 $ addConst operand1
                       $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_or_V{..} -> addConst operand2 $ addConst operand1
                      $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_xor_V{..} -> addConst operand2 $ addConst operand1
                       $ propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f

        I_fadd{..} -> addConst operand2 $ addConst operand1 f
        I_fsub{..} -> addConst operand2 $ addConst operand1 f
        I_fmul{..} -> addConst operand2 $ addConst operand1 f
        I_fdiv{..} -> addConst operand2 $ addConst operand1 f
        I_frem{..} -> addConst operand2 $ addConst operand1 f

        I_fadd_V{..} -> addConst operand2 $ addConst operand1 f
        I_fsub_V{..} -> addConst operand2 $ addConst operand1 f
        I_fmul_V{..} -> addConst operand2 $ addConst operand1 f
        I_fdiv_V{..} -> addConst operand2 $ addConst operand1 f
        I_frem_V{..} -> addConst operand2 $ addConst operand1 f

        I_trunc{..} -> aTv addConst srcI f
        I_zext{..} -> aTv addConst srcI f
        I_sext{..} -> aTv addConst srcI f
        I_fptrunc{..} -> aTv addConst srcF f
        I_fpext{..} -> aTv addConst srcF f
        I_fptoui{..} -> aTv addConst srcF f
        I_fptosi{..} -> aTv addConst srcF f
        I_uitofp{..} -> aTv addConst srcI f
        I_sitofp{..} -> aTv addConst srcI f

        I_ptrtoint{..} -> aTv addConst srcP $ aTv (propogateUpPtrUsage result) srcP f
        I_inttoptr{..} -> aTv addConst srcI $ aTv (propogateUpPtrUsage result) srcI f
        I_addrspacecast{..} -> aTv addConst srcP $ aTv (propogateUpPtrUsage result) srcP f
        I_bitcast{..} -> aTv addConst srcP $ aTv (propogateUpPtrUsage result) srcP f
        I_bitcast_D{..} -> aTv addConst srcD $ aTv (propogateUpPtrUsage result) srcD f

        I_trunc_V{..} -> aTv addConst srcVI f
        I_zext_V{..} -> aTv addConst srcVI f
        I_sext_V{..} -> aTv addConst srcVI f
        I_fptrunc_V{..} -> aTv addConst srcVF f
        I_fpext_V{..} -> aTv addConst srcVF f
        I_fptoui_V{..} -> aTv addConst srcVF f
        I_fptosi_V{..} -> aTv addConst srcVF f
        I_uitofp_V{..} -> aTv addConst srcVI f
        I_sitofp_V{..} -> aTv addConst srcVI f

        I_ptrtoint_V{..} -> aTv addConst srcVP $ aTv (propogateUpPtrUsage result) srcVP f
        I_inttoptr_V{..} -> aTv addConst srcVI $ aTv (propogateUpPtrUsage result) srcVI f
        I_addrspacecast_V{..} -> aTv addConst srcVP $ aTv (propogateUpPtrUsage result) srcVP f
        I_select_I{..} -> aTv addConst trueI $ aTv addConst falseI
                          $ aTv (propogateUpPtrUsage result) trueI
                          $ aTv (propogateUpPtrUsage result) falseI f
        I_select_F{..} -> aTv addConst trueF $ aTv addConst falseF f
        I_select_P{..} -> aTv addConst trueP $ aTv addConst falseP
                          $ aTv (propogateUpPtrUsage result) trueP
                          $ aTv (propogateUpPtrUsage result) falseP f
        I_select_First{..} -> aTv addConst trueFirst $ aTv addConst falseFirst
                              $ aTv (propogateUpPtrUsage result) trueFirst
                              $ aTv (propogateUpPtrUsage result) falseFirst f
        I_select_VI{..} -> aTv addConst trueVI $ aTv addConst falseVI
                           $ aTv (propogateUpPtrUsage result) trueVI
                           $ aTv (propogateUpPtrUsage result) falseVI f
        I_select_VF{..} -> aTv addConst trueVF $ aTv addConst falseVF f
        I_select_VP{..} -> aTv addConst trueVP $ aTv addConst falseVP
                           $ aTv (propogateUpPtrUsage result) trueVP
                           $ aTv (propogateUpPtrUsage result) falseVP f
        I_call_fun{..} ->
          let vals = getValuesFromParams (fs_params $ cfi_signature call_fun_interface)
          in foldl (\p e -> addConst e $ addAddrCaptured e $ addAddrStoringPtr e
                            $ addAddrStoringValue e p
                   ) (f { callFunInfoSet = S.insert (call_ptr, call_fun_interface) (callFunInfoSet f) })
             (S.toList vals)
        I_call_asm{..} ->
          let vals = getValuesFromParams (cai_actualParams call_asm_interface)
          in foldl (\p e -> addConst e $ addAddrCaptured e $ addAddrStoringPtr e
                            $ addAddrStoringValue e p
                   ) (f { callAsmInfoSet = S.insert call_asm_interface (callAsmInfoSet f)}) 
             (S.toList vals)
        I_va_arg{..} -> aTv addConst dv $ aTv (addAddrPassedToVaStart) dv f
        I_llvm_va_start v -> addConst v $ addAddrPassedToVaStart v f
        I_llvm_va_end v -> addConst v $ addAddrPassedToVaStart v f
        I_llvm_va_copy{..} -> addConst destarglist $ addConst srcarglist
                              $ addAddrPassedToVaStart destarglist
                              $ addAddrPassedToVaStart srcarglist f
        I_llvm_memcpy{..} -> aTv addConst dest $ aTv addConst src
                             $ aTv addAddrStoringValue dest
                             $ aTv addAddrCaptured dest
                             $ aTv addAddrCaptured src f
        I_llvm_memmove{..} -> aTv addConst dest $ aTv addConst src
                              $ aTv addAddrStoringValue dest
                              $ aTv addAddrCaptured dest
                              $ aTv addAddrCaptured src f
        I_llvm_memset{..} -> aTv addConst dest $ aTv addConst setValue
                             $ aTv addAddrStoringValue dest
                             $ aTv addAddrCaptured dest
                             $ aTv addAddrCaptured setValue f
        I_llvm_read_register{..} -> f                             
        I_llvm_write_register{..} -> f
        I_llvm_stacksave{..} -> f
        I_llvm_stackrestore{..} -> f
        I_llvm_libm_una{..} -> f
        I_llvm_libm_bin{..} -> f
        I_llvm_powi{..} -> f
        _ -> errorLoc FLC $ show n ++ " is not supported."
      Mnode _ _ -> f
#ifdef DEBUG
      _ -> errorLoc FLC $ show n
#endif
    getValuesFromParams :: [FunOperand (Value g)] -> S.Set (Value g)
    getValuesFromParams ls = foldl (\p e -> case e of
                                                 FunOperandAsRet _ _ _ v -> S.insert v p
                                                 FunOperandData _ _ _ v -> S.insert v p
                                                 FunOperandByVal _ _ _ v -> S.insert v p
                                                 _ -> p
                                              ) S.empty ls

scanGraph :: (H.CheckpointMonad m, H.FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => S.Set LocalId 
             -> Label -> H.Graph (Node g a) H.C H.C -> m (DataUsage g)
scanGraph fm entry graph =
  do { (_, a, _) <- H.analyzeAndRewriteBwd (bwdScan fm) (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyDataUsage (H.lookupFact entry a))
     }

scanDefine :: (CheckpointMonad m, FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => IrCxt g 
              -> TlDefine g a -> m (DataUsage g)
scanDefine s (TlDefine fn entry graph) = scanGraph formalParamIds entry graph
  where formalParamIds :: S.Set LocalId
        formalParamIds = let FunSignature { fs_params = r} = fi_signature fn
                         in foldl (\p x -> case x of
                                      FunOperandAsRet (DtypeScalarP _)  _ _ v -> S.insert v p
                                      FunOperandData (DtypeScalarP _)  _ _ v -> S.insert v p
                                      FunOperandByVal (DtypeScalarP _)  _ _ v -> S.insert v p
                                      _ -> p
                                  ) S.empty r

scanModule :: (H.CheckpointMonad m, H.FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => Module g a -> IrCxt g -> 
              m (Dm.Map (FunctionInterface g) (DataUsage g))
scanModule (Module l) ic =
  do { l0 <- mapM (\x -> case x of
                      ToplevelDefine def@(TlDefine fn _ _) ->
                        do { fct <- scanDefine ic def
                           ; return (Dm.insert fn fct Dm.empty)
                           }
                      _ -> return Dm.empty
                  ) l
     ; return $ Dm.unions l0
     }
