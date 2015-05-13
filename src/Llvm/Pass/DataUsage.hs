{-# LANGUAGE CPP, ScopedTypeVariables, GADTs, RecordWildCards, TemplateHaskell #-}

module Llvm.Pass.DataUsage (scanModule,DataUsage(..)) where
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as Dm
import Llvm.Query.HirCxt

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Hir.Data
import Llvm.Hir.Print
import Llvm.ErrorLoc

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

data DataUsage = DataUsage { -- | The addresses that store pointer parameters
                             addrs_storing_ptr_params :: S.Set Value
                             -- | The local addresses that store pointer parameters
                           , stack_addrs_storing_ptr_params :: S.Set LocalId
                             -- | An address is captured if it's stored on stack or heap
                           , addrs_captured :: S.Set Value
                             -- | An address of a local variable is stored on stack or heap
                           , stack_addrs_captured :: S.Set LocalId
                             -- | The stack or heap addresses that store another addresses
                           , addrs_storing_ptrs :: S.Set Value
                             -- | The addresses of local variables that stores another addresses
                           , stack_addrs_storing_ptrs :: S.Set LocalId
                             -- | The addresses are passed to va_start 
                           , addrs_passed_to_va_start :: S.Set Value
                             -- | The addresses of a local variables that are passed to va_start
                           , stack_addrs_passed_to_va_start :: S.Set LocalId
                             -- | The addresses of all stores
                           , addrs_storing_values :: S.Set Value
                             -- | The addresses of local variable that store values
                           , stack_addrs_storing_values :: S.Set LocalId
                             -- | The addresses that are involved in pointer arithmatic
                           , addrs_involving_pointer_arithmatic :: S.Set Value
                             -- | The stack addresses that are involved in pointer arithmatic
                           , stack_addrs_involving_pointer_arithmatic :: S.Set LocalId
                           , callSites :: S.Set (Rtype, FunPtr, [Dtype])
                           } deriving (Eq, Ord, Show)


addAddrStoringPtrParam :: Value -> DataUsage -> DataUsage
addAddrStoringPtrParam v du@DataUsage{..} = 
  du { addrs_storing_ptr_params = S.insert v addrs_storing_ptr_params }
  
addStackAddrStoringPtrParam :: LocalId -> DataUsage -> DataUsage  
addStackAddrStoringPtrParam v du@DataUsage{..} =
  du { stack_addrs_storing_ptr_params = S.insert v stack_addrs_storing_ptr_params }
  
addAddrCaptured :: Value -> DataUsage -> DataUsage  
addAddrCaptured v du@DataUsage{..} =
  du { addrs_captured = S.insert v addrs_captured }
  
addStackAddrCaptured :: LocalId -> DataUsage -> DataUsage  
addStackAddrCaptured v du@DataUsage{..} = 
  du { stack_addrs_captured = S.insert v stack_addrs_captured }
  
addAddrStoringPtr :: Value -> DataUsage -> DataUsage  
addAddrStoringPtr v du@DataUsage{..} =
  du { addrs_storing_ptrs = S.insert v addrs_storing_ptrs }
  
addStackAddrStoringPtr :: LocalId -> DataUsage -> DataUsage
addStackAddrStoringPtr v du@DataUsage{..} =
  du { stack_addrs_storing_ptrs = S.insert v stack_addrs_storing_ptrs }
  
addAddrPassedToVaStart :: Value -> DataUsage -> DataUsage
addAddrPassedToVaStart v du@DataUsage{..} = 
  du { addrs_passed_to_va_start = S.insert v addrs_passed_to_va_start }
  
addStackAddrPassedToVaStart :: LocalId -> DataUsage -> DataUsage
addStackAddrPassedToVaStart v du@DataUsage{..} =
  du { stack_addrs_passed_to_va_start = S.insert v stack_addrs_passed_to_va_start }
  
addAddrStoringValue :: Value -> DataUsage -> DataUsage
addAddrStoringValue v du@DataUsage{..} =
  du { addrs_storing_values = S.insert v addrs_storing_values }
  
addStackAddrStoringValue :: LocalId -> DataUsage -> DataUsage  
addStackAddrStoringValue v du@DataUsage{..} = 
  du { stack_addrs_storing_values = S.insert v stack_addrs_storing_values }

addAddrInvolvingPtrArithm :: Value -> DataUsage -> DataUsage
addAddrInvolvingPtrArithm v du@DataUsage{..} = 
  du { addrs_involving_pointer_arithmatic = S.insert v addrs_involving_pointer_arithmatic }

addStackAddrInvolvingPtrArithm :: LocalId -> DataUsage -> DataUsage
addStackAddrInvolvingPtrArithm v du@DataUsage{..} = 
  du { stack_addrs_involving_pointer_arithmatic = S.insert v stack_addrs_involving_pointer_arithmatic }

bubbleUp :: Ord x => x -> x -> S.Set x -> S.Set x
bubbleUp dest src s = if S.member dest s then S.insert src s else s

filterAlloca :: LocalId -> (DataUsage -> S.Set Value) -> (LocalId -> DataUsage -> DataUsage) -> DataUsage -> DataUsage
filterAlloca ssa src addf du = if S.member (Val_ssa ssa) (src du) then addf ssa du else du

bubbleUp2 :: Value -> (DataUsage -> S.Set Value) -> Value -> (Value -> DataUsage -> DataUsage) -> DataUsage -> DataUsage
bubbleUp2 dest setf src addf du = if S.member dest (setf du) then addf src du else du

propogateUpPtrUsage :: LocalId -> Value -> DataUsage -> DataUsage
propogateUpPtrUsage dest src du = 
  bubbleUp2 (Val_ssa dest) addrs_storing_ptr_params src addAddrStoringPtrParam
  $ bubbleUp2 (Val_ssa dest) addrs_captured src addAddrCaptured
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
--  $ bubbleUp2 (Val_ssa dest) addrs_passed_to_va_start src addAddrPassedToVaStart
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
  $ bubbleUp2 (Val_ssa dest) addrs_storing_values src addAddrStoringValue
  $ bubbleUp2 (Val_ssa dest) addrs_involving_pointer_arithmatic src addAddrInvolvingPtrArithm  
  du

emptyDataUsage :: DataUsage                            
emptyDataUsage = 
  DataUsage S.empty S.empty S.empty S.empty S.empty 
  S.empty S.empty S.empty S.empty S.empty 
  S.empty S.empty S.empty

instance (IrPrint t1, IrPrint t2, IrPrint t3) => IrPrint (t1, t2, t3) where
  printIr (t1, t2, t3) = parens (printIr t1 <+> printIr t2 <+> printIr t3)

instance IrPrint DataUsage where
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
    $+$ text "callSites:" <+> printIr callSites

usageLattice :: H.DataflowLattice DataUsage
usageLattice = H.DataflowLattice
              { H.fact_name = "Data and Ssa variable Usage"
              , H.fact_bot = emptyDataUsage
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
            where
              j = unionDataUsage old new 
              ch = H.changeIf (j /= old)

unionDataUsage :: DataUsage -> DataUsage -> DataUsage
unionDataUsage (DataUsage s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13) (DataUsage t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) = 
  DataUsage (s1 `S.union` t1) (s2 `S.union` t2) (s3 `S.union` t3) 
  (s4 `S.union` t4) (s5 `S.union` t5) (s6 `S.union` t6)
  (s7 `S.union` t7) (s8 `S.union` t8) (s9 `S.union` t9)
  (s10 `S.union` t10) (s11 `S.union` t11) (s12 `S.union` t12) 
  (s13 `S.union` t13)

bwdScan :: forall a.forall m. (Show a, H.FuelMonad m) => S.Set LocalId -> H.BwdPass m (Node a) DataUsage
bwdScan formalParams = H.BwdPass { H.bp_lattice = usageLattice
                                 , H.bp_transfer = H.mkBTransfer (bwdTran formalParams)
                                 , H.bp_rewrite = H.noBwdRewrite
                                 }
  where
    bwdTran :: S.Set LocalId -> Node a e x -> H.Fact x DataUsage -> DataUsage
    bwdTran fp n f = case n of 
      Tnode _ _ -> 
        let bs = H.successors n
        in foldl (\p l -> p `unionDataUsage` (fromMaybe emptyDataUsage $ H.lookupFact l f)) emptyDataUsage bs
      Lnode _ -> f
      Pnode (Pinst{..}) _ -> foldl (\p (e, _) -> propogateUpPtrUsage flowout e p) f flowins
      Enode x -> f
      Comment _ -> f
      Cnode comp _ -> case comp of
        I_alloca{..} -> 
          filterAlloca result addrs_storing_ptr_params addStackAddrStoringPtrParam
          $ filterAlloca result addrs_storing_ptrs addStackAddrStoringPtr
          $ filterAlloca result addrs_storing_values addStackAddrStoringValue
          $ filterAlloca result addrs_captured addStackAddrCaptured
          $ filterAlloca result addrs_passed_to_va_start addStackAddrPassedToVaStart 
          $ filterAlloca result addrs_involving_pointer_arithmatic addStackAddrInvolvingPtrArithm f
        
        I_load{..} -> 
          let (T _ ptrv) = pointer
          in addAddrStoringValue ptrv f
        I_loadatomic{..} -> 
          let (T _ ptrv) = pointer
          in addAddrStoringValue ptrv f
        I_store{..} -> 
          let (T _ ptrv) = pointer
              f0 = addAddrStoringValue ptrv f
          in addAddrStoringValue ptrv $ case storedvalue of
            T (DtypeScalarP _) sv -> 
              case sv of
                Val_ssa v | S.member v fp -> addAddrStoringPtrParam ptrv $ addAddrCaptured sv f0
                _ -> addAddrStoringPtr ptrv $ addAddrCaptured sv f0
            _ -> f0
        I_storeatomic{..} -> 
          let (T _ ptrv) = pointer
              f0 = addAddrStoringValue ptrv f
          in addAddrStoringValue ptrv $ case storedvalue of
            T (DtypeScalarP _) sv -> 
              case sv of
                Val_ssa v | S.member v fp -> addAddrStoringPtrParam ptrv $ addAddrCaptured sv f0
                _ -> addAddrStoringPtr ptrv $ addAddrCaptured sv f0
            _ -> f0
        I_fence{..} -> f    
        I_cmpxchg_I{..} -> let (T _ v1) = cmpi
                               (T _ v2) = newi           
                           in propogateUpPtrUsage result v1
                              $ propogateUpPtrUsage result v2 f
        I_cmpxchg_F{..} -> f
        I_cmpxchg_P{..} -> let (T _ v1) = cmpp
                               (T _ v2) = newp           
                           in propogateUpPtrUsage result v1
                              $ propogateUpPtrUsage result v2 f
        I_atomicrmw{..} -> let (T _ ptrv) = pointer
                               (T _ v) = val
                           in addAddrStoringValue ptrv 
                              $ propogateUpPtrUsage result v f
          
        I_extractelement_I {..} -> let (T _ src) = vectorI
                                   in propogateUpPtrUsage result src f
        I_extractelement_F {..} -> f
        I_extractelement_P {..} -> let (T _ src) = vectorP
                                   in propogateUpPtrUsage result src f
        I_insertelement_I {..} -> let (T _ src) = vectorI
                                      (T _ e) = elementI
                                   in propogateUpPtrUsage result src 
                                      $ propogateUpPtrUsage result e f
        I_insertelement_F {..} -> f
        I_insertelement_P {..} -> let (T _ src) = vectorP
                                      (T _ e) = elementP
                                  in propogateUpPtrUsage result src 
                                     $ propogateUpPtrUsage result e f                                  
        I_shufflevector_I{..} -> let (T _ v1) = vector1I
                                     (T _ v2) = vector2I
                                 in propogateUpPtrUsage result v1
                                    $ propogateUpPtrUsage result v2 f
        I_shufflevector_F{..} -> f
        I_shufflevector_P{..} -> let (T _ v1) = vector1P
                                     (T _ v2) = vector2P
                                 in propogateUpPtrUsage result v1
                                    $ propogateUpPtrUsage result v2 f                           
        I_extractvalue{..} -> let (T _ v) = record
                              in propogateUpPtrUsage result v f
        I_insertvalue{..} -> let (T _ v) = record
                                 (T _ e) = element
                             in propogateUpPtrUsage result v 
                                $ propogateUpPtrUsage result e f                                 
        I_landingpad{..} -> f
        I_getelementptr{..} -> let (T _ ptr) = pointer
                               in propogateUpPtrUsage result ptr (addAddrInvolvingPtrArithm (Val_ssa result) f)
                                
        I_getelementptr_V{..} -> let (T _ ptr) = vpointer
                                 in propogateUpPtrUsage result ptr (addAddrInvolvingPtrArithm (Val_ssa result) f)

        I_icmp{..} -> f
        I_icmp_V{..} -> f
        I_fcmp{..} -> f
        I_fcmp_V{..} -> f
        I_add{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f  
        I_sub{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_mul{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_udiv{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_sdiv{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_urem{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_srem{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_shl{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f        
        I_lshr{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f                
        I_ashr{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_and{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_or{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_xor{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        
        I_add_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f  
        I_sub_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_mul_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_udiv_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_sdiv_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_urem_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_srem_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_shl_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f        
        I_lshr_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f                
        I_ashr_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_and_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_or_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        I_xor_V{..} -> propogateUpPtrUsage result operand2 $ propogateUpPtrUsage result operand1 f
        
        I_fadd{..} -> f
        I_fsub{..} -> f
        I_fmul{..} -> f
        I_fdiv{..} -> f
        I_frem{..} -> f

        I_fadd_V{..} -> f
        I_fsub_V{..} -> f
        I_fmul_V{..} -> f
        I_fdiv_V{..} -> f
        I_frem_V{..} -> f

        I_trunc{..} -> f
        I_zext{..} -> f
        I_sext{..} -> f
        I_fptrunc{..} -> f
        I_fpext{..} -> f
        I_fptoui{..} -> f
        I_fptosi{..} -> f
        I_uitofp{..} -> f
        I_sitofp{..} -> f
        I_ptrtoint{..} -> let (T _ src) = srcP
                          in propogateUpPtrUsage result src f
        I_inttoptr{..} -> let (T _ src) = srcI
                          in propogateUpPtrUsage result src f
        I_addrspacecast{..} -> let (T _ src) = srcP
                               in propogateUpPtrUsage result src f
        I_bitcast{..} -> let (T _ src) = srcP
                         in propogateUpPtrUsage result src f
        I_bitcast_D{..} -> let (T _ src) = srcD
                           in propogateUpPtrUsage result src f
        
        I_trunc_V{..} -> f
        I_zext_V{..} -> f
        I_sext_V{..} -> f
        I_fptrunc_V{..} -> f
        I_fpext_V{..} -> f
        I_fptoui_V{..} -> f
        I_fptosi_V{..} -> f
        I_uitofp_V{..} -> f
        I_sitofp_V{..} -> f
        I_ptrtoint_V{..} -> let (T _ src) = srcVP
                            in propogateUpPtrUsage result src f
        I_inttoptr_V{..} -> let (T _ src) = srcVI
                            in propogateUpPtrUsage result src f
        I_addrspacecast_V{..} -> let (T _ src) = srcVP
                                 in propogateUpPtrUsage result src f                               
        I_select_I{..} -> let (T _ src1) = trueI
                              (T _ src2) = falseI              
                          in propogateUpPtrUsage result src1
                             $ propogateUpPtrUsage result src2 f
        I_select_F{..} -> f
        I_select_P{..} -> let (T _ src1) = trueP
                              (T _ src2) = falseP              
                          in propogateUpPtrUsage result src1
                             $ propogateUpPtrUsage result src2 f
        I_select_First{..} -> let (T _ src1) = trueFirst
                                  (T _ src2) = falseFirst
                              in propogateUpPtrUsage result src1
                                 $ propogateUpPtrUsage result src2 f
        I_select_VI{..} -> let (T _ src1) = trueVI
                               (T _ src2) = falseVI              
                           in propogateUpPtrUsage result src1
                              $ propogateUpPtrUsage result src2 f
        I_select_VF{..} -> f
        I_select_VP{..} -> let (T _ src1) = trueVP
                               (T _ src2) = falseVP              
                           in propogateUpPtrUsage result src1
                              $ propogateUpPtrUsage result src2 f
        I_call_fun{..} -> 
          let vals = getValuesFromParams call_actualParams
          in foldl (\p e -> addAddrCaptured e
                            $ addAddrStoringPtr e
                            $ addAddrStoringValue e
                            p
                   ) (f { callSites = S.insert (returnType call_type, call_ptr, typesOfActualParams call_actualParams) 
                                      (callSites f)}) (S.toList vals)
        I_call_asm{..} -> 
          let vals = getValuesFromParams call_actualParams
          in foldl (\p e -> addAddrPassedToVaStart e 
                            $ addAddrCaptured e
                            $ addAddrStoringPtr e
                            $ addAddrStoringValue e
                            p
                   ) f (S.toList vals)
        I_va_arg{..} -> let (T _ v) = dv
                        in addAddrPassedToVaStart v f
        I_llvm_va_start v -> addAddrPassedToVaStart v f
        I_llvm_va_end v -> addAddrPassedToVaStart v f
        I_llvm_memcpy{..} -> let (T _ d) = dest
                                 (T _ s) = src
                             in addAddrStoringValue d
                                $ addAddrCaptured d
                                $ addAddrCaptured s f
        I_llvm_memmove{..} -> let (T _ d) = dest
                                  (T _ s) = src
                              in addAddrStoringValue d
                                 $ addAddrCaptured d
                                 $ addAddrCaptured s f
        I_llvm_memset{..} -> let (T _ d) = dest
                                 (T _ s) = setValue
                             in addAddrStoringValue d
                                $ addAddrCaptured d
                                $ addAddrCaptured s f               
        I_llvm_libm_una{..} -> f             
        I_llvm_libm_bin{..} -> f
        I_llvm_powi{..} -> f
        _ -> errorLoc FLC $ show n ++ " is not supported."
      Mnode _ _ -> f
#ifdef DEBUG      
      _ -> errorLoc FLC $ show n
#endif
    getValuesFromParams :: [ActualParam] -> S.Set Value
    getValuesFromParams ls = foldl (\p e -> case e of
                                       ActualParamData _ _ _ v _ -> S.insert v p
                                       _ -> p
                                   ) S.empty ls
                             
typesOfActualParams :: [ActualParam] -> [Dtype]
typesOfActualParams aps = let b = all (\x -> case x of
                                          ActualParamData _ _ _ _ _ -> True
                                          _ -> False
                                      ) aps 
                              ds = fmap (\(ActualParamData d _ _ _ _) -> d) aps
                          in if b then ds
                             else []

returnType :: CallSiteType -> Rtype
returnType ct = case ct of
  CallSiteRet rt -> rt
  CallSiteFun (Tfunction rt _ _) _ -> rt

scanGraph :: (H.CheckpointMonad m, H.FuelMonad m, Show a) => S.Set LocalId -> Label -> H.Graph (Node a) H.C H.C -> m DataUsage
scanGraph fm entry graph =
  do { (_, a, b) <- H.analyzeAndRewriteBwd (bwdScan fm) (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyDataUsage (H.lookupFact entry a))
     }

scanDefine :: (CheckpointMonad m, FuelMonad m, Show a) => IrCxt -> TlDefine a -> m DataUsage
scanDefine s (TlDefine fn entry graph) = scanGraph formalParamIds entry graph
  where formalParamIds :: S.Set LocalId
        formalParamIds = let (FormalParamList l _ _) = fp_param_list fn
                         in foldl (\p x -> case x of
                                      FormalParamData (DtypeScalarP _)  _ _ (FexplicitParam v) _ -> S.insert v p
                                      _ -> p
                                  ) S.empty l
  
scanModule :: (H.CheckpointMonad m, H.FuelMonad m, Show a) => Module a -> IrCxt -> m (Dm.Map FunctionPrototype DataUsage)
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