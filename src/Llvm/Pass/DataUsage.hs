{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards #-}

module Llvm.Pass.DataUsage where
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as Dm
import Llvm.Query.IrCxt

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Data.Ir
import Llvm.Syntax.Printer.IrPrint

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
                           , callSites :: S.Set (Rtype, FunName, [Dtype])
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
  du { addrs_storing_values = S.insert v addrs_storing_ptrs }
  
addStackAddrStoringValue :: LocalId -> DataUsage -> DataUsage  
addStackAddrStoringValue v du@DataUsage{..} = 
  du { stack_addrs_storing_values = S.insert v stack_addrs_storing_values }

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
  $ bubbleUp2 (Val_ssa dest) addrs_passed_to_va_start src addAddrPassedToVaStart
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
  $ bubbleUp2 (Val_ssa dest) addrs_storing_values src addAddrStoringValue
  du

emptyDataUsage :: DataUsage                            
emptyDataUsage = DataUsage S.empty S.empty S.empty S.empty S.empty 
                 S.empty S.empty S.empty S.empty S.empty S.empty

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
unionDataUsage (DataUsage s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11) (DataUsage t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) = 
  DataUsage (s1 `S.union` t1) (s2 `S.union` t2) (s3 `S.union` t3) 
  (s4 `S.union` t4) (s5 `S.union` t5) (s6 `S.union` t6)
  (s7 `S.union` t7) (s8 `S.union` t8) (s9 `S.union` t9)
  (s10 `S.union` t10) (s11 `S.union` t11)

bwdScan :: forall a.forall m. (Show a, H.FuelMonad m) => S.Set LocalId -> H.BwdPass m (Node a) DataUsage
bwdScan formalParams = H.BwdPass { H.bp_lattice = usageLattice
                                 , H.bp_transfer = H.mkBTransfer (bwdTran formalParams)
                                 , H.bp_rewrite = H.noBwdRewrite
                                 }
  where
    bwdTran :: S.Set LocalId -> Node a e x -> H.Fact x DataUsage -> DataUsage
    bwdTran _ n@(Tnode _ _) f = 
      let bs = H.successors n
      in foldl (\p l -> p `unionDataUsage` (fromMaybe emptyDataUsage $ H.lookupFact l f)) emptyDataUsage bs
    bwdTran _ (Lnode _) f = f
    bwdTran _ (Pnode (Pinst{..}) _) f = foldl (\p (e, _) -> propogateUpPtrUsage flowout e p) f flowins
    bwdTran _ (Enode x) f = f -- error $ "unexpected " ++ show x
    bwdTran fp (Cnode comp _) f = case comp of
      I_store{..} -> 
        let (T _ ptrv) = pointer
        in addAddrStoringValue ptrv $ case (storedvalue, pointer) of
          (T (DtypeScalarP _) sv, T _ p) -> 
            case sv of
              Val_ssa v | S.member v fp -> addAddrStoringPtrParam p $ addAddrCaptured sv f
              _ -> addAddrStoringPtr p $ addAddrCaptured sv f
          (T _ sv, _) -> addAddrCaptured sv f
          (_,_) -> f
      I_alloca{..} -> 
        filterAlloca result addrs_storing_ptr_params addStackAddrStoringPtrParam
        $ filterAlloca result addrs_storing_ptrs addStackAddrStoringPtr
        $ filterAlloca result addrs_storing_values addStackAddrStoringValue
        $ filterAlloca result addrs_captured addStackAddrCaptured
        $ filterAlloca result addrs_passed_to_va_start addStackAddrPassedToVaStart f
      I_getelementptr{..} -> let (T _ ptr) = pointer
                             in propogateUpPtrUsage result ptr f
      I_bitcast{..} -> let (T _ src) = srcP
                       in propogateUpPtrUsage result src f
      I_bitcast_D{..} -> let (T _ src) = srcD
                         in propogateUpPtrUsage result src f
      I_ptrtoint{..} -> let (T _ src) = srcP
                        in propogateUpPtrUsage result src f
      I_inttoptr{..} -> let (T _ src) = srcI
                        in propogateUpPtrUsage result src f
      I_addrspacecast{..} -> let (T _ src) = srcP
                             in propogateUpPtrUsage result src f
      I_select_P{..} -> let (T _ src1) = trueP
                            (T _ src2) = falseP              
                        in propogateUpPtrUsage result src1
                           $ propogateUpPtrUsage result src2 f
      I_select_First{..} -> let (T _ src1) = trueFirst
                                (T _ src2) = falseFirst
                            in propogateUpPtrUsage result src1
                               $ propogateUpPtrUsage result src2 f
      I_va_start (T _ v) -> addAddrPassedToVaStart v f
      I_va_end (T _ v) -> addAddrPassedToVaStart v f
      I_call_fun{..} -> 
        let vals = getValuesFromParams actualParams
        in foldl (\p e -> addAddrPassedToVaStart e 
                          $ addAddrCaptured e
                          $ addAddrStoringPtr e
                          $ addAddrStoringValue e
                          p
                 ) (f { callSites = S.insert (returnType callSiteType, calleeName, typesOfActualParams actualParams) (callSites f)}) (S.toList vals)
      _ -> f
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