{-# LANGUAGE CPP, ScopedTypeVariables, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses #-}

module Llvm.Pass.DataUsage where
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Llvm.Query.HirCxt

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Hir.Data
import Llvm.Hir.Print
import Llvm.ErrorLoc
import Llvm.Hir.Mangle
import Data.List(foldl')

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

data Summary g = Summary 
  { -- | The addresses that store function parameters that are pointers
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
  -- | The values used as pointers
  , addrs :: S.Set (Value g)
  , stack_addrs :: S.Set LocalId
  -- | The call function info set
  , callFunInfoSet :: S.Set (FunPtr g, CallFunInterface g)
  , invokeFunInfoSet :: S.Set (FunPtr g, InvokeFunInterface g)
  , callAsmInfoSet :: S.Set (AsmCode, CallAsmInterface g)
  } deriving (Eq, Ord, Show)
                                      
data DuFact g = DuFact { summary :: Summary g
                       , liveness :: S.Set LocalId
                       } deriving (Eq, Ord, Show)
                                  
data DataUsage g = DataUsage { usageSummary :: Summary g
                             , livenessFact :: LabelMap (S.Set LocalId)
                             } deriving (Eq, Ord, Show)
                                        
                                        

class DataUsageUpdator g a where
  update :: a -> DuFact g -> DuFact g
  
instance DataUsageUpdator Gname () where
  update _ = id

addAddrStoringPtrParam :: Ord g => Value g -> DuFact g -> DuFact g
addAddrStoringPtrParam v du@DuFact{ summary = ds@Summary {..} } =
  du { summary = ds {addrs_storing_ptr_params = S.insert v addrs_storing_ptr_params} }

addStackAddrStoringPtrParam :: LocalId -> DuFact g -> DuFact g
addStackAddrStoringPtrParam v du@DuFact{ summary = ds@Summary {..} } =
  du { summary = ds {stack_addrs_storing_ptr_params = S.insert v stack_addrs_storing_ptr_params} }

addAddrCaptured :: Ord g => Value g -> DuFact g -> DuFact g
addAddrCaptured v du@DuFact{ summary = ds@Summary{..} } =
  du { summary = ds { addrs_captured = S.insert v addrs_captured } }

addStackAddrCaptured :: LocalId -> DuFact g -> DuFact g
addStackAddrCaptured v du@DuFact{ summary = ds@Summary{..} } =
  du { summary = ds {stack_addrs_captured = S.insert v stack_addrs_captured } }

addAddrStoringPtr :: Ord g => Value g -> DuFact g -> DuFact g
addAddrStoringPtr v du@DuFact{ summary = ds@Summary{..} } =
  du { summary = ds {addrs_storing_ptrs = S.insert v addrs_storing_ptrs }}

addStackAddrStoringPtr :: LocalId -> DuFact g -> DuFact g
addStackAddrStoringPtr v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {stack_addrs_storing_ptrs = S.insert v stack_addrs_storing_ptrs }}

addAddrPassedToVaStart :: Ord g => Value g -> DuFact g -> DuFact g
addAddrPassedToVaStart v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {addrs_passed_to_va_start = S.insert v addrs_passed_to_va_start }}

addStackAddrPassedToVaStart :: LocalId -> DuFact g -> DuFact g
addStackAddrPassedToVaStart v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {stack_addrs_passed_to_va_start = S.insert v stack_addrs_passed_to_va_start }}

addAddrStoringValue :: Ord g => Value g -> DuFact g -> DuFact g
addAddrStoringValue v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {addrs_storing_values = S.insert v addrs_storing_values }}

addStackAddrStoringValue :: LocalId -> DuFact g -> DuFact g
addStackAddrStoringValue v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {stack_addrs_storing_values = S.insert v stack_addrs_storing_values }}

addAddr :: Ord g => Value g -> DuFact g -> DuFact g
addAddr v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {addrs = S.insert v addrs}}

addStackAddr :: LocalId -> DuFact g -> DuFact g
addStackAddr v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {stack_addrs = S.insert v stack_addrs }}

addAddrInvolvingPtrArithm :: Ord g => Value g -> DuFact g -> DuFact g
addAddrInvolvingPtrArithm v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {addrs_involving_pointer_arithmatic = S.insert v addrs_involving_pointer_arithmatic }}

addStackAddrInvolvingPtrArithm :: LocalId -> DuFact g -> DuFact g
addStackAddrInvolvingPtrArithm v du@DuFact{ summary = ds@Summary{..}} =
  du { summary = ds {stack_addrs_involving_pointer_arithmatic = S.insert v stack_addrs_involving_pointer_arithmatic }}

addConst :: Ord g => Value g -> DuFact g -> DuFact g
addConst (Val_const c) du@DuFact{ summary = ds@Summary{..}} = 
  case c of
    C_u8 _ -> du
    C_u16 _ -> du
    C_u32 _ -> du
    C_u64 _ -> du
    C_u96 _ -> du
    C_u128 _ -> du
    C_s8 _ -> du
    C_s16 _ -> du
    C_s32 _ -> du
    C_s64 _ -> du
    C_s96 _ -> du
    C_s128 _ -> du
    C_int _ -> du
    C_uhex_int _ -> du
    C_shex_int _ -> du
    C_float _ -> du
    C_null -> du
    C_true -> du
    C_false -> du
    _ -> du { summary = ds {constants = S.insert c constants }}
addConst (Val_ssa _) du = du


addLive :: Ord g => Value g -> DuFact g -> DuFact g
addLive (Val_ssa lid) du@DuFact{..} = 
  if S.member lid liveness then
    du
  else
    du { liveness = S.insert lid liveness }
addLive _ du = du

killLive :: Ord g => LocalId -> DuFact g -> DuFact g
killLive lid du@DuFact{..} = 
  if S.member lid liveness then
    du { liveness = S.delete lid liveness }
  else
    du

addMaybeConst :: Ord g => Maybe (Value g) -> DuFact g -> DuFact g
addMaybeConst (Just c) du = addConst c du
addMaybeConst Nothing du = du

aTv :: (Value g -> DuFact g -> DuFact g) -> T t (Value g) -> DuFact g -> DuFact g
aTv  f (T _ v) du = f v du

aTvs :: (Value g -> DuFact g -> DuFact g) -> [T t (Value g)] -> DuFact g -> DuFact g
aTvs f l du = foldl' (\p e -> aTv f e p) du l

aTvE :: [Value g -> DuFact g -> DuFact g] -> T t (Value g) -> DuFact g -> DuFact g
aTvE  fs tv du = foldl' (\p f -> aTv f tv p) du fs

aTvsE :: [Value g -> DuFact g -> DuFact g] -> [T t (Value g)] -> DuFact g -> DuFact g
aTvsE fs l du = foldl' (\p e -> foldl' (\pp f -> aTv f e pp) p fs) du l


foldlvs :: (Value g -> DuFact g -> DuFact g) -> [Value g] -> DuFact g -> DuFact g
foldlvs  f vs du = foldl' (\p e -> f e p) du  vs

foldlvsE :: [Value g -> DuFact g -> DuFact g] -> [Value g] -> DuFact g -> DuFact g
foldlvsE  fs vs du = foldl' (\du0 f -> foldl' (\p e -> f e p) du0 vs) du fs

foldlE :: [Value g -> DuFact g -> DuFact g] -> Value g -> DuFact g -> DuFact g
foldlE fs v du = foldl' (\du0 f -> f v du0) du fs


applyToMaybe :: (a -> DuFact g -> DuFact g) -> Maybe a -> DuFact g -> DuFact g
applyToMaybe f (Just x) du = f x du
applyToMaybe f Nothing du = du

applyToEither :: (a -> DuFact g -> DuFact g) -> (b -> DuFact g -> DuFact g) 
                 -> Either a b -> DuFact g -> DuFact g
applyToEither fl fr (Left x) du = fl x du
applyToEither fl fr (Right x) du = fr x du

bubbleUp :: Ord x => x -> x -> S.Set x -> S.Set x
bubbleUp dest src s = if S.member dest s then S.insert src s else s

filterAlloca :: Ord g => LocalId -> (Summary g -> S.Set (Value g)) 
                -> (LocalId -> DuFact g -> DuFact g) -> DuFact g -> DuFact g
filterAlloca ssa src addf du = if S.member (Val_ssa ssa) (src $ summary du) then addf ssa du else du

bubbleUp2 :: Ord g => Value g -> (Summary g -> S.Set (Value g)) 
             -> Value g -> (Value g -> DuFact g -> DuFact g) -> DuFact g -> DuFact g
bubbleUp2 dest setf src addf du = if S.member dest (setf $ summary du) then addf src du else du

propogateUpPtrUsage :: Ord g => LocalId -> Value g -> DuFact g -> DuFact g
propogateUpPtrUsage dest src du =
  bubbleUp2 (Val_ssa dest) addrs_storing_ptr_params src addAddrStoringPtrParam
  $ bubbleUp2 (Val_ssa dest) addrs_captured src addAddrCaptured
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
  $ bubbleUp2 (Val_ssa dest) addrs_passed_to_va_start src addAddrPassedToVaStart
  $ bubbleUp2 (Val_ssa dest) addrs_storing_ptrs src addAddrStoringPtr
  $ bubbleUp2 (Val_ssa dest) addrs_storing_values src addAddrStoringValue
  $ bubbleUp2 (Val_ssa dest) addrs_involving_pointer_arithmatic src addAddrInvolvingPtrArithm
  $ bubbleUp2 (Val_ssa dest) addrs src addAddr
  du

emptyDuFact :: DuFact g
emptyDuFact =
  DuFact { summary = Summary 
                        S.empty S.empty S.empty S.empty S.empty
                        S.empty S.empty S.empty S.empty S.empty
                        S.empty S.empty S.empty S.empty S.empty 
                        S.empty S.empty S.empty 
         , liveness = S.empty
         }

instance (IrPrint t1, IrPrint t2, IrPrint t3) => IrPrint (t1, t2, t3) where
  printIr (t1, t2, t3) = parens (printIr t1 <+> printIr t2 <+> printIr t3)

instance IrPrint g => IrPrint (Summary g) where
  printIr Summary{..} =
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
    $+$ text "addrs:" <+> printIr addrs
    $+$ text "stack_addrs:" <+> printIr stack_addrs
    $+$ text "callInfoSet:" <+> printIr callFunInfoSet
    $+$ text "callAsmSet:" <+> printIr callAsmInfoSet
  
  
instance IrPrint g => IrPrint (DuFact g) where  
  printIr DuFact{..} = text "summary:" <+> printIr summary
                       $+$ text "liveness:" <+> printIr liveness

instance IrPrint g => IrPrint (DataUsage g) where
  printIr DataUsage{..} = text "usageSummary:" <+> printIr usageSummary
                          $+$ text "livenessFact:" <+> printIr livenessFact
    
usageLattice :: Ord g => H.DataflowLattice (DuFact g)
usageLattice = H.DataflowLattice
              { H.fact_name = "Data and Ssa variable Usage"
              , H.fact_bot = emptyDuFact
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) =
            if old == new then
              (NoChange, old)
            else 
              (SomeChange, unionDuFact old new)

unionDuFact :: Ord g => (DuFact g) -> (DuFact g) -> (DuFact g)
unionDuFact old new = 
  if old == emptyDuFact then
    new
  else if summary old == summary new then
         DuFact (summary old) ((liveness old) `sUnion` (liveness new))
       else 
         DuFact (unionSummary (summary old) (summary new)) ((liveness old) `sUnion` (liveness new))
      
unionSummary :: Ord g => Summary g -> Summary g -> Summary g
unionSummary old_du@(Summary s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18)
  new_du@(Summary t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18)
  = Summary
    (s1 `sUnion` t1)   (s2 `sUnion` t2)   (s3 `sUnion` t3)
    (s4 `sUnion` t4)   (s5 `sUnion` t5)   (s6 `sUnion` t6)
    (s7 `sUnion` t7)   (s8 `sUnion` t8)   (s9 `sUnion` t9)
    (s10 `sUnion` t10) (s11 `sUnion` t11) (s12 `sUnion` t12)
    (s13 `sUnion` t13) (s14 `sUnion` t14) (s15 `sUnion` t15)
    (s16 `sUnion` t16) (s17 `sUnion` t17) (s18 `sUnion` t18) 
    
sUnion :: (Ord v, Eq v) => S.Set v -> S.Set v -> S.Set v
sUnion s1 s2 = if S.isSubsetOf s1 s2 then
                 s2
               else if S.isSubsetOf s2 s1 then
                      s1
                    else lUnion s2 s1 
  where lUnion l r = S.foldl' (\p e -> 
                                if S.member e p then
                                  p
                                else
                                  S.insert e p
                              ) l r

bwdScan :: forall g.forall a.forall m. (Show g, Ord g, Show a, DataUsageUpdator g a, H.FuelMonad m) => 
           S.Set LocalId -> H.BwdPass m (Node g a) (DuFact g)
bwdScan formalParams = H.BwdPass { H.bp_lattice = usageLattice
                                 , H.bp_transfer = H.mkBTransfer (bwdTran formalParams)
                                 , H.bp_rewrite = H.noBwdRewrite
                                 }
  where
    bwdTran :: (Show g, Ord g) => S.Set LocalId -> Node g a e x -> H.Fact x (DuFact g) -> DuFact g
    bwdTran fp n f = case n of
      Tnode tinst _ ->
        let f0 = foldl' (\p l -> p `unionDuFact` (fromMaybe emptyDuFact $ H.lookupFact l f)) 
                 emptyDuFact (H.successors n)
        in case tinst of
          T_ret_void -> f0
          T_return vs -> aTvsE [addConst, addLive] vs f0
          T_invoke{..} ->
            let vals = getValuesFromParams (fs_params $ ifi_signature invoke_fun_interface)
            in applyToMaybe killLive invoke_return
               $ foldlvsE [addConst, addAddrCaptured, addAddrStoringPtr, addAddrStoringValue, addLive] 
               (S.toList vals)
               (f0 { summary = (summary f0) {invokeFunInfoSet = S.insert (invoke_ptr, invoke_fun_interface) (invokeFunInfoSet $ summary f0) }}) 
          T_invoke_asm{..} ->
            let vals = getValuesFromParams (cai_actualParams invoke_asm_interface)
            in applyToMaybe killLive invoke_return
               $ foldlvsE [addConst, addAddrCaptured, addAddrStoringPtr, addAddrStoringValue, addLive]
               (S.toList vals) 
               (f0 { summary = (summary f0) { callAsmInfoSet = S.insert (invoke_asmcode, invoke_asm_interface) (callAsmInfoSet $ summary f0)}})
          _ -> f0
      Lnode _ -> f
      Pnode (Pinst{..}) _ -> 
        killLive flowout
        $ foldl' (\p (e,_) -> foldlE [addLive,addConst,propogateUpPtrUsage flowout] e p) 
        f flowins
      Enode x _ -> update x f
      Comment _ -> f
      Cnode comp _ -> case comp of
        I_alloca{..} ->
          killLive result
          $ applyToMaybe (\a du -> aTvE [addConst,addLive] a du) size 
          $ addStackAddr result 
          $ killLive result
          $ filterAlloca result addrs_storing_ptr_params addStackAddrStoringPtrParam
          $ filterAlloca result addrs_storing_ptrs addStackAddrStoringPtr
          $ filterAlloca result addrs_storing_values addStackAddrStoringValue
          $ filterAlloca result addrs_captured addStackAddrCaptured
          $ filterAlloca result addrs_passed_to_va_start addStackAddrPassedToVaStart
          $ filterAlloca result addrs_involving_pointer_arithmatic addStackAddrInvolvingPtrArithm f
        I_load{..} ->
          killLive result
          $ aTvE [addConst, addAddrStoringValue, addAddr, addLive] pointer
          f
        I_loadatomic{..} ->
          killLive result
          $ aTvE [addConst, addAddrStoringValue, addAddr, addLive] pointer
          f
        I_store{..} ->
          aTvE [addConst,addLive] storedvalue
          $ aTvE [addConst,addAddrStoringValue,addAddr, addLive] pointer
          $ case storedvalue of
            T (DtypeScalarP _) sv ->
              case sv of
                Val_ssa v | S.member v fp -> aTv addAddrStoringPtrParam pointer $ addAddrCaptured sv f
                _ -> aTv addAddrStoringPtr pointer $ addAddrCaptured sv f
            _ -> f
        I_storeatomic{..} -> 
          aTvE [addLive,addConst] storedvalue
          $ aTvE [addLive,addConst, addAddrStoringValue, addAddr] pointer
          $ case storedvalue of
            T (DtypeScalarP _) sv ->
              case sv of
                Val_ssa v | S.member v fp -> aTv addAddrStoringPtrParam pointer $ addAddrCaptured sv f
                _ -> aTv addAddrStoringPtr pointer $ addAddrCaptured sv f
            _ -> f
        I_fence{..} -> f
        I_cmpxchg_I{..} -> killLive result
                           $ aTvE [addLive,addConst] pointer
                           $ aTvsE [addLive,addConst, propogateUpPtrUsage result] [cmpi,newi]
                           f
        I_cmpxchg_F{..} -> killLive result
                           $ aTvE [addLive,addConst] pointer
                           $ aTvsE [addLive,addConst] [cmpf,newf]
                           f
        I_cmpxchg_P{..} -> killLive result
                           $ aTvE [addLive,addConst] pointer
                           $ aTvsE [addLive,addConst, propogateUpPtrUsage result] [cmpp,newp]
                           f
        I_atomicrmw{..} -> killLive result
                           $ aTvE [addLive,addConst,addAddrStoringPtr,addAddr] pointer
                           $ aTvE [addLive,addConst, propogateUpPtrUsage result] val
                           f
        I_extractelement_I {..} -> killLive result
                                   $ aTvE [addLive,addConst, propogateUpPtrUsage result] vectorI
                                   f
        I_extractelement_F {..} -> killLive result
                                   $ aTvE [addLive,addConst] vectorF
                                   f
        I_extractelement_P {..} -> killLive result
                                   $ aTvE [addLive,addConst,propogateUpPtrUsage result] vectorP
                                   f
        I_insertelement_I {..} -> killLive result
                                  $ aTvE [addLive,addConst, propogateUpPtrUsage result] vectorI
                                  $ aTvE [addLive,addConst, propogateUpPtrUsage result] elementI
                                  f
        I_insertelement_F {..} -> killLive result
                                  $ aTvE [addLive,addConst] vectorF
                                  $ aTvE [addLive,addConst] elementF
                                  f
        I_insertelement_P {..} -> killLive result
                                  $ aTvE [addLive,addConst] vectorP
                                  $ aTvE [addLive,addConst,propogateUpPtrUsage result] elementP
                                  f
        I_shufflevector_I{..} -> killLive result
                                 $ aTvsE [addLive,addConst,propogateUpPtrUsage result] [vector1I,vector2I]
                                 f
        I_shufflevector_F{..} -> killLive result
                                 $ aTvsE [addLive,addConst] [vector1F,vector2F]
                                 f
        I_shufflevector_P{..} -> killLive result
                                 $ aTvsE [addLive,addConst, propogateUpPtrUsage result] [vector1P,vector2P]
                                 f
        I_extractvalue{..} -> killLive result
                              $ aTvE [addLive,addConst, propogateUpPtrUsage result] record
                              f
        I_insertvalue{..} -> killLive result
                             $ aTvE [addLive,addConst,propogateUpPtrUsage result] record 
                             $ aTvE [addLive,addConst,propogateUpPtrUsage result] element 
                             f
        I_landingpad{..} -> f
        I_getelementptr{..} -> killLive result
                               $ aTvE [addLive,addConst,propogateUpPtrUsage result] pointer
                               $ addAddrInvolvingPtrArithm (Val_ssa result) 
                               f

        I_getelementptr_V{..} -> killLive result
                                 $ aTvE [addLive,addConst,propogateUpPtrUsage result] vpointer
                                 $ addAddrInvolvingPtrArithm (Val_ssa result) 
                                 f
        I_icmp{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1, operand2]
                      f
        I_icmp_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1, operand2]
                        f
        I_fcmp{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1,operand2]
                      f
        I_fcmp_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1,operand2]
                        f
        I_add{..} -> killLive result
                     $ foldlvsE [addLive,addConst, propogateUpPtrUsage result] [operand1, operand2] 
                     f
        I_sub{..} -> killLive result
                     $ foldlvsE [addLive,addConst, propogateUpPtrUsage result] [operand1, operand2]
                     f
        I_mul{..} -> killLive result
                     $ foldlvsE [addLive,addConst, propogateUpPtrUsage result] [operand1, operand2]
                     f
        I_udiv{..} -> killLive result
                      $ foldlvsE [addLive,addConst, propogateUpPtrUsage result] [operand1, operand2]
                      f
        I_sdiv{..} -> killLive result
                      $ foldlvsE [addLive,addConst, propogateUpPtrUsage result] [operand1, operand2]
                      f
        I_urem{..} -> killLive result
                      $ foldlvsE [addLive,addConst, propogateUpPtrUsage result] [operand1, operand2] 
                      f
        I_srem{..} -> killLive result
                      $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                      f
        I_shl{..} -> killLive result
                     $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                     f
        I_lshr{..} -> killLive result
                      $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                      f
        I_ashr{..} -> killLive result
                      $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                      f
        I_and{..} -> killLive result
                     $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                     f
        I_or{..} -> killLive result
                    $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                    f
        I_xor{..} -> killLive result
                     $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                     f
        I_add_V{..} -> killLive result
                       $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                       f
        I_sub_V{..} -> killLive result
                       $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                       f
        I_mul_V{..} -> killLive result
                       $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                       f
        I_udiv_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                        f
        I_sdiv_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                        f
        I_urem_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                        f
        I_srem_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                        f
        I_shl_V{..} -> killLive result
                       $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                       f
        I_lshr_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                        f
        I_ashr_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                        f
        I_and_V{..} -> killLive result
                       $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                       f
        I_or_V{..} -> killLive result
                      $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                      f
        I_xor_V{..} -> killLive result
                       $ foldlvsE [addLive,addConst,propogateUpPtrUsage result] [operand1, operand2]
                       f
        I_fadd{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1, operand2]
                      f
        I_fsub{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1, operand2]
                      f
        I_fmul{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1, operand2]
                      f
        I_fdiv{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1, operand2]
                      f
        I_frem{..} -> killLive result
                      $ foldlvsE [addLive,addConst] [operand1, operand2]
                      f
        I_fadd_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1, operand2]
                        f
        I_fsub_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1, operand2]
                        f
        I_fmul_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1, operand2]
                        f
        I_fdiv_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1, operand2]
                        f
        I_frem_V{..} -> killLive result
                        $ foldlvsE [addLive,addConst] [operand1, operand2]
                        f
        I_trunc{..} -> killLive result
                       $ aTvE [addLive,addConst] srcI f
        I_zext{..} -> killLive result
                      $ aTvE [addLive, addConst] srcI f
        I_sext{..} -> killLive result
                      $ aTvE [addLive,addConst] srcI f
        I_fptrunc{..} -> killLive result
                         $ aTvE [addLive, addConst] srcF f
        I_fpext{..} -> killLive result
                       $ aTvE [addLive, addConst] srcF f
        I_fptoui{..} -> killLive result
                        $ aTvE [addLive, addConst] srcF f
        I_fptosi{..} -> killLive result
                        $ aTvE [addLive, addConst] srcF f
        I_uitofp{..} -> killLive result
                        $ aTvE [addLive, addConst] srcI f
        I_sitofp{..} -> killLive result
                        $ aTvE [addLive, addConst] srcI f
        I_ptrtoint{..} -> killLive result
                          $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcP 
                          f
        I_inttoptr{..} -> killLive result
                          $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcI 
                          f
        I_addrspacecast{..} -> killLive result
                               $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcP 
                               f
        I_bitcast{..} -> killLive result
                         $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcP 
                         f
        I_bitcast_D{..} -> killLive result
                           $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcD 
                           f
        I_trunc_V{..} -> killLive result $ aTvE [addLive, addConst] srcVI f
        I_zext_V{..} -> killLive result $ aTvE [addLive, addConst] srcVI f
        I_sext_V{..} -> killLive result $ aTvE [addLive, addConst] srcVI f
        I_fptrunc_V{..} -> killLive result $ aTvE [addLive, addConst] srcVF f
        I_fpext_V{..} -> killLive result $ aTvE [addLive, addConst] srcVF f
        I_fptoui_V{..} -> killLive result $ aTvE [addLive, addConst] srcVF f
        I_fptosi_V{..} -> killLive result $ aTvE [addLive, addConst] srcVF f
        I_uitofp_V{..} -> killLive result $ aTvE [addLive, addConst] srcVI f
        I_sitofp_V{..} -> killLive result $ aTvE [addLive, addConst] srcVI f

        I_ptrtoint_V{..} -> killLive result
                            $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcVP f
        I_inttoptr_V{..} -> killLive result
                            $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcVI f
        I_addrspacecast_V{..} -> killLive result
                                 $ aTvE [addLive, addConst, propogateUpPtrUsage result] srcVP f
        I_select_I{..} -> killLive result
                          $ aTvE [addLive,addConst] cond 
                          $ aTvsE [addLive,addConst,propogateUpPtrUsage result] [trueI,falseI]
                          f
        I_select_F{..} -> killLive result
                          $ aTvE [addLive,addConst] cond
                          $ aTvsE [addLive, addConst] [trueF,falseF]
                          f
        I_select_P{..} -> killLive result
                          $ aTvE [addLive,addConst] cond
                          $ aTvsE [addLive,addConst, propogateUpPtrUsage result]  [trueP, falseP]
                          f
        I_select_First{..} -> killLive result
                              $ aTvE [addLive,addConst] cond
                              $ aTvsE [addLive,addConst, propogateUpPtrUsage result] [trueFirst, falseFirst]
                              f
        I_select_VI{..} -> killLive result
                           $ applyToEither (aTvE [addLive,addConst]) (aTvE [addLive,addConst]) condVI
                           $ aTvsE [addLive,addConst, propogateUpPtrUsage result] [trueVI,falseVI]
                           f
        I_select_VF{..} -> killLive result
                           $ applyToEither (aTvE [addLive,addConst]) (aTvE [addLive,addConst]) condVF 
                           $ aTvsE [addLive,addConst] [trueVF, falseVF]
                           f
        I_select_VP{..} -> killLive result
                           $ applyToEither (aTvE [addLive,addConst]) (aTvE [addLive,addConst]) condV
                           $ aTvsE [addLive,addConst, propogateUpPtrUsage result] [trueVP, falseVP]
                           f
        I_call_fun{..} ->
          let vals = getValuesFromParams (fs_params $ cfi_signature call_fun_interface)
          in applyToMaybe killLive call_return
             $ foldlvsE [addConst,addAddrCaptured,addAddrStoringPtr,addAddrStoringValue,addLive] 
             (S.toList vals)
             (f { summary = (summary f) { callFunInfoSet = S.insert (call_ptr, call_fun_interface) (callFunInfoSet $ summary f) }})
        I_call_asm{..} ->
          let vals = getValuesFromParams (cai_actualParams call_asm_interface)
          in applyToMaybe killLive call_return
             $ foldlvsE [addConst, addAddrCaptured, addAddrStoringPtr, addAddrStoringValue, addLive]
             (S.toList vals)
             (f { summary = (summary f) { callAsmInfoSet = S.insert (call_asmcode, call_asm_interface) (callAsmInfoSet $ summary f)}})
        I_va_arg{..} -> aTvE [addLive, addConst, addAddrPassedToVaStart] dv f
        I_llvm_va_start v -> foldlE [addLive, addConst, addAddrPassedToVaStart] v f
        I_llvm_va_end v -> foldlE [addLive, addConst, addAddrPassedToVaStart] v f
        I_llvm_va_copy{..} -> foldlvsE [addLive,addConst, addAddrPassedToVaStart] [srcarglist,destarglist] f
        I_llvm_memcpy{..} -> aTvsE [addLive,addConst, addAddrCaptured] [src,dest] 
                             $ aTv addAddrStoringValue dest
                             f
        I_llvm_memmove{..} -> aTvsE [addLive,addConst, addAddrCaptured] [src,dest] 
                              $ aTv addAddrStoringValue dest
                              f
        I_llvm_memset{..} -> aTvE [addLive,addConst, addAddrCaptured,addAddrStoringValue] dest
                             $ aTvE [addLive,addConst] setValue
                             f
        I_llvm_read_register{..} -> f                             
        I_llvm_write_register{..} -> f
        I_llvm_stacksave{..} -> f
        I_llvm_stackrestore{..} -> f
        I_llvm_libm_una{..} -> f
        I_llvm_libm_bin{..} -> f
        I_llvm_powi{..} -> f
        I_llvm_ctpop { dv = d } -> aTvE [addLive, addConst] d f
        I_llvm_lifetime_start { objsize = os, pointer = ptr } -> aTvE [addLive, addConst] os 
                                                                 $ aTvE [addLive, addConst] ptr f 
        I_llvm_lifetime_end { objsize = os, pointer = ptr } -> aTvE [addLive, addConst] os 
                                                               $ aTvE [addLive, addConst] ptr f         
        _ -> errorLoc FLC $ show n ++ " is not supported."
      Mnode _ _ -> f
#ifdef DEBUG
      _ -> errorLoc FLC $ show n
#endif
    getValuesFromParams :: [FunOperand (Value g)] -> S.Set (Value g)
    getValuesFromParams ls = foldl' (\p e -> case e of
                                                 FunOperandAsRet _ _ _ v -> S.insert v p
                                                 FunOperandData _ _ _ v -> S.insert v p
                                                 FunOperandByVal _ _ _ v -> S.insert v p
                                                 _ -> p
                                              ) S.empty ls

scanGraph :: (H.CheckpointMonad m, H.FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => S.Set LocalId 
             -> Label -> H.Graph (Node g a) H.C H.C -> m (DataUsage g)
scanGraph fm entry graph =
  do { (_, a, _) <- H.analyzeAndRewriteBwd (bwdScan fm) (H.JustC [entry]) graph H.mapEmpty
     ; let entryD = fromJust (H.lookupFact entry a)
     ; return (DataUsage { usageSummary = summary entryD
                         , livenessFact = mapMap liveness a 
                         }
              )
     }

scanDefine :: (CheckpointMonad m, FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => IrCxt g 
              -> TlDefine g a -> m (DataUsage g)
scanDefine s (TlDefine fn entry graph) = scanGraph formalParamIds entry graph
  where formalParamIds :: S.Set LocalId
        formalParamIds = let FunSignature { fs_params = r} = fi_signature fn
                         in foldl' (\p x -> case x of
                                      FunOperandAsRet (DtypeScalarP _)  _ _ v -> S.insert v p
                                      FunOperandData (DtypeScalarP _)  _ _ v -> S.insert v p
                                      FunOperandByVal (DtypeScalarP _)  _ _ v -> S.insert v p
                                      _ -> p
                                  ) S.empty r

scanModule :: (H.CheckpointMonad m, H.FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => 
              Module g a -> IrCxt g -> m (M.Map (FunctionInterface g) (DataUsage g))
scanModule (Module l) ic =
  do { l0 <- mapM (\x -> case x of
                      ToplevelDefine def@(TlDefine fn _ _) ->
                        do { fct <- scanDefine ic def
                           ; return (M.insert fn fct M.empty)
                           }
                      _ -> return M.empty
                  ) l
     ; return $ M.unions l0
     }
