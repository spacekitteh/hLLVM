{-# LANGUAGE CPP, ScopedTypeVariables, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses #-}

module Llvm.Pass.Liveness where
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

data DataUsage g = DataUsage { -- | The addresses that store function parameters that are pointers
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
                           , callAsmInfoSet :: S.Set (CallAsmInterface g)
                           , lives :: S.Set LocalId
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

addAddr :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddr v du@DataUsage{..} =
  du { addrs = S.insert v addrs}

addStackAddr :: LocalId -> DataUsage g -> DataUsage g
addStackAddr v du@DataUsage{..} =
  du { stack_addrs = S.insert v stack_addrs }


addAddrInvolvingPtrArithm :: Ord g => Value g -> DataUsage g -> DataUsage g
addAddrInvolvingPtrArithm v du@DataUsage{..} =
  du { addrs_involving_pointer_arithmatic = S.insert v addrs_involving_pointer_arithmatic }

addStackAddrInvolvingPtrArithm :: LocalId -> DataUsage g -> DataUsage g
addStackAddrInvolvingPtrArithm v du@DataUsage{..} =
  du { stack_addrs_involving_pointer_arithmatic = S.insert v stack_addrs_involving_pointer_arithmatic }

addConst :: Ord g => Value g -> DataUsage g -> DataUsage g
addConst (Val_const c) du@DataUsage{..} = du { constants = S.insert c constants }
addConst (Val_ssa _) du = du

addLive :: Ord g => Value g -> DataUsage g -> DataUsage g
addLive (Val_ssa lid) du@DataUsage{..} = du { lives = S.insert lid lives }
addLive _ du = du

killLive :: LocalId -> DataUsage g -> DataUsage g
killLive lid du@DataUsage{..} = du { lives = S.delete lid lives }

addMaybeConst :: Ord g => Maybe (Value g) -> DataUsage g -> DataUsage g
addMaybeConst (Just c) du = addConst c du
addMaybeConst Nothing du = du

aTv :: (Value g -> DataUsage g -> DataUsage g) -> T t (Value g) -> DataUsage g -> DataUsage g
aTv  f (T _ v) du = f v du

aTvs :: (Value g -> DataUsage g -> DataUsage g) -> [T t (Value g)] -> DataUsage g -> DataUsage g
aTvs f l du = foldl (\p e -> aTv f e p) du l

aTvE :: [Value g -> DataUsage g -> DataUsage g] -> T t (Value g) -> DataUsage g -> DataUsage g
aTvE  fs tv du = foldl (\p f -> aTv f tv p) du fs

aTvsE :: [Value g -> DataUsage g -> DataUsage g] -> [T t (Value g)] -> DataUsage g -> DataUsage g
aTvsE fs l du = foldl (\p e -> foldl (\pp f -> aTv f e pp) p fs) du l


foldlvs :: (Value g -> DataUsage g -> DataUsage g) -> [Value g] -> DataUsage g -> DataUsage g
foldlvs  f vs du = foldl (\p e -> f e p) du  vs

foldlvsE :: [Value g -> DataUsage g -> DataUsage g] -> [Value g] -> DataUsage g -> DataUsage g
foldlvsE  fs vs du = foldl (\du0 f -> foldl (\p e -> f e p) du0 vs) du fs

foldlE :: [Value g -> DataUsage g -> DataUsage g] -> Value g -> DataUsage g -> DataUsage g
foldlE fs v du = foldl (\du0 f -> f v du0) du fs



applyToMaybe :: (a -> DataUsage g -> DataUsage g) -> Maybe a -> DataUsage g -> DataUsage g
applyToMaybe f (Just x) du = f x du
applyToMaybe f Nothing du = du

applyToEither :: (a -> DataUsage g -> DataUsage g) -> (b -> DataUsage g -> DataUsage g) 
                 -> Either a b -> DataUsage g -> DataUsage g
applyToEither fl fr (Left x) du = fl x du
applyToEither fl fr (Right x) du = fr x du

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
  $ bubbleUp2 (Val_ssa dest) addrs src addAddr
  du

emptyDataUsage :: DataUsage g
emptyDataUsage =
  DataUsage 
  S.empty S.empty S.empty S.empty S.empty
  S.empty S.empty S.empty S.empty S.empty
  S.empty S.empty S.empty S.empty S.empty 
  S.empty S.empty S.empty S.empty

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
    $+$ text "addrs:" <+> printIr addrs
    $+$ text "stack_addrs:" <+> printIr stack_addrs
    $+$ text "callInfoSet:" <+> printIr callFunInfoSet
    $+$ text "callAsmSet:" <+> printIr callAsmInfoSet
    $+$ text "lives:" <+> printIr lives

usageLattice :: Ord g => H.DataflowLattice (DataUsage g)
usageLattice = H.DataflowLattice
              { H.fact_name = "Data and Ssa variable Usage"
              , H.fact_bot = emptyDataUsage
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = 
            if old == new then
              (NoChange, old)
            else 
              (SomeChange, j)
            where
              j = unionDataUsage old new
              -- ch = H.changeIf (j /= old)

unionDataUsage :: Ord g => DataUsage g -> DataUsage g -> DataUsage g
unionDataUsage (DataUsage s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19)
  (DataUsage t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
  = DataUsage
    (s1 `lbUnion` t1)   (s2 `lbUnion` t2)   (s3 `lbUnion` t3)
    (s4 `lbUnion` t4)   (s5 `lbUnion` t5)   (s6 `lbUnion` t6)
    (s7 `lbUnion` t7)   (s8 `lbUnion` t8)   (s9 `lbUnion` t9)
    (s10 `lbUnion` t10) (s11 `lbUnion` t11) (s12 `lbUnion` t12)
    (s13 `lbUnion` t13) (s14 `lbUnion` t14) (s15 `lbUnion` t15)
    (s16 `lbUnion` t16) (s17 `lbUnion` t17) (s18 `lbUnion` t18) 
    (s19 `lbUnion` t19)
{- hopefully, this can improve the sharing of sets -}
lbUnion :: Ord v => S.Set v -> S.Set v -> S.Set v
lbUnion left right = S.foldl (\p e -> if S.member e p then 
                                        p
                                      else 
                                        S.insert e p
                             ) left right

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
        let f0 = foldl (\p l -> p `unionDataUsage` (fromMaybe emptyDataUsage $ H.lookupFact l f)) 
                 emptyDataUsage (H.successors n)
        in case tinst of
          T_ret_void -> f0
          T_return vs -> aTvsE [addConst,addLive] vs f0
          T_invoke{..} ->
            let vals = getValuesFromParams (fs_params $ ifi_signature invoke_fun_interface)
            in applyToMaybe killLive invoke_return
               $ foldlvsE [addConst, addAddrCaptured, addAddrStoringPtr, addAddrStoringValue, addLive] 
               (S.toList vals)
               (f0 { invokeFunInfoSet = S.insert (invoke_ptr, invoke_fun_interface) (invokeFunInfoSet f0) }) 
          T_invoke_asm{..} ->
            let vals = getValuesFromParams (cai_actualParams invoke_asm_interface)
            in applyToMaybe killLive invoke_return
               $ foldlvsE [addConst, addAddrCaptured, addAddrStoringPtr, addAddrStoringValue, addLive]
               (S.toList vals) 
               (f0 { callAsmInfoSet = S.insert invoke_asm_interface (callAsmInfoSet f0)})
          _ -> f0
      Lnode _ -> f
      Pnode (Pinst{..}) _ -> 
        killLive flowout
        $ foldl (\p (e,_) -> foldlE [addLive,addConst,propogateUpPtrUsage flowout] e p) 
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
             (f { callFunInfoSet = S.insert (call_ptr, call_fun_interface) (callFunInfoSet f) })
        I_call_asm{..} ->
          let vals = getValuesFromParams (cai_actualParams call_asm_interface)
          in applyToMaybe killLive call_return
             $ foldlvsE [addConst, addAddrCaptured, addAddrStoringPtr, addAddrStoringValue, addLive]
             (S.toList vals)
             (f { callAsmInfoSet = S.insert call_asm_interface (callAsmInfoSet f)})
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
             -> Label -> H.Graph (Node g a) H.C H.C -> m (LabelMap (DataUsage g))
scanGraph fm entry graph =
  do { (_, a, _) <- H.analyzeAndRewriteBwd (bwdScan fm) (H.JustC [entry]) graph H.mapEmpty
     ; return a
     }

scanDefine :: (CheckpointMonad m, FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => IrCxt g 
              -> TlDefine g a -> m (LabelMap (DataUsage g))
scanDefine s (TlDefine fn entry graph) = scanGraph formalParamIds entry graph
  where formalParamIds :: S.Set LocalId
        formalParamIds = let FunSignature { fs_params = r} = fi_signature fn
                         in foldl (\p x -> case x of
                                      FunOperandAsRet (DtypeScalarP _)  _ _ v -> S.insert v p
                                      FunOperandData (DtypeScalarP _)  _ _ v -> S.insert v p
                                      FunOperandByVal (DtypeScalarP _)  _ _ v -> S.insert v p
                                      _ -> p
                                  ) S.empty r

scanModule :: (H.CheckpointMonad m, H.FuelMonad m, Show g, Ord g, Show a, DataUsageUpdator g a) => 
              Module g a -> IrCxt g -> m (M.Map (FunctionInterface g) (LabelMap (DataUsage g)))
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
