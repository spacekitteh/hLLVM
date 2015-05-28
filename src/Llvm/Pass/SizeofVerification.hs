{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards, TypeFamilies #-}

module Llvm.Pass.SizeofVerification where
import Data.Maybe
import qualified Data.Set as Ds
import qualified Data.Map as Dm
import qualified Data.List as L

import qualified Compiler.Hoopl as H
import Compiler.Hoopl
import Llvm.Hir.Data
import Llvm.Hir.Composer
import Llvm.Hir.Cast
import Llvm.Hir.Internalization
import Llvm.Query.HirCxt
import Llvm.Query.Conversion
import Llvm.Query.HirType
import Llvm.Hir.Print
import Control.Monad (liftM,foldM)
import Llvm.Query.HirCxt


{- 
This pass composes a standalone main function to check if hLLVM's type query 
implementation is equalivent to LLVM's implementation
-}

type Visualized = Ds.Set Dtype 


emptyVisualized :: Visualized
emptyVisualized = Ds.empty 

visLattice :: H.DataflowLattice Visualized
visLattice = H.DataflowLattice
              { H.fact_name = "TypeCheck Pass"
              , H.fact_bot = emptyVisualized
              , H.fact_join = add
              }
    where add _ (H.OldFact old) (H.NewFact new) = (ch, j)
            where
              j = Ds.union old new 
              ch = H.changeIf (j /= old)

bwdScan :: (Ord a, H.FuelMonad m) => TypeEnv -> H.BwdPass m (Node a) Visualized
bwdScan te = 
  let bwdTran :: (Node a) e x -> H.Fact x Visualized -> Visualized
      bwdTran n@(Tnode _ _) f = let bs = H.successors n
                                in foldl (\p l -> p `Ds.union` (fromMaybe emptyVisualized $ H.lookupFact l f)) 
                                   emptyVisualized bs
      bwdTran (Lnode _) f = f
      bwdTran (Pnode _ _) f = f
      bwdTran (Mnode _ _) f = f
      bwdTran (Comment _) f = f
      bwdTran (Enode _) f = f
      bwdTran (Cnode comp _) f = let (mtyp::Maybe Dtype) = typeof te comp
                                 in maybe f (flip Ds.insert f) mtyp 
  in H.BwdPass { H.bp_lattice = visLattice
               , H.bp_transfer = H.mkBTransfer bwdTran
               , H.bp_rewrite = H.noBwdRewrite
               }

scanDefine :: (CheckpointMonad m, FuelMonad m, Ord a) => TypeEnv -> TlDefine a -> m Visualized
scanDefine te (TlDefine fn entry graph) = 
  do { (_, a, b) <- H.analyzeAndRewriteBwd (bwdScan te) (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyVisualized (H.lookupFact entry a))
     }
  
scanModule :: (CheckpointMonad m, FuelMonad m, Ord a) => Module a -> m Visualized
scanModule m@(Module l) = 
  let IrCxt{..} = irCxtOfModule m
  in do { l0 <- mapM (\x -> case x of
                         ToplevelGlobal tglb -> case tglb of
                           TlGlobalDtype{..} -> return $ Ds.insert tlg_dtype Ds.empty
                           _ -> return Ds.empty
                         ToplevelTypeDef tdef -> case tdef of
                           TlDatTypeDef _ t -> return $ Ds.insert t Ds.empty
                           _ -> return Ds.empty
                         ToplevelDefine def ->
                           do { fct <- scanDefine (typeEnv globalCxt) def
                              ; return fct
                              }
                         _ -> return Ds.empty 
                     ) l
        ; return (Ds.unions l0)
        }


type VisualIds = Dm.Map Dtype Int
                    
allocateVisualIds :: Visualized -> VisualIds
allocateVisualIds mp = 
  let sl = Ds.toList mp
  in fst $ foldl (\(p,idx) e -> (Dm.insert e idx p, idx+1)) (Dm.empty,0) sl

stringize :: Dm.Map Dtype GlobalId -> ([Toplevel a], Dm.Map Dtype Const)
stringize mp = 
  let mp0 = Dm.mapWithKey (\c lhs -> internalize (lhs,render $ printIr c)) mp
  in (fmap ToplevelGlobal $ Dm.elems $ Dm.map llvmDef mp0, Dm.map llvmRef mp0)


mkVerificationModule :: Ord a => Module a -> Module a
mkVerificationModule m@(Module l) = 
  let IrCxt{..} = irCxtOfModule m
      vis = H.runSimpleUniqueMonad $ H.runWithFuel H.infiniteFuel ((scanModule m):: H.SimpleFuelMonad Visualized)
      (globals, duC) = stringize (Dm.map (\x -> GlobalIdAlphaNum $ ".visual_" ++ show x) $ allocateVisualIds vis)
      dataLayoutAndTriple = getDataLayoutAndTriple m
      insts = fmap (mkCheck (typeEnv globalCxt) duC)
              $ (filter (\x -> case x  of
                            DtypeScalarI (TpI 1) -> False {- bool type is boring -}
                            DtypeScalarP _ -> False {- pointer type is boring -}
                            _ -> True
                        )
                ) (Ds.toList vis)
  in Module (dataLayoutAndTriple  
             ++ globals
             ++ (fmap (ToplevelDeclare . TlDeclare) visFunctions)
             ++ [ToplevelDefine $ defineMain $ concat insts])


getDataLayoutAndTriple :: Module a -> [Toplevel a]
getDataLayoutAndTriple (Module l) = filter (\x -> case x of
                                               ToplevelTriple _ -> True
                                               ToplevelDataLayout _ -> True
                                               ToplevelTypeDef _ -> True
                                               _ -> False) l


mkCheck :: TypeEnv -> Dm.Map Dtype Const -> Dtype -> [Node a O O]
mkCheck te mp dt = [ Comment $ Cstring $ render $ printIr dt
                   , Comment $ Cstring $ show dt
                   , Comment $ Cstring $ "SizeInBits:" ++ show (getTypeSizeInBits te dt)
                   , Comment $ Cstring $ "TypeStoreSize:" ++ show (getTypeStoreSize te dt)
                   , Comment $ Cstring $ "Alignment:" ++ show (getTypeAlignment te dt AlignAbi) 
                   , callLog [T (ucast $ ptr0 i8) (ucast $ fromJust $ Dm.lookup dt mp),
                              ucast (T i32 (llvm_sizeof dt i32)), ucast $ toTC (sizeof te dt)]
                   ]


callLog :: [T Dtype Value] -> (Node a) O O
callLog tvs = 
  let aps = fmap (\(T t v) -> ActualParamData t [] Nothing v) tvs
      callSiteType = CallSiteTypeFun (Tfunction (RtypeVoidU Tvoid) (TypeParamList [ucast $ ptr0 i8, ucast i32, ucast i32] Nothing) []) 0
  in Cnode (I_call_fun (FunId (GlobalIdAlphaNum "check_int2")) 
            (CallFunInterface TcNon Ccc [] callSiteType  Nothing aps []) Nothing) []


visFunctions = [FunctionDeclare { fd_linkage = Nothing
                                , fd_visibility = Nothing
                                , fd_dllstorage = Nothing
                                , fd_call_conv = Nothing
                                , fd_param_attrs = []
                                , fd_ret_type = RtypeVoidU Tvoid
                                , fd_fun_name = GlobalIdAlphaNum "check_int2"
                                , fd_param_list = (FunParamTypeList [FunParamDataType (ucast $ ptr0 i8) [] Nothing 
                                                                    ,FunParamDataType (ucast i32) [] Nothing
                                                                    ,FunParamDataType (ucast i32) [] Nothing
                                                                    ] 
                                                   Nothing [])
                                , fd_addr_naming = Nothing
                                , fd_fun_attrs = []
                                , fd_section = Nothing
                                , fd_comdat = Nothing
                                , fd_alignment = Nothing
                                , fd_gc = Nothing
                                , fd_prefix = Nothing
                                , fd_prologue = Nothing
                                }
               ]


defineMain :: [Node a O O] -> TlDefine a
defineMain insts = let (entry, graph) = H.runSimpleUniqueMonad (composeGraph insts (T_return [ucast $ u32ToTv 0]))
                   in TlDefine mainFp entry graph
  where mainFp :: FunctionInterface 
        mainFp = FunctionInterface { fi_linkage = Nothing
                                   , fi_visibility = Nothing
                                   , fi_dllstorage = Nothing
                                   , fi_call_conv = Nothing
                                   , fi_param_attrs = []
                                   , fi_ret_type = RtypeScalarI (TpI 32)
                                   , fi_fun_name = GlobalIdAlphaNum "main"
                                   , fi_param_list = FunParamList [] Nothing []
                                   , fi_addr_naming = Nothing
                                   , fi_fun_attrs = []
                                   , fi_section = Nothing
                                   , fi_comdat = Nothing
                                   , fi_alignment = Nothing
                                   , fi_gc = Nothing
                                   , fi_prefix = Nothing
                                   , fi_prologue = Nothing
                                   }
        composeGraph :: H.UniqueMonad m => [Node a O O] -> Tinst -> m (H.Label, H.Graph (Node a) C C)
        composeGraph insts ret = do { lbl <- H.freshLabel
                                    ; let graph = mkFirst (Lnode lbl) 
                                                  H.<*> mkMiddles insts
                                                  H.<*> mkLast (Tnode ret [])
                                    ; return (lbl, graph)
                                    }

