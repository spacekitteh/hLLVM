{-# LANGUAGE ScopedTypeVariables, GADTs, RecordWildCards, TypeFamilies, TupleSections, RankNTypes #-}

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
import Llvm.Query.Type
import Llvm.Hir.Print
import Control.Monad (liftM,foldM)
import Llvm.Query.HirCxt
import Llvm.Hir.DataLayoutMetrics

{- 
This pass composes a standalone main function to check if hLLVM's type query 
implementation is equalivent to LLVM's implementation
-}

{- this should be simplified as just a graph folding pass -}
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

bwdScan :: forall g.forall m.forall a.(Ord a, Show g, Eq g, H.FuelMonad m) => TypeEnv -> H.BwdPass m (Node g a) Visualized
bwdScan te = 
  let bwdTran :: (Node g a) e x -> H.Fact x Visualized -> Visualized
      bwdTran n@(Tnode _ _) f = let bs = H.successors n
                                in foldl (\p l -> p `Ds.union` (fromMaybe emptyVisualized $ H.lookupFact l f)) 
                                   emptyVisualized bs
      bwdTran (Lnode _) f = f
      bwdTran (Pnode _ _) f = f
      bwdTran (Mnode _ _) f = f
      bwdTran (Comment _) f = f
      bwdTran (Enode _ _) f = f
      bwdTran (Cnode comp _) f = let (mtyp::Maybe Dtype) = typeof te comp
                                 in maybe f (flip Ds.insert f) mtyp 
  in H.BwdPass { H.bp_lattice = visLattice
               , H.bp_transfer = H.mkBTransfer bwdTran
               , H.bp_rewrite = H.noBwdRewrite
               }

scanDefine :: (CheckpointMonad m, FuelMonad m, Ord g, Show g, Ord a) => TypeEnv -> TlDefine g a -> m Visualized
scanDefine te (TlDefine fn entry graph) = 
  do { (_, a, b) <- H.analyzeAndRewriteBwd (bwdScan te) (H.JustC [entry]) graph H.mapEmpty
     ; return (fromMaybe emptyVisualized (H.lookupFact entry a))
     }
  
scanModule :: (CheckpointMonad m, FuelMonad m, Ord g, Show g, Ord a) => Module g a -> m (Ds.Set Dtype)
scanModule m@(Module l) = 
  let IrCxt{..} = irCxtOfModule m
  in foldM (\p x -> case x of
               ToplevelGlobal tglb -> case tglb of
                 TlGlobalDtype{..} -> return $ Ds.insert tlg_dtype p
                 _ -> return p
               ToplevelTypeDef tdef -> case tdef of
                 TlDatTypeDef _ t -> return $ Ds.insert t p
                 _ -> return p
               ToplevelDefine def ->
                 do { fct <- scanDefine (typeEnv globalCxt) def
                    ; return $ Ds.union fct p
                    }
               _ -> return p 
           ) Ds.empty l


stringize :: Ord g => Ds.Set Dtype -> ([Toplevel g a], Dm.Map Dtype (Const g))
stringize mp = 
  let (kvs, tpls) = runSimpleLlvmGlobalGen ".sizeof_" 0 
                    (mapM (\c -> do { (DefAndRef _ (T _ c0)) <- internalize (render $ printIr c)
                                    ; return (c, c0) 
                                    }) $ Ds.toList mp)
  in (Dm.elems $ Dm.map llvmDef tpls, Dm.fromList kvs)
     

mkVerificationModule :: (DataLayoutMetrics dlm, Ord g, Show g, Ord a) => SpecializedModule dlm g a -> SpecializedModule dlm g a
mkVerificationModule (SpecializedModule dlm m@(Module l)) = 
  let IrCxt{..} = irCxtOfModule m
      vis = H.runSimpleUniqueMonad $ H.runWithFuel H.infiniteFuel ((scanModule m):: H.SimpleFuelMonad Visualized)
      (globals, duC) = stringize vis
      typeDefs = filter (\x -> case x of
                            ToplevelTypeDef _ -> True
                            _ -> False) l
      insts = fmap (mkCheck dlm (typeEnv globalCxt) duC)
              $ (filter (\x -> case x  of
                            DtypeScalarP _ -> False {- pointer type is boring -}
                            _ -> True
                        )
                ) (Ds.toList vis)
  in SpecializedModule dlm (Module (typeDefs ++ globals
                                    ++ (fmap (ToplevelDeclare . TlDeclare) visFunctions)
                                    ++ [ToplevelDefine $ defineMain $ concat insts]))

mkCheck :: forall dlm.forall g.forall a.DataLayoutMetrics dlm => dlm -> TypeEnv -> Dm.Map Dtype (Const g) -> Dtype -> [Node g a O O]
mkCheck dlm te mp dt = [ Comment $ Cstring $ render $ printIr dt
                       , Comment $ Cstring $ show dt
                       , Comment $ Cstring $ "SizeInBits:" ++ show (getTypeSizeInBits dlm te dt)
                       , Comment $ Cstring $ "TypeStoreSize:" ++ show (getTypeStoreSize dlm te dt)
                       , Comment $ Cstring $ "Alignment:" ++ show (getTypeAlignment dlm te dt AlignAbi) 
                       , callLog [T (ucast $ ptr0 i8) (ucast $ fromJust $ Dm.lookup dt mp)
                                 , ucast (T i64 ((llvm_sizeof dt i64)::Const g))
                                 , ucast ((toTC (sizeof dlm te dt))::T (Type ScalarB I) (Const g))
                                 ]
                       ]


callLog :: [T Dtype (Value g)] -> (Node g a) O O
callLog tvs = 
  let aps = fmap (\(T t v) -> FunOperandData t [] Nothing v) tvs
      callSiteType = Tfunction (RtypeVoidU Tvoid, []) [(MtypeData (ucast $ ptr0 i8), Nothing)
                                                      ,(MtypeData (ucast i64), Nothing)
                                                      ,(MtypeData (ucast i64), Nothing)] Nothing
  in Cnode (I_call_fun (FunId (GlobalIdAlphaNum "check_int2"))
            CallFunInterface { cfi_tail = TcNon 
                             , cfi_castType = Nothing
                             , cfi_signature = FunSignature Ccc callSiteType aps 
                             , cfi_funAttrs = [] 
                             } Nothing) []

visFunctions = [FunctionDeclareData { fd_linkage = Nothing
                                    , fd_visibility = Nothing
                                    , fd_dllstorage = Nothing
                                    , fd_signature = FunSignature { fs_callConv = Ccc
                                                                  , fs_type = Tfunction (RtypeVoidU Tvoid,[]) [(MtypeData $ ucast $ ptr0 i8, Nothing)
                                                                                                              ,(MtypeData $ ucast i64, Nothing)
                                                                                                              ,(MtypeData $ ucast i64, Nothing)] Nothing
                                                                  , fs_params = [FunOperandData (ucast $ ptr0 i8) [] Nothing ()
                                                                                ,FunOperandData (ucast i64) [] Nothing ()
                                                                                ,FunOperandData (ucast i64) [] Nothing ()
                                                                                ] 
                                                                  }
                                    , fd_fun_name = GlobalIdAlphaNum "check_int2"
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


defineMain :: forall g.forall a.[Node g a O O] -> TlDefine g a
defineMain insts = let (entry, graph) = H.runSimpleUniqueMonad (composeGraph insts (T_return [ucast ((u32ToTv 0)::T (Type ScalarB I) (Value g))]))
                   in TlDefine mainFp entry graph
  where mainFp :: FunctionInterface g
        mainFp = FunctionInterface { fi_linkage = Nothing
                                   , fi_visibility = Nothing
                                   , fi_dllstorage = Nothing
                                   , fi_signature = FunSignature { fs_callConv = Ccc
                                                                 , fs_type = Tfunction (RtypeScalarI (TpI 32), []) [] Nothing
                                                                 , fs_params = []
                                                                 }
                                   , fi_fun_name = GlobalIdAlphaNum "main"
                                   , fi_addr_naming = Nothing
                                   , fi_fun_attrs = []
                                   , fi_section = Nothing
                                   , fi_comdat = Nothing
                                   , fi_alignment = Nothing
                                   , fi_gc = Nothing
                                   , fi_prefix = Nothing
                                   , fi_prologue = Nothing
                                   }
        composeGraph :: H.UniqueMonad m => [Node g a O O] -> Tinst g -> m (H.Label, H.Graph (Node g a) C C)
        composeGraph insts ret = do { lbl <- H.freshLabel
                                    ; let graph = mkFirst (Lnode lbl) 
                                                  H.<*> mkMiddles insts
                                                  H.<*> mkLast (Tnode ret [])
                                    ; return (lbl, graph)
                                    }

