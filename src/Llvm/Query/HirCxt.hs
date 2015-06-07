{-# LANGUAGE RecordWildCards, GADTs, TemplateHaskell,CPP #-}
module Llvm.Query.HirCxt where

#define FLC  (FileLoc $(srcLoc))

import qualified Llvm.Hir.Data as Ci
import qualified Data.Map as M
import qualified Data.Set as S
import Llvm.Hir.Data
import Llvm.Hir.Print
import qualified Compiler.Hoopl as H
import Llvm.Query.Conversion (strToApInt)
import Llvm.ErrorLoc

data TypeEnv = TypeEnv { dataLayout :: DataLayoutInfo
                       , targetTriple :: Ci.TargetTriple
                       , typedefs :: M.Map Ci.LocalId Ci.Dtype
                       , opaqueTypeDefs :: M.Map Ci.LocalId (Ci.Type OpaqueB D)
                       } deriving (Eq, Ord, Show)

data FunCxt = FunCxt { funInterface :: FunctionInterface
                     , dbgDeclares :: S.Set (Minst, [Dbg])
                     } deriving (Eq, Ord, Show)

data GlobalCxt = GlobalCxt { typeEnv :: TypeEnv
                           , globals :: M.Map Ci.GlobalId (TlGlobal, Ci.Dtype)
                           , functions :: M.Map Ci.GlobalId FunctionDeclare
                           , attributes :: M.Map Word32 [FunAttr]
                           , unamedMetadata :: M.Map Word32 TlUnamedMd
                           } deriving (Eq, Ord, Show)

data IrCxt = IrCxt { globalCxt :: GlobalCxt
                   , funCxt :: FunCxt
                   } deriving (Eq, Ord, Show)

instance IrPrint TypeEnv where
  printIr (TypeEnv dl tt td otd) = text "datalayout:" <+> printIr dl
                                   $+$ text "triple:" <+> printIr tt
                                   $+$ text "typedefs:" <+> printIr td
                                   $+$ text "opaqueTypedefs:" <+> printIr otd

instance IrPrint GlobalCxt where
  printIr (GlobalCxt te gl fns atts um) = text "typeEnv:" <+> printIr te
                                          $+$ text "globals:" <+> printIr gl
                                          $+$ text "functions:" <+> printIr fns
                                          $+$ text "attributes:" <+> printIr atts
                                          $+$ text "unamedMetadata:" <+> printIr um

instance IrPrint FunCxt where
  printIr (FunCxt fi dbgDeclares) = text "funInterface:" <+> printIr fi
                                    $+$ text "dbgDeclares:" <+> printIr dbgDeclares

instance IrPrint IrCxt where
  printIr (IrCxt g l) = text "globalCxt:" <+> printIr g $+$ text "funCxt:" <+> printIr l

convert_to_FunctionDeclareType :: FunctionInterface -> FunctionDeclare
convert_to_FunctionDeclareType
  FunctionInterface {..} =
    FunctionDeclare { fd_linkage = fi_linkage
                    , fd_visibility = fi_visibility
                    , fd_dllstorage = fi_dllstorage
                    , fd_call_conv = fi_call_conv
                    , fd_param_attrs = fi_param_attrs
                    , fd_ret_type = fi_ret_type
                    , fd_fun_name = fi_fun_name
                    , fd_param_list = convert_to_FormalParamTypeList fi_param_list
                    , fd_addr_naming = fi_addr_naming
                    , fd_fun_attrs = fi_fun_attrs
                    , fd_section = fi_section
                    , fd_comdat = fi_comdat
                    , fd_alignment = fi_alignment
                    , fd_gc = fi_gc
                    , fd_prefix = fi_prefix
                    , fd_prologue = fi_prologue
                    }

convert_to_FormalParamTypeList :: FunParamList -> FunParamTypeList
convert_to_FormalParamTypeList (FunParamList l ma fas) =
  FunParamTypeList (fmap convert_to_FormalParamType l) ma fas

convert_to_FormalParamType :: FunParam -> FunParamType
convert_to_FormalParamType x = case x of
  FunParamData dt pas ma _ -> FunParamDataType dt pas ma
  FunParamByVal dt pas ma _ -> FunParamByValType dt pas ma


irCxtOfModule :: Module a -> IrCxt
irCxtOfModule m = 
  IrCxt { globalCxt = globalCxtOfModule m
        , funCxt = FunCxt { funInterface = error "funInterface is not initialized." 
                          , dbgDeclares = error "dbgDeclare is not initialized."
                          }
        }


funCxtOfTlDefine :: TlDefine a -> FunCxt
funCxtOfTlDefine (TlDefine fi _ graph) = 
  let dbgs = H.foldGraphNodes fld graph S.empty
  in FunCxt { funInterface = fi
            , dbgDeclares = dbgs }
  where
    fld :: Node a e x -> S.Set (Minst, [Dbg]) -> S.Set (Minst, [Dbg])
    fld n = case n of
      Ci.Mnode m@(M_llvm_dbg_declare _ _) dbgs -> S.insert (m, dbgs) 
      _ -> id
      
      
globalCxtOfModule :: Module a -> GlobalCxt
globalCxtOfModule (Module tl) =
  let [ToplevelDataLayout (TlDataLayout dl)] = filter (\x -> case x of
                                                          ToplevelDataLayout _ -> True
                                                          _ -> False
                                                      ) tl
      [ToplevelTriple (TlTriple tt)] = filter (\x -> case x of
                                                  ToplevelTriple _ -> True
                                                  _ -> False
                                              ) tl
      tdefs = fmap (\(ToplevelTypeDef td) -> case td of
                       TlDatTypeDef lhs def -> (lhs, def))
              $ filter (\x -> case x of
                           ToplevelTypeDef (TlDatTypeDef _ _) -> True
                           _ -> False
                       ) tl
      glbs = fmap (\(ToplevelGlobal g@(TlGlobalDtype lhs _ _ _ _ _ _ _ _ t _ _ _ _)) -> (lhs, (g,t)))
             $ filter (\x -> case x of
                          ToplevelGlobal _ -> True
                          _ -> False
                      ) tl
      funs = fmap (\tl -> case tl of
                      ToplevelDeclare (TlDeclare fp@FunctionDeclare{..}) -> (fd_fun_name, fp)
                      ToplevelDefine (TlDefine fp@FunctionInterface{..} _ _) -> 
                        (fi_fun_name, convert_to_FunctionDeclareType fp))
             $ filter (\x -> case x of
                          ToplevelDeclare _ -> True
                          ToplevelDefine{..} -> True
                          _ -> False
                      ) tl
      attrs = fmap (\(ToplevelAttribute (TlAttribute n l)) -> (n, l))
              $ filter (\x -> case x of
                           ToplevelAttribute _ -> True
                           _ -> False
                       ) tl
      unameMeta = fmap (\(ToplevelUnamedMd um) -> case um of
                           TlUnamedMd n _ -> (n, um)
                           TlUnamedMd_DW_file_type n _ -> (n, um)
                           TlUnamedMd_DW_subprogram n _ -> (n, um)
                           _ -> errorLoc FLC $ show um)
                  $ filter (\x -> case x of
                               ToplevelUnamedMd _ -> True
                               _ -> False
                           ) tl
  in GlobalCxt { typeEnv = TypeEnv { dataLayout = getDataLayoutInfo dl
                                   , targetTriple = tt
                                   , typedefs = M.fromList tdefs
                                   , opaqueTypeDefs = M.empty
                                   }
               , globals = M.fromList glbs
               , functions = M.fromList funs
               , attributes = M.fromList attrs
               , unamedMetadata = M.fromList unameMeta
               }


data FileInfo = FileInfo { dir :: String
                         , file :: String
                         } deriving (Eq, Ord, Show)

instance IrPrint FileInfo where
  printIr (FileInfo d f) = text d <> text f

data PositionInfo = PositionInfo { line :: Word32
                                 , column :: Word32
                                 } deriving (Eq, Ord, Show)
                                            
instance IrPrint PositionInfo where                                            
  printIr (PositionInfo l c) = integer (fromIntegral l) <> colon <> integer (fromIntegral l)
                                            
data SrcInfo = SrcInfo { fileInfo :: FileInfo
                       , positionInfo :: Maybe PositionInfo
                       } deriving (Eq, Ord, Show)
                                  
instance IrPrint SrcInfo where                                  
  printIr (SrcInfo f p) = printIr f <> colon <> printIr p

srcInfoMap :: M.Map Word32 TlUnamedMd -> M.Map Word32 SrcInfo
srcInfoMap mdMap = foldl (\mp (w, um) -> maybe mp (\srcInfo -> M.insert w srcInfo mp) (getSrcInfo mdMap w)) M.empty (M.toList mdMap)

localIdSrcInfoMap :: M.Map Word32 TlUnamedMd -> S.Set (Minst, [Dbg]) -> M.Map LocalId SrcInfo
localIdSrcInfoMap mdMap set = 
  foldl (\mp (mi, dbgs) -> 
          case (mi, dbgs) of
            (M_llvm_dbg_declare (MetaOperandMeta m1) (MetaOperandMeta m2), [Dbg (MdRefName (MdName "dbg")) (McMdRef (MdRefNode (MdNode n)))]) -> 
              maybe mp (\(lid, srcInfo) -> M.insert lid srcInfo mp) 
              $ do { lid <- getLocalId m1
                   ; mf <- getMdRef m2
                   ; fref <- getFileRef mf
                   ; finfo <- getFileInfo mdMap fref 
                   ; (SrcInfo _ pos) <- getSrcInfo mdMap n
                   ; return (lid, SrcInfo finfo pos)
                   }
            (_,_) -> mp
        ) M.empty (S.toList set)
  where getLocalId m = case m of
          MetaKindedConst Mmetadata (McStruct [MetaKindedConst (Mtype _) (McSsa lid)]) -> Just lid
          _ -> Nothing
        getMdRef m = case m of
          MetaKindedConst Mmetadata (McMdRef (MdRefNode (MdNode mf))) -> Just mf
          _ -> Nothing
        getFileRef num = case M.lookup num mdMap of
          Just (TlUnamedMd _ (MetaKindedConst _ (McStruct [_,_,_,MetaKindedConst Mmetadata (McMdRef (MdRefNode (MdNode fref))),_,_,_,_]))) -> Just fref
          _ -> Nothing
         
getFileInfo :: M.Map Word32 TlUnamedMd -> Word32 -> Maybe FileInfo
getFileInfo mdMap fref = case M.lookup fref mdMap of
  Just (TlUnamedMd_DW_file_type _ (MetaKindedConst Mmetadata (McMdRef (MdRefNode (MdNode ref))))) -> getFileName mdMap ref
  Nothing -> errorLoc FLC $ show fref

getFileInfoFromSubprog :: M.Map Word32 TlUnamedMd -> Word32 -> Maybe FileInfo             
getFileInfoFromSubprog mdMap n = case M.lookup n mdMap of
  Just (TlUnamedMd_DW_subprogram _ [_,MetaKindedConst Mmetadata (McMdRef (MdRefNode (MdNode fref))),_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]) -> 
    getFileInfo mdMap fref
  _ -> Nothing

getSrcInfo :: M.Map Word32 TlUnamedMd -> Word32 -> Maybe SrcInfo
getSrcInfo mdMap n = case M.lookup n mdMap of
  Just (TlUnamedMd _ (MetaKindedConst _ (McStruct [MetaKindedConst (Mtype _) (McSimple (C_int lin))
                                                  ,MetaKindedConst (Mtype _) (McSimple (C_int col))
                                                  ,MetaKindedConst Mmetadata (McMdRef (MdRefNode (MdNode subprog))),UnmetaKindedNull]))) -> 
    do { fileInfo <- getFileInfoFromSubprog mdMap subprog
       ; return $ SrcInfo fileInfo (Just $ PositionInfo (fromIntegral $ strToApInt lin) (fromIntegral $ strToApInt col))
       }
  _ -> Nothing
  
getFileName :: M.Map Word32 TlUnamedMd -> Word32 -> Maybe FileInfo
getFileName mdMap ref = case M.lookup ref mdMap of
  Just (TlUnamedMd _ (MetaKindedConst Mmetadata (McStruct [MetaKindedConst Mmetadata (McString (DqString file))
                                                          ,MetaKindedConst Mmetadata (McString (DqString dir))]))) ->
    Just $ FileInfo dir file 
  _ -> Nothing