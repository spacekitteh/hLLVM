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

data TypeEnv = TypeEnv {typedefs :: M.Map Ci.LocalId Ci.Dtype
                       , opaqueTypeDefs :: M.Map Ci.LocalId (Ci.Type OpaqueB D)
                       } deriving (Eq, Ord, Show)

data FunCxt g = FunCxt { funInterface :: FunctionInterface g
                       , dbgDeclares :: S.Set (Minst g, [Dbg g])
                       } deriving (Eq, Ord, Show)

data GlobalCxt g = GlobalCxt { typeEnv :: TypeEnv
                             , globals :: M.Map g (TlGlobal g, Ci.Dtype)
                             , functions :: M.Map g (FunctionDeclare g)
                             , alias :: M.Map g (TlAlias g)
                             , attributes :: M.Map Word32 [FunAttr]
                             , unamedMetadata :: M.Map Word32 (TlUnamedMd g)
                             } deriving (Eq, Ord, Show)

data IrCxt g = IrCxt { globalCxt :: GlobalCxt g
                     , funCxt :: FunCxt g
                     } deriving (Eq, Ord, Show)

instance IrPrint TypeEnv where
  printIr (TypeEnv td otd) = text "typedefs:" <+> printIr td
                             $+$ text "opaqueTypedefs:" <+> printIr otd

instance IrPrint g => IrPrint (GlobalCxt g) where
  printIr (GlobalCxt te gl fns alias atts um) = text "typeEnv:" <+> printIr te
                                                $+$ text "globals:" <+> printIr gl
                                                $+$ text "functions:" <+> printIr fns
                                                $+$ text "alias:" <+> printIr alias
                                                $+$ text "attributes:" <+> printIr atts
                                                $+$ text "unamedMetadata:" <+> printIr um

instance IrPrint g => IrPrint (FunCxt g) where
  printIr (FunCxt fi dbgDeclares) = text "funInterface:" <+> printIr fi
                                    $+$ text "dbgDeclares:" <+> printIr dbgDeclares

instance IrPrint g => IrPrint (IrCxt g) where
  printIr (IrCxt g l) = text "globalCxt:" <+> printIr g $+$ text "funCxt:" <+> printIr l

convert_to_FunctionDeclareType :: FunctionInterface g -> FunctionDeclare g
convert_to_FunctionDeclareType
  FunctionInterface {..} =
    FunctionDeclareData { fd_linkage = fi_linkage
                        , fd_visibility = fi_visibility
                        , fd_dllstorage = fi_dllstorage
                        , fd_fun_name = fi_fun_name
                        , fd_signature = convert_to_FormalParamTypeList fi_signature
                        , fd_addr_naming = fi_addr_naming
                        , fd_fun_attrs = fi_fun_attrs
                        , fd_section = fi_section
                        , fd_comdat = fi_comdat
                        , fd_alignment = fi_alignment
                        , fd_gc = fi_gc
                        , fd_prefix = fi_prefix
                        , fd_prologue = fi_prologue
                        }

convert_to_FormalParamTypeList :: FunSignature LocalId -> FunSignature ()
convert_to_FormalParamTypeList FunSignature{..} = 
  FunSignature fs_callConv fs_type (fmap convert_to_FormalParamType fs_params)


convert_to_FormalParamType :: FunOperand LocalId -> FunOperand ()
convert_to_FormalParamType x = case x of
  FunOperandData dt pas ma _ -> FunOperandData dt pas ma ()
  FunOperandByVal dt pas ma _ -> FunOperandByVal dt pas ma ()
  FunOperandAsRet dt pa ma _ -> FunOperandAsRet dt pa ma ()


irCxtOfModule :: (Show g, Ord g) => Module g a -> IrCxt g
irCxtOfModule m = 
  IrCxt { globalCxt = globalCxtOfModule m
        , funCxt = FunCxt { funInterface = error "funInterface is not initialized." 
                          , dbgDeclares = error "dbgDeclare is not initialized."
                          }
        }


funCxtOfTlDefine :: Ord g => TlDefine g a -> FunCxt g
funCxtOfTlDefine (TlDefine fi _ graph) = 
  let dbgs = H.foldGraphNodes fld graph S.empty
  in FunCxt { funInterface = fi
            , dbgDeclares = dbgs 
            }
  where
    fld :: Ord g => Node g a e x -> S.Set (Minst g, [Dbg g]) -> S.Set (Minst g, [Dbg g])
    fld n = case n of
      Ci.Mnode m@(M_llvm_dbg_declare _ _) dbgs -> S.insert (m, dbgs) 
      _ -> id
      
      
globalCxtOfModule :: (Show g, Ord g) => Module g a -> GlobalCxt g
globalCxtOfModule (Module tl) =
  let tdefs = fmap (\(ToplevelTypeDef td) -> case td of
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
                      ToplevelDeclare (TlDeclare fp@FunctionDeclareData{..}) -> (fd_fun_name, fp)
                      ToplevelDefine (TlDefine fp@FunctionInterface{..} _ _) -> 
                        (fi_fun_name, convert_to_FunctionDeclareType fp))
             $ filter (\x -> case x of
                          ToplevelDeclare (TlDeclare FunctionDeclareData{..}) -> True
                          ToplevelDefine{..} -> True
                          _ -> False
                      ) tl
      alia = fmap (\(ToplevelAlias x) -> (tla_lhs x, x))
             $ filter (\x -> case x of
                          ToplevelAlias _ -> True
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
                           TlUnamedMd_DW_lexical_block n _ -> (n, um)
                           _ -> errorLoc FLC $ show um)
                  $ filter (\x -> case x of
                               ToplevelUnamedMd _ -> True
                               _ -> False
                           ) tl
  in GlobalCxt { typeEnv = TypeEnv { typedefs = M.fromList tdefs
                                   , opaqueTypeDefs = M.empty
                                   }
               , globals = M.fromList glbs
               , functions = M.fromList funs
               , alias = M.fromList alia
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

srcInfoMap :: Show g => M.Map Word32 (TlUnamedMd g) -> M.Map Word32 SrcInfo
srcInfoMap mdMap = foldl (\mp (w, um) -> maybe mp (\srcInfo -> M.insert w srcInfo mp) (getSrcInfo mdMap w)) M.empty (M.toList mdMap)

localIdSrcInfoMap :: Show g => M.Map Word32 (TlUnamedMd g) -> S.Set (Minst g, [Dbg g]) -> M.Map LocalId SrcInfo
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
          MetaKindedConst MKmetadata (McStruct [MetaKindedConst (MKtype _) (McSsa lid)]) -> Just lid
          _ -> Nothing
        getMdRef m = case m of
          MetaKindedConst MKmetadata (McMdRef (MdRefNode (MdNode mf))) -> Just mf
          _ -> Nothing
        getFileRef num = case M.lookup num mdMap of
          Just (TlUnamedMd _ (MetaKindedConst _ (McStruct [_,_,_,MetaKindedConst MKmetadata (McMdRef (MdRefNode (MdNode fref))),_,_,_,_]))) -> Just fref
          _ -> Nothing
         
getFileInfo :: Show g => M.Map Word32 (TlUnamedMd g) -> Word32 -> Maybe FileInfo
getFileInfo mdMap fref = case M.lookup fref mdMap of
  Just (TlUnamedMd_DW_file_type _ x) -> case x of
    MetaKindedConst MKmetadata (McMdRef (MdRefNode (MdNode ref))) -> getFileName mdMap ref
    _ -> errorLoc FLC $ show x
  y -> errorLoc FLC $ show y

getFileInfoFromSubprog :: Show g => M.Map Word32 (TlUnamedMd g) -> Word32 -> Maybe FileInfo
getFileInfoFromSubprog mdMap n = case M.lookup n mdMap of
  Just (TlUnamedMd_DW_subprogram _ [_,MetaKindedConst MKmetadata (McMdRef (MdRefNode (MdNode fref))),_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]) -> 
    getFileInfo mdMap fref
  Just (TlUnamedMd_DW_lexical_block _ [MetaKindedConst MKmetadata (McMdRef (MdRefNode (MdNode fref))),_,_,_,_,_]) ->
    getFileName mdMap fref
  _ -> Nothing

getSrcInfo :: Show g => M.Map Word32 (TlUnamedMd g) -> Word32 -> Maybe SrcInfo
getSrcInfo mdMap n = case M.lookup n mdMap of
  Just (TlUnamedMd _ (MetaKindedConst _ (McStruct [MetaKindedConst (MKtype _) (McSimple (C_int lin))
                                                  ,MetaKindedConst (MKtype _) (McSimple (C_int col))
                                                  ,MetaKindedConst MKmetadata (McMdRef (MdRefNode (MdNode subprog))),UnmetaKindedNull]))) -> 
    do { fileInfo <- getFileInfoFromSubprog mdMap subprog
       ; return $ SrcInfo fileInfo (Just $ PositionInfo (fromIntegral $ strToApInt lin) (fromIntegral $ strToApInt col))
       }
  _ -> Nothing
  
getFileName :: M.Map Word32 (TlUnamedMd g) -> Word32 -> Maybe FileInfo
getFileName mdMap ref = case M.lookup ref mdMap of
  Just (TlUnamedMd _ (MetaKindedConst MKmetadata (McStruct [MetaKindedConst MKmetadata (McString (DqString file))
                                                           ,MetaKindedConst MKmetadata (McString (DqString dir))]))) ->
    Just $ FileInfo dir file 
  _ -> Nothing