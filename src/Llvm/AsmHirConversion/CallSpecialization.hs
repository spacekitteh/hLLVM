{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RecordWildCards #-}
module Llvm.AsmHirConversion.CallSpecialization where

import Llvm.Hir
import Llvm.ErrorLoc
import qualified Llvm.Asm.Data as A


#define FLC  (FileLoc $(srcLoc))

specializeRetAttr :: ParamAttr -> RetAttr    
specializeRetAttr x = case x of
  PaZeroExt -> RetAttrZeroExt
  PaSignExt -> RetAttrSignExt
  PaInReg -> RetAttrInReg
  PaNoAlias -> RetAttrNoAlias
  PaDereferenceable n -> RetAttrDereferenceable n
  _ -> errorLoc FLC $ show x

unspecializeRetAttr :: RetAttr -> ParamAttr  
unspecializeRetAttr x = case x of
  RetAttrZeroExt -> PaZeroExt
  RetAttrSignExt -> PaSignExt
  RetAttrInReg -> PaInReg
  RetAttrNoAlias -> PaNoAlias
  RetAttrDereferenceable n -> PaDereferenceable n


specializeParamAsRetAttr :: ParamAttr -> Maybe ParamAsRetAttr
specializeParamAsRetAttr x = case x of
  PaSRet -> Just ParamAsRetAttr
  _ -> Nothing


unspecializeParamAsRetAttr :: ParamAsRetAttr -> ParamAttr
unspecializeParamAsRetAttr x = case x of
  ParamAsRetAttr -> PaSRet


