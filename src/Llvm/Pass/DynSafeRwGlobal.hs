{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Llvm.Pass.DynSafeRwGlobal (dynsafeRwGlobal) where

import Llvm.Data.Ir
import Llvm.Query.IrCxt
import Llvm.Query.ConstValue
import Llvm.Query.Conversion
import Llvm.Query.TypeDef
import Llvm.Query.TypeConstValue
import Llvm.Pass.DynSafeUtil
import qualified Data.Map as M

{-
dynsafeRwGlobal :: TypeEnv -> TlGlobal -> ([TlGlobal], M.Map GlobalId (FatPointer GlobalId))
dynsafeRwGlobal te x = case x of
  TlGlobalOpaque {..} -> ([x], M.empty)
  TlGlobalDtype {..} -> case (tlg_lhs, tlg_linkage) of
    (Nothing,_) -> ([x], M.empty)
    (_, Just LinkageExternal) -> ([x], M.empty)
    (Just lhs, _) -> 
      let newDtyp = getTypeWithRtMeta te tlg_dtype
          blockLhs = newGlobalId lhs "#block"
          (SizeInByte typeSize) = getTypeStoreSize te tlg_dtype
          tlgBlock = x { tlg_lhs = Just blockLhs
                       , tlg_dtype = ucast newDtyp
                       }
          tlgX = x { tlg_const = Just $ C_getelementptr (Is InBounds) (T (ptr0 newDtyp) (C_globalAddr blockLhs))
                                 const_Int32ZeroOne
                   }
          (T _ cTypeSize) = getUint32 (fromIntegral typeSize)
      in ([tlgBlock,tlgX], 
          M.insert lhs (FatPointer (PtrAndInt lhs Nothing) lhs  (Just $ Val_const cTypeSize) Nothing) M.empty)
-}