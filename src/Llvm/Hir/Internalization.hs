{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Llvm.Hir.Internalization where

import Llvm.Hir.Data
import Llvm.Hir.Cast
import Llvm.Hir.Composer

{-
 Internalization converts a "metadata" value to a first class value that can be referred by LLVM code.
-}

data DefAndRef d r = DefAndRef { llvmDef :: d
                               , llvmRef :: r
                               }

class Internalization a d r where
  internalize :: a -> DefAndRef d r
  
instance Internalization (GlobalId, String) TlGlobal Const where  
  internalize (lhs,c) = 
    let str = (fmap (\x -> case x of
                        '\\' -> '_'
                        '"' -> '_'
                        _ ->  x) c) ++ ['\00']
        strType = ucast (Tarray (fromIntegral $ length str) (ucast i8))
    in DefAndRef { llvmDef = TlGlobalDtype { tlg_lhs = lhs
                                           , tlg_linkage = Just LinkagePrivate 
                                           , tlg_visibility = Nothing
                                           , tlg_dllstorage = Nothing
                                           , tlg_tls = Nothing
                                           , tlg_addrnaming = UnnamedAddr
                                           , tlg_addrspace = Nothing
                                           , tlg_externallyInitialized = IsNot ExternallyInitialized
                                           , tlg_globalType = GlobalType "constant"
                                           , tlg_dtype = strType
                                           , tlg_const = Just $ C_str str
                                           , tlg_section = Nothing
                                           , tlg_comdat = Nothing
                                           , tlg_alignment = Just (Alignment 1)
                                           }
                 , llvmRef = C_getelementptr (Is InBounds)
                             (T (ucast $ Tpointer (ucast strType) 0) (C_globalAddr lhs)) 
                             (i32sToTcs [0,0])
                 }