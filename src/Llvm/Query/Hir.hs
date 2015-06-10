{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Llvm.Query.Hir where

import Llvm.Hir.Data
import qualified Data.Set as S
import Llvm.ErrorLoc
#define FLC (FileLoc $(srcLoc))


-- this should be a map, globalid might have an opaque type
globalIdOfModule :: (Module a) -> S.Set (Dtype, GlobalId) 
globalIdOfModule (Module tl) = foldl (\a b -> S.union a (globalIdOf b)) S.empty tl
  where globalIdOf (ToplevelGlobal (TlGlobalDtype lhs _ _ _ _ _ _ _ _ t _ _ _ _)) = S.singleton (t, lhs)
        globalIdOf _ = S.empty


{- a transformation might add globals to a module, LLVM does not allow duplicate global 
   declarations. This function finds out what are not declared. 
-}
{-
selectUndeclaredTlGlobals :: Module a -> [TlGlobal] -> [TlGlobal]
selectUndeclaredTlGlobals (Module l) = 
  let s = S.foldl (\p e -> case e of
                      ToplevelGlobal (TlGlobalDtype{..}) -> maybe p (flip S.insert p) tlg_lhs
                      ToplevelGlobal (TlGlobalOpaque{..}) -> maybe p (flip S.insert p) tlg_lhs   
                      _ -> p
                  ) S.empty l
  in filter (\x -> case x of
                TlGlobalDtype{..} -> maybe 
-}

data SingleConstAddr = SingleConstAddr { globalId :: Either (GlobalId, Label) GlobalId
                                       , reconstructor :: Const -> Const
                                       }

getSingleConstAddr :: Const -> Maybe SingleConstAddr -- (GlobalId, Const -> Const)
getSingleConstAddr cnst = case cnst of
  C_u8 _ -> Nothing
  C_u16 _ -> Nothing
  C_u32 _ -> Nothing
  C_u64 _ -> Nothing
  C_u96 _ -> Nothing
  C_u128 _ -> Nothing
  C_s8 _ -> Nothing
  C_s16 _ -> Nothing
  C_s32 _ -> Nothing
  C_s64 _ -> Nothing
  C_s96 _ -> Nothing
  C_s128 _ -> Nothing
  C_int _ -> Nothing
  C_uhex_int _ -> Nothing
  C_shex_int _ -> Nothing
  C_float _ -> Nothing
  C_null -> Nothing
  C_undef -> Nothing
  C_true -> Nothing
  C_false -> Nothing
  C_zeroinitializer -> Nothing
  C_globalAddr g -> Just $ SingleConstAddr { globalId = Right g, reconstructor = id }
  C_getelementptr b (T t c) idx -> case getSingleConstAddr c of
    Nothing -> Nothing
    Just ca -> Just $ ca { reconstructor = \x -> C_getelementptr b (T t (reconstructor ca x)) idx}
  C_ptrtoint (T st c) dt -> case getSingleConstAddr c of
    Nothing -> Nothing
    Just ca -> Just $ ca { reconstructor = \x -> C_ptrtoint (T st (reconstructor ca x)) dt }
  C_inttoptr (T st c) dt -> case getSingleConstAddr c of
    Nothing -> Nothing
    Just ca -> Just $ ca { reconstructor = \x -> C_inttoptr (T st (reconstructor ca x)) dt }
  C_bitcast (T st c) dt -> case getSingleConstAddr c of  
    Nothing -> Nothing
    Just ca -> Just $ ca { reconstructor = \x -> C_bitcast (T st (reconstructor ca x)) dt }
  C_str _ -> Nothing -- a constant string itself is a value initialized in memory, itself does not indicate an address
  C_struct _ _ -> Nothing
  C_vector _ -> Nothing
  C_vectorN _ _-> Nothing
  C_array _ -> Nothing
  C_arrayN _ _ -> Nothing
  C_labelId _ -> errorLoc FLC ("unsupported " ++ show cnst)
  C_block g l -> Just $ SingleConstAddr { globalId = Left (g, l), reconstructor = id } -- errorLoc FLC ("unsupported " ++ show cnst)
  _ -> Nothing