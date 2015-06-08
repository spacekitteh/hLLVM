{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
, FlexibleInstances, FlexibleContexts #-}
module Llvm.Hir.Internalization where

import Llvm.Hir.Data
import Llvm.Hir.Cast
import Llvm.Hir.Composer
import Control.Monad ()
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

{-
 Internalization converts a "metadata" value to a first class value that can be referred by LLVM code.
-}

class (Monad m, MonadState (M.Map (Dtype, Const) (DefAndRef a)) m) =>  LlvmGlobalGenMonad a m where
  newGlobalId :: m GlobalId
  getDefAndRef :: (Dtype, Const) -> m (Maybe (DefAndRef a))
  cacheToplevel :: (Dtype, Const) -> DefAndRef a -> m ()

data DefAndRef a = DefAndRef { llvmDef :: Toplevel a
                             , llvmRef :: T (Type ScalarB P) Const
                             }

class (Ord x, Eq x) => Internalization x where
  internalize :: LlvmGlobalGenMonad a m => x -> m (DefAndRef a)

instance Internalization String where
  internalize c =
    let str = (fmap (\x -> case x of
                        '\\' -> '_'
                        '"' -> '_'
                        _ ->  x) c) ++ ['\00']
        strType = ucast (Tarray (fromIntegral $ length str) (ucast i8))
    in do { mdef <- getDefAndRef (strType, C_str str)
          ; case mdef of
            Just defref -> return defref
            Nothing -> do { lhs <- newGlobalId
                          ; let dr = DefAndRef { llvmDef = ToplevelGlobal $
                                                           TlGlobalDtype { tlg_lhs = lhs
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
                                               , llvmRef = T (ptr0 i8) $ C_getelementptr (Is InBounds)
                                                           (T (ucast $ Tpointer (ucast strType) 0) (C_globalAddr lhs))
                                                           (i32sToTcs [0,0])
                                               }
                          ; cacheToplevel (strType, C_str str) dr
                          ; return dr
                          }
          }

instance LlvmGlobalGenMonad a (StateT (M.Map (Dtype, Const) (DefAndRef a)) (State (String, Int))) where
  newGlobalId = do { (prefix, cnt) <- lift get
                   ; lift $ put (prefix, cnt+1)
                   ; return $ GlobalIdAlphaNum (prefix ++ show cnt)
                   }
  cacheToplevel k v = modify (M.insert k v)
  getDefAndRef k = do { s <- get
                      ; return $ M.lookup k s
                      }

runSimpleLlvmGlobalGen :: String -> Int -> (StateT (M.Map (Dtype, Const) (DefAndRef a)) (State (String, Int))) x -> 
                          (x, (M.Map (Dtype, Const) (DefAndRef a)))
runSimpleLlvmGlobalGen prefix initCnt f = evalState (runStateT f M.empty) (prefix, initCnt)