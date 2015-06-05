{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
, FlexibleInstances, FlexibleContexts #-}
module Llvm.Hir.Internalization where

import Llvm.Hir.Data
import Llvm.Hir.Cast
import Llvm.Hir.Composer
import Control.Monad ()
import Control.Monad.State

{-
 Internalization converts a "metadata" value to a first class value that can be referred by LLVM code.
-}

class (Monad m, MonadState [Toplevel a] m) =>  LlvmGlobalGenMonad a m where
  newGlobalId :: m GlobalId
  cacheToplevel :: Toplevel a -> m ()

data DefAndRef a r = DefAndRef { llvmDef :: Toplevel a
                               , llvmRef :: r
                               }

class Internalization x r where
  internalize :: LlvmGlobalGenMonad a m => x -> m (DefAndRef a r)

instance Internalization String (T Dtype Const) where
  internalize c =
    let str = (fmap (\x -> case x of
                        '\\' -> '_'
                        '"' -> '_'
                        _ ->  x) c) ++ ['\00']
        strType = ucast (Tarray (fromIntegral $ length str) (ucast i8))
    in do { lhs <- newGlobalId
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
                               , llvmRef = T (ucast $ ptr0 i8) $ C_getelementptr (Is InBounds)
                                           (T (ucast $ Tpointer (ucast strType) 0) (C_globalAddr lhs))
                                           (i32sToTcs [0,0])
                               }
          ; cacheToplevel $ llvmDef dr
          ; return dr
          }

instance LlvmGlobalGenMonad a (StateT [Toplevel a] (State (String, Int))) where
  newGlobalId = do { (prefix, cnt) <- lift get
                   ; lift $ put (prefix, cnt+1)
                   ; return $ GlobalIdAlphaNum (prefix ++ show cnt)
                   }
  cacheToplevel x = modify (x:)

runSimpleLlvmGlobalGen :: String -> Int -> (StateT [Toplevel a] (State (String, Int))) x -> (x, [Toplevel a])
runSimpleLlvmGlobalGen prefix initCnt f = evalState (runStateT f []) (prefix, initCnt)
