{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Llvm.ErrorLoc where

import Language.Haskell.TH
import Data.Maybe (fromJust)

newtype FileLoc = FileLoc String deriving (Eq, Ord, Show)

castError :: Show a => FileLoc -> String -> a -> b
castError (FileLoc loc) s x = error $ (loc ++ "irrefutable error, casting " ++ show x ++ " to " ++ s)

dcastError :: Show a => FileLoc -> String -> a -> b
dcastError = castError


srcLoc :: ExpQ
srcLoc = do { (Loc f p m s e) <- location
            ; stringE (p ++ ":" ++ m ++ "@" ++ show s)
            }            
                       

errorLoc :: FileLoc -> String -> a
errorLoc (FileLoc lc) s = error (lc ++ ":" ++ s)


fromJustLoc :: FileLoc -> Maybe a -> a
fromJustLoc loc x = case x of
  Just v -> v
  Nothing -> errorLoc loc "Nothing"