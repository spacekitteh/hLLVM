{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Llvm.Data.Shared.Util where

import Language.Haskell.TH

eq2 :: (Eq a, Eq b) => (a,a) -> (b,b) -> Bool
eq2 (a1,a2) (b1,b2) = a1==a2 && b1==b2

eq3 :: (Eq a, Eq b, Eq c) => (a,a) -> (b,b) -> (c,c) -> Bool
eq3 (a1,a2) (b1,b2) (c1,c2) = eq2 (a1,a2) (b1,b2) && c1 == c2

eq4 :: (Eq a, Eq b, Eq c, Eq d) => (a,a) -> (b,b) -> (c,c) -> (d,d) -> Bool
eq4 (a1,a2) (b1,b2) (c1,c2) (d1, d2) = eq3 (a1,a2) (b1,b2) (c1, c2) && d1 == d2

eq5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => (a,a) -> (b,b) -> (c,c) -> (d,d) -> (e,e) -> Bool
eq5 (a1,a2) (b1,b2) (c1,c2) (d1, d2) (e1, e2) = eq4 (a1,a2) (b1,b2) (c1,c2) (d1,d2) && e1 == e2


compare2 :: (Ord a, Ord b) => (a,a) -> (b, b) -> Ordering    
compare2 (a1,a2) (b1, b2) = let v = compare a1 a2
                            in if v == EQ then compare b1 b2
                               else v

compare3 :: (Ord a, Ord b, Ord c) => (a,a) -> (b,b) -> (c,c) -> Ordering
compare3 (a1,a2) (b1,b2) (c1,c2) = let v = compare2 (a1,a2) (b1,b2)
                                   in if v == EQ then compare c1 c2
                                      else v

compare4 :: (Ord a, Ord b, Ord c, Ord d) => (a,a) -> (b,b) -> (c,c) -> (d,d) -> Ordering
compare4 (a1,a2) (b1,b2) (c1,c2) (d1,d2) = let v = compare3 (a1,a2) (b1,b2) (c1,c2)
                                           in if v == EQ then compare d1 d2
                                              else v

compare5 :: (Ord a, Ord b, Ord c, Ord d, Ord e) => (a,a) -> (b,b) -> (c,c) -> (d,d) -> (e,e) -> Ordering
compare5 (a1,a2) (b1,b2) (c1,c2) (d1, d2) (e1, e2) = let v = compare4 (a1,a2) (b1,b2) (c1,c2) (d1,d2)
                                                     in if v == EQ then compare e1 e2
                                                        else v

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

{- up casting -}
class Ucast l1 l2 where
  ucast :: l1 -> l2

{- down casting -}
class Dcast l1 l2 where
  dcast :: FileLoc -> l1 -> l2
  
{- horizontal casting -}
class Hcast l1 l2 where 
  hcast :: FileLoc -> l1 -> l2
  
  
class Mingle a where  
  mingle :: a -> String
  
instance Mingle a => Mingle ([a]) where  
  mingle l = foldl (\p e -> p ++ (mingle e)) "" l