{-# LANGUAGE CPP, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Llvm.Hir.DataLayoutMetrics where

import Llvm.ErrorLoc
#define FLC   (FileLoc $(srcLoc))

import Llvm.Asm.Data.DataLayout
import Llvm.Asm.Data.AtomicEntity (TargetTriple)
import Llvm.Hir.Data.Type
import Data.DoubleWord (Word96)
import Data.Bits(Bits(..), (.&.), (.|.), shiftR)
import qualified Data.Map as M

data AlignType = AlignAbi | AlignPref deriving (Eq, Ord, Show)

{- DataLayoutMetrics class with default values -}
class DataLayoutMetrics a where
  matchLayoutSpecAndTriple :: a -> [LayoutSpec] -> TargetTriple -> Bool
  toTriple :: a -> TargetTriple
  toLayoutSpec :: a -> [LayoutSpec]
  toLayoutSpec x = [DlE $ endianness x
                   ,DlM $ mangling x
                   ,DlP LayoutAddrSpaceUnspecified (sizeOfPtr x 0) (alignOfPtr x 0)
                   ] ++ (fmap (uncurry DlI) $ M.toList $ alignOfIx x)
                   ++ (fmap (uncurry DlF) $ M.toList $ alignOfFx x)
                   ++ (fmap (uncurry DlV) $ M.toList $ alignOfVx x)
                   ++ [DlN $ nativeInts x
                      ,DlA $ alignOfAggregate x
                      ,DlS $ alignOfStack x]

  endianness :: a -> Endianness
  endianness _ = BigEndian
  
  nativeInts :: a -> [SizeInBit]  
  
  {- integers -}
  integers :: a -> [SizeInBit]
  integers _ = [SizeInBit 1, SizeInBit 8, SizeInBit 16, SizeInBit 32, SizeInBit 64]
  
  {- pointer -}
  sizeOfPtr :: a -> AddrSpace -> SizeInBit
  sizeOfPtr _ _ = SizeInBit 64
  
  alignOfPtr :: a -> AddrSpace -> AlignMetrics
  alignOfPtr _ _ = AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64)
  
  {- natural stack alignment -}
  alignOfStack :: a -> StackAlign
  alignOfStack _ = StackAlignUnspecified
  
  {- ix -}
  alignOfIx :: a -> M.Map SizeInBit AlignMetrics
  alignOfIx _ = intLayoutMetrics

  alignOfFx :: a -> M.Map SizeInBit AlignMetrics
  alignOfFx _ = floatLayoutMetrics
  
  {- v64 -}
  alignOfVx :: a -> M.Map SizeInBit AlignMetrics
  alignOfVx _ = vectorLayoutMetrics 
  
  {- the aggregate alignment -}
  alignOfAggregate :: a -> AlignMetrics
  alignOfAggregate _ = AlignMetrics (AlignInBit 0) (Just $ AlignInBit 64)
  
  mangling:: a -> Mangling
  mangling _ = error "mangling is not specified"


intLayoutMetrics :: M.Map SizeInBit AlignMetrics 
intLayoutMetrics = M.fromList [(SizeInBit 1, AlignMetrics (AlignInBit 8) (Just (AlignInBit 8)))
                              ,(SizeInBit 8, AlignMetrics (AlignInBit 8) (Just (AlignInBit 8)))
                              ,(SizeInBit 16, AlignMetrics (AlignInBit 16) (Just (AlignInBit 16)))
                              ,(SizeInBit 32, AlignMetrics (AlignInBit 32) (Just (AlignInBit 32)))
                              ,(SizeInBit 64, AlignMetrics (AlignInBit 32) (Just $ AlignInBit 64))
                              ]

floatLayoutMetrics :: M.Map SizeInBit AlignMetrics
floatLayoutMetrics = M.fromList [(SizeInBit 16, AlignMetrics (AlignInBit 16) (Just $ AlignInBit 16))
                                ,(SizeInBit 32, AlignMetrics (AlignInBit 32) (Just $ AlignInBit 32))
                                ,(SizeInBit 64, AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64))
                                ,(SizeInBit 80, error "unsuppoert")
                                ,(SizeInBit 128, AlignMetrics (AlignInBit 128) (Just $ AlignInBit 128))
                                ]
                     
vectorLayoutMetrics :: M.Map SizeInBit AlignMetrics
vectorLayoutMetrics = M.fromList [(SizeInBit 64, AlignMetrics (AlignInBit 64) (Just $ AlignInBit 64))
                                 ,(SizeInBit 128, AlignMetrics (AlignInBit 128) (Just $ AlignInBit 128))
                                 ]

getIXAlignMetrics :: DataLayoutMetrics dlm => dlm -> SizeInBit -> AlignMetrics  
getIXAlignMetrics dlm n = case M.lookup n (alignOfIx dlm) of
  Just x -> x
  Nothing -> error "unsupported int"
  
getFXAlignMetrics :: DataLayoutMetrics dlm => dlm -> SizeInBit -> AlignMetrics  
getFXAlignMetrics dlm n = case M.lookup n (alignOfFx dlm) of
  Just x -> x
  Nothing -> error "unsupported float"  
  
  
getVXAlignMetrics :: DataLayoutMetrics dlm => dlm -> SizeInBit -> Maybe AlignMetrics  
getVXAlignMetrics dlm n = M.lookup n (alignOfVx dlm)
  
  
bestMatchSizeInBit :: DataLayoutMetrics dlm => dlm -> Word96 -> SizeInBit
bestMatchSizeInBit dlm n = 
  SizeInBit $ foldl 
  (\p s -> 
    if p == 0 then s {- get the first one -}
    else if p < n && p < s then s {- when p is still smaller than n, get a bigger one -}
         else if n < p && n < s && s < p 
              then s {- when both p and s are bigger than n, get the smallest one that is bigger than n -}
              else p
  ) 0 $ fmap (\(SizeInBit x) -> x) (integers dlm)
