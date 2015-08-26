{-# LANGUAGE CPP, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Llvm.Hir.Target
       (module Llvm.Hir.DataLayoutMetrics
       ,module Llvm.Hir.Target
       ,module Llvm.Hir.Target.Linux_Gnu) where

import Llvm.Hir.DataLayoutMetrics
import Llvm.Hir.Target.Linux_Gnu 

data Target = forall a. (Show a, DataLayoutMetrics a) => Target a 

instance Show Target where
   show (Target x) = show x


targets :: [Target]
targets = [Target I386_Pc_Linux_Gnu, Target X86_64_Pc_Linux_Gnu, Target X86_64_Unknown_Linux_Gnu]
