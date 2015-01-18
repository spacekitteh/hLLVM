module Llvm.Data.Shared.SimpleConst 
       (module Llvm.Data.Shared.SimpleConst
       ,module Llvm.Data.Shared.AtomicEntity
       )
       where

import Llvm.Data.Shared.AtomicEntity (GlobalId)                    
-- | Simple Constants <http://llvm.org/releases/3.0/docs/LangRef.html#simpleconstants>  
data SimpleConstant = CpInt String
                    | CpUhexInt String
                    | CpShexInt String
                    | CpFloat String
                    -- | null pointer constant
                    | CpNull
                    | CpUndef
                    -- | true::i1 
                    | CpTrue
                    -- | false::i1
                    | CpFalse
                    | CpZero
                    | CpGlobalAddr GlobalId
                    | CpStr String
                    deriving (Eq, Ord, Show)
                             
