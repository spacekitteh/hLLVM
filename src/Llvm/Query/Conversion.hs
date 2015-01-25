{-# LANGUAGE FlexibleContexts #-}
module Llvm.Query.Conversion where
import Llvm.Data.CoreIr
import Llvm.Query.Qerror

strToApInt :: MonadError Qerror m => String -> m Integer
strToApInt s = case ((read s)::[(Integer,String)]) of
  [(n,"")] -> return n
  _ -> throwError (QerrIsNotInteger s)

typedValueToTypedConst :: MonadError Qerror m => TypedValue -> m TypedConst  
typedValueToTypedConst (TypedValue t (Vc c)) = return $ (TypedConst t c) 
typedValueToTypedConst x = throwError (QerrIsNotConst $ show x)


zExtTo32or64 :: MonadError Qerror m => Integer -> m Integer
zExtTo32or64 n = return n -- FIXME n should be less then or equal to uint64_t


castToStructType :: MonadError Qerror m => Type -> m (Packing, [Type])
castToStructType (Tstruct p ts) = return (p, ts)
castToStructType x = throwError (QerrWithInfo $ (show x) ++ " is not a struct type")


getUniqueInteger :: MonadError Qerror m => TypedConst -> m Integer
getUniqueInteger (TypedConst _ (Ccp (CpInt s)))  = strToApInt s
getUniqueInteger x = throwError (QerrIsNotInteger $ show x)


type ExponentType = Int 
data FloatingSemantics = FloatingSemantics { maxExponent :: ExponentType  
                                           , minExponent :: ExponentType
                                           , precision :: Integer
                                           } deriving (Eq, Ord, Show)

ieeeHalf = FloatingSemantics 15 (-14) 11
ieeeSingle = FloatingSemantics 127 (-126) 24
ieeeDouble = FloatingSemantics 1023 (-1022) 53
ieeeQuad = FloatingSemantics 16383 (-16382) 113
x87DoubleExtended = FloatingSemantics 16383 (-16382) 64
pPcDoubleDouble = FloatingSemantics 1023 (-1022+53) (53+53)
bogus = FloatingSemantics 0 0 0

data InMemRep  = InMemRep TypePrimitive SimpleConstant deriving (Eq, Ord, Show)