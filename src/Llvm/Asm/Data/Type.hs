module Llvm.Asm.Data.Type  
       (module Llvm.Asm.Data.Type
       ,module Llvm.Asm.Data.AtomicEntity 
       )
       where

import Llvm.Asm.Data.AtomicEntity hiding (AddrSpace(..))
import Data.Word (Word8, Word32, Word64)

data TypePrimitive = TpI Word32
                   | TpF Word32
                   | TpV Word32 
                   | TpHalf | TpFloat | TpDouble | TpFp128 | TpX86Fp80 | TpPpcFp128 
                   | TpX86Mmx 
                   | TpNull
                   | TpLabel
                   deriving (Eq,Ord,Show)


splitCallReturnType :: Type -> (Type, Maybe (Type, AddrSpace))
splitCallReturnType (Tpointer f@(Tfunction rt _ _) as) = (rt, Just (f, as))
splitCallReturnType x = (x, Nothing)

data Type = Tprimitive TypePrimitive
          | Topaque 
          | Tname String
          | TquoteName String
          | Tno Word32
          | Tarray Word64 Type
          | Tvector Word32 Type
          | Tstruct Packing [Type]
          | Tpointer Type AddrSpace
          | Tfunction Type TypeParamList [FunAttr]
          | Tvoid
          deriving (Eq,Ord,Show)


data MetaKind = Mtype Type
              | Mmetadata
              deriving (Eq, Ord, Show)
                   
data FormalParam = FormalParamData Type [ParamAttr] Fparam
                 | FormalParamMeta MetaKind Fparam
                 deriving (Eq,Ord,Show)

data FormalParamList = FormalParamList [FormalParam] (Maybe VarArgParam) deriving (Eq,Ord,Show)

data TypeParamList = TypeParamList [Type] (Maybe VarArgParam) deriving (Eq,Ord,Show)

data AddrSpace = AddrSpace Word32 
               | AddrSpaceUnspecified deriving (Eq,Ord,Show)