module Llvm.Data.Shared.Type  
       (module Llvm.Data.Shared.Type
       ,module Llvm.Data.Shared.AtomicEntity
       )
       where

import Llvm.Data.Shared.AtomicEntity (FunAttr,ParamAttr,Alignment,Lstring,AddrSpace,LocalId)


data TypePrimitive = TpI Integer
                   | TpF Integer 
                   | TpV Integer 
                   | TpVoid 
                   | TpHalf | TpFloat | TpDouble | TpFp128 | TpX86Fp80 | TpPpcFp128 
                   | TpX86Mmx 
                   | TpNull
                   | TpLabel
                   deriving (Eq,Ord,Show)


isVoidType :: Type -> Bool
isVoidType (Tprimitive TpVoid) = True
isVoidType (Tfunction t _ _) = isVoidType t
isVoidType (Tpointer t _) = isVoidType t
isVoidType _ = False
 
data Packing = Packed                
             | Unpacked
             deriving (Eq, Ord, Show)
                      
data Type = Tprimitive TypePrimitive
          | Tmetadata
          | Topaque 
          | Tname Lstring
          | TquoteName Lstring
          | Tno Integer
          | TupRef Integer
          | Tarray Integer Type
          | Tvector Integer Type
          | Tstruct Packing [Type]
          | Tpointer Type AddrSpace
          | Tfunction Type TypeParamList [FunAttr]
          -- | deref a type will strip off Tpointer, this is a syntatical
          -- | representation that should be evaluated later.
          | Tderef Type 
          deriving (Eq,Ord,Show)
                   
data Fparam = FimplicitParam 
            | FexplicitParam LocalId 
            deriving (Eq,Ord,Show)
              
data FormalParam = FormalParam Type [ParamAttr] (Maybe Alignment) Fparam [ParamAttr]
                 deriving (Eq,Ord,Show)

data FormalParamList = FormalParamList [FormalParam] (Maybe VarArgParam) [FunAttr] deriving (Eq,Ord,Show)

data TypeParamList = TypeParamList [Type] (Maybe VarArgParam) deriving (Eq,Ord,Show)

data VarArgParam = VarArgParam deriving (Eq, Ord, Show)



