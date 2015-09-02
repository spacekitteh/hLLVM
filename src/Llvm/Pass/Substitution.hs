{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs, TemplateHaskell, CPP #-}
{-# LANGUAGE RecordWildCards,  TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}
module Llvm.Pass.Substitution where

import Llvm.ErrorLoc
#define FLC (FileLoc $(srcLoc))

import Control.Monad
import Data.Maybe
import Prelude hiding (succ)

import qualified Compiler.Hoopl as H

import Llvm.Hir.Data
import Llvm.Query.Type
import Llvm.Util.Monadic (maybeM)
import Debug.Trace
import Control.Monad
import qualified Data.Map as M

import Llvm.Pass.Changer

class Substitutable a g b h where
  substitute :: Changer g h -> a -> b

instance Substitutable a g b h => Substitutable (Maybe a) g (Maybe b) h where
  substitute chg (Just a) = Just $ substitute chg a
  substitute chg Nothing = Nothing

instance Substitutable a g b h => Substitutable [a] g [b] h where
  substitute chg l = fmap (substitute chg) l

instance Substitutable () g () h where
  substitute chg l = ()

instance (Substitutable a g x h, Substitutable b g y h) => Substitutable (Either a b) g (Either x y) h where
  substitute chg (Left a) = Left $ substitute chg a
  substitute chg (Right a) = Right $ substitute chg a

instance (Substitutable a g x h, Substitutable b g y h) => Substitutable (a, b) g (x, y) h where
  substitute chg (a, b) = (substitute chg a, substitute chg b)

instance Substitutable (TypedConstOrNull g) g (TypedConstOrNull h) h where
  substitute chg@Changer{..} x = case x of
    TypedConst (T dt c) -> TypedConst (T dt (substitute chg c))
    UntypedNull -> UntypedNull

instance Substitutable (Value g) g (Value h) h where
  substitute chg@Changer{..} cst = case cst of
    Val_ssa lid -> Val_ssa $ substitute chg lid
    Val_const c -> Val_const $ substitute chg c

instance Substitutable Label g Label h where
  substitute _ = id

instance Substitutable v g w h => Substitutable (Select s r v) g (Select s r w) h where
  substitute chg (Select cnd t f) =
    Select (substitute chg cnd) (substitute chg t) (substitute chg f)

instance (Substitutable v g w h, Substitutable idx g ldx h) => Substitutable (GetElementPtr s v idx) g (GetElementPtr s w ldx) h where
  substitute chg (GetElementPtr b base indices) =
    GetElementPtr b (substitute chg base) (substitute chg indices)

instance Substitutable v g w h => Substitutable (Icmp s v) g (Icmp s w) h where
  substitute chg (Icmp op t v1 v2) =
    Icmp op t (substitute chg v1) (substitute chg v2)

instance Substitutable v g w h => Substitutable (Fcmp s v) g (Fcmp s w) h where
  substitute chg (Fcmp op t v1 v2) =
    Fcmp op t (substitute chg v1) (substitute chg v2)

instance Substitutable v g w h => Substitutable (ShuffleVector r v) g (ShuffleVector r w) h where
  substitute chg (ShuffleVector v1 v2 i) =
    ShuffleVector (substitute chg v1) (substitute chg v2) (substitute chg i)

instance Substitutable v g w h => Substitutable (ExtractElement r v) g (ExtractElement r w) h where
  substitute chg (ExtractElement v i) =
    ExtractElement (substitute chg v) (substitute chg i)

instance Substitutable v g w h => Substitutable (InsertElement r v) g (InsertElement r w) h where
  substitute chg (InsertElement v e i) =
    InsertElement (substitute chg v) (substitute chg e) (substitute chg i)

instance Substitutable v g w h => Substitutable (ExtractValue v) g (ExtractValue w) h where
  substitute chg (ExtractValue v ns) = ExtractValue (substitute chg v) ns

instance Substitutable v g w h => Substitutable (InsertValue v) g (InsertValue w) h where
  substitute chg (InsertValue v e ns) =
    InsertValue (substitute chg v) (substitute chg e) ns

instance Substitutable (Const g) g (Const h) h where
  substitute chg@Changer{..} cst =
    let change2c cf a b = cf (substitute chg a) (substitute chg b)
        cst1 = case cst of
          C_u8 x -> C_u8 x
          C_u16 x -> C_u16 x
          C_u32 x -> C_u32 x
          C_u64 x -> C_u64 x
          C_u96 x -> C_u96 x
          C_u128 x -> C_u128 x
          C_s8 x -> C_s8 x
          C_s16 x -> C_s16 x
          C_s32 x -> C_s32 x
          C_s64 x -> C_s64 x
          C_s96 x -> C_s96 x
          C_s128 x -> C_s128 x
          C_int x -> C_int x
          C_uhex_int x -> C_uhex_int x
          C_shex_int x -> C_shex_int x
          C_float x -> C_float x
          C_true -> C_true
          C_false -> C_false
          C_null -> C_null
          C_undef -> C_undef
          C_zeroinitializer -> C_zeroinitializer
          C_str x -> C_str x
          C_labelId lb -> C_labelId (substitute chg lb)
          C_block g l -> C_block (substitute chg g) l
          C_globalAddr x -> C_globalAddr (substitute chg x)
          C_struct pk l -> C_struct pk (substitute chg l)
          C_vector l -> C_vector (substitute chg l)
          C_vectorN n e -> C_vectorN n (substitute chg e)
          C_array l -> C_array (substitute chg l)
          C_arrayN n e -> C_arrayN n (substitute chg e)
          C_add n t c1 c2 -> change2c (C_add n t) c1 c2
          C_sub n t c1 c2 -> change2c (C_sub n t) c1 c2
          C_mul n t c1 c2 -> change2c (C_mul n t) c1 c2
          C_udiv n t c1 c2 -> change2c (C_udiv n t) c1 c2
          C_sdiv n t c1 c2 -> change2c (C_sdiv n t) c1 c2
          C_urem t c1 c2 -> change2c (C_urem t) c1 c2
          C_srem t c1 c2 -> change2c (C_srem t) c1 c2
          C_shl n t c1 c2 -> change2c (C_shl n t) c1 c2
          C_lshr n t c1 c2 -> change2c (C_lshr n t) c1 c2
          C_ashr n t c1 c2 -> change2c (C_ashr n t) c1 c2
          C_and t c1 c2 -> change2c (C_and t) c1 c2
          C_or t c1 c2 -> change2c (C_or t) c1 c2
          C_xor t c1 c2 -> change2c (C_xor t) c1 c2

          C_add_V n t c1 c2 -> change2c (C_add_V n t) c1 c2
          C_sub_V n t c1 c2 -> change2c (C_sub_V n t) c1 c2
          C_mul_V n t c1 c2 -> change2c (C_mul_V n t) c1 c2
          C_udiv_V n t c1 c2 -> change2c (C_udiv_V n t) c1 c2
          C_sdiv_V n t c1 c2 -> change2c (C_sdiv_V n t) c1 c2
          C_urem_V t c1 c2 -> change2c (C_urem_V t) c1 c2
          C_srem_V t c1 c2 -> change2c (C_srem_V t) c1 c2
          C_shl_V n t c1 c2 -> change2c (C_shl_V n t) c1 c2
          C_lshr_V n t c1 c2 -> change2c (C_lshr_V n t) c1 c2
          C_ashr_V n t c1 c2 -> change2c (C_ashr_V n t) c1 c2
          C_and_V t c1 c2 -> change2c (C_and_V t) c1 c2
          C_or_V t c1 c2 -> change2c (C_or_V t) c1 c2
          C_xor_V t c1 c2 -> change2c (C_xor_V t) c1 c2

          C_fadd n t c1 c2 -> change2c (C_fadd n t) c1 c2
          C_fsub n t c1 c2 -> change2c (C_fsub n t) c1 c2
          C_fmul n t c1 c2 -> change2c (C_fmul n t) c1 c2
          C_fdiv n t c1 c2 -> change2c (C_fdiv n t) c1 c2
          C_frem n t c1 c2 -> change2c (C_frem n t) c1 c2

          C_fadd_V n t c1 c2 -> change2c (C_fadd_V n t) c1 c2
          C_fsub_V n t c1 c2 -> change2c (C_fsub_V n t) c1 c2
          C_fmul_V n t c1 c2 -> change2c (C_fmul_V n t) c1 c2
          C_fdiv_V n t c1 c2 -> change2c (C_fdiv_V n t) c1 c2
          C_frem_V n t c1 c2 -> change2c (C_frem_V n t) c1 c2

          C_trunc tc tdest -> C_trunc (substitute chg tc) tdest
          C_zext tc tdest -> C_zext (substitute chg tc) tdest
          C_sext tc tdest -> C_sext (substitute chg tc) tdest
          C_fptrunc tc tdest -> C_fptrunc (substitute chg tc) tdest
          C_fpext tc tdest -> C_fpext (substitute chg tc) tdest
          C_fptoui tc tdest -> C_fptoui (substitute chg tc) tdest
          C_fptosi tc tdest -> C_fptosi (substitute chg tc) tdest
          C_uitofp tc tdest -> C_uitofp (substitute chg tc) tdest
          C_sitofp tc tdest -> C_sitofp (substitute chg tc) tdest
          C_ptrtoint tc tdest -> C_ptrtoint (substitute chg tc) tdest
          C_inttoptr tc tdest -> C_inttoptr (substitute chg tc) tdest
          C_bitcast tc tdest -> C_bitcast (substitute chg tc) tdest
          C_addrspacecast tc tdest -> C_addrspacecast (substitute chg tc) tdest

          C_trunc_V tc tdest -> C_trunc_V (substitute chg tc) tdest
          C_zext_V tc tdest -> C_zext_V (substitute chg tc) tdest
          C_sext_V tc tdest -> C_sext_V (substitute chg tc) tdest
          C_fptrunc_V tc tdest -> C_fptrunc_V (substitute chg tc) tdest
          C_fpext_V tc tdest -> C_fpext_V (substitute chg tc) tdest
          C_fptoui_V tc tdest -> C_fptoui_V (substitute chg tc) tdest
          C_fptosi_V tc tdest -> C_fptosi_V (substitute chg tc) tdest
          C_uitofp_V tc tdest -> C_uitofp_V (substitute chg tc) tdest
          C_sitofp_V tc tdest -> C_sitofp_V (substitute chg tc) tdest
          C_ptrtoint_V tc tdest -> C_ptrtoint_V (substitute chg tc) tdest
          C_inttoptr_V tc tdest -> C_inttoptr_V (substitute chg tc) tdest
          C_addrspacecast_V tc tdest -> C_addrspacecast_V (substitute chg tc) tdest

          C_getelementptr ib (T t c) l ->
            C_getelementptr ib (T t (substitute chg c)) (substitute chg l)
          C_getelementptr_V ib (T t c) l ->
            C_getelementptr_V ib (T t (substitute chg c)) (substitute chg l)

          C_select_I x -> C_select_I (substitute chg x)
          C_select_F x -> C_select_F (substitute chg x)
          C_select_P x -> C_select_P (substitute chg x)

          C_select_First c t f -> C_select_First (substitute chg c)
                                  (substitute chg t) (substitute chg f)
          C_select_VI x -> C_select_VI (substitute chg x)
          C_select_VF x -> C_select_VF (substitute chg x)
          C_select_VP x -> C_select_VP (substitute chg x)

          C_icmp x -> C_icmp (substitute chg x)
          C_icmp_V x -> C_icmp_V (substitute chg x)
          C_fcmp x -> C_fcmp (substitute chg x)
          C_fcmp_V x -> C_fcmp_V (substitute chg x)
          C_shufflevector_I x -> C_shufflevector_I (substitute chg x)
          C_shufflevector_F x -> C_shufflevector_F (substitute chg x)
          C_shufflevector_P x -> C_shufflevector_P (substitute chg x)

          C_extractelement_I x -> C_extractelement_I (substitute chg x)
          C_extractelement_F x -> C_extractelement_F (substitute chg x)
          C_extractelement_P x -> C_extractelement_P (substitute chg x)
          C_insertelement_I x -> C_insertelement_I (substitute chg x)
          C_insertelement_F x -> C_insertelement_F (substitute chg x)
          C_insertelement_P x -> C_insertelement_P (substitute chg x)

          C_extractvalue x -> C_extractvalue (substitute chg x)
          C_insertvalue x -> C_insertvalue (substitute chg x)
    in change_Const cst1


instance Substitutable x g y h => Substitutable (T t x) g (T t y) h where
  substitute chg (T t a) = T t (substitute chg a)

instance Substitutable (FunctionInterface g) g (FunctionInterface h) h where
  substitute chg fp@FunctionInterface {..} =
    fp { fi_fun_name = substitute chg fi_fun_name
       , fi_signature = substitute chg fi_signature
       , fi_comdat = substitute chg fi_comdat
       , fi_prefix = substitute chg fi_prefix
       , fi_prologue = substitute chg fi_prologue
       }

instance Substitutable (FunctionDeclare g) g (FunctionDeclare h) h where
  substitute chg x = case x of
    FunctionDeclareData {..} ->
      FunctionDeclareData { fd_linkage = fd_linkage
                          , fd_visibility = fd_visibility
                          , fd_dllstorage = fd_dllstorage
                          , fd_signature = fd_signature
                          , fd_fun_name = substitute chg fd_fun_name
                          , fd_addr_naming = fd_addr_naming
                          , fd_fun_attrs = fd_fun_attrs
                          , fd_section = fd_section
                          , fd_comdat = substitute chg fd_comdat
                          , fd_alignment = fd_alignment
                          , fd_gc = fd_gc
                          , fd_prefix = substitute chg fd_prefix
                          , fd_prologue = substitute chg fd_prologue
                          }
    FunctionDeclareMeta {..} -> 
      FunctionDeclareMeta { fd_fun_name = substitute chg fd_fun_name 
                          , fd_fun_attrs = fd_fun_attrs
                          , fd_retType = fd_retType
                          , fd_metakinds = fd_metakinds
                          }

instance Substitutable (TlDeclare g) g (TlDeclare h) h where
  substitute chg (TlDeclare fp) = TlDeclare (substitute chg fp)

instance Substitutable a g b h => Substitutable (TlDefine g a) g (TlDefine h b) h where
  substitute chg (TlDefine fp e g) =
    TlDefine (substitute chg fp) e (H.mapGraph (substitute chg) g)

instance Substitutable LocalId g LocalId h where
  substitute chg = change_LocalId chg

instance Substitutable (Pinst g) g (Pinst h) h where
  substitute chg p@Pinst{..} = p { flowins = substitute chg flowins
                                 , flowout = substitute chg flowout
                                 }

instance Substitutable a g b h => Substitutable (Node g a e x) g (Node h b e x) h where
  substitute chg node = case node of
    Lnode x -> Lnode x
    Pnode x dbgs -> Pnode (substitute chg x) (substitute chg dbgs)
    Cnode x dbgs -> Cnode (substitute chg x) (substitute chg dbgs)
    Mnode x dbgs -> Mnode (substitute chg x) (substitute chg dbgs)
    Comment x -> Comment x
    Enode x dbgs -> Enode (substitute chg x) (substitute chg dbgs)
    Tnode ti dbgs -> Tnode (substitute chg ti) (substitute chg dbgs)

instance Substitutable (Clause g) g (Clause h) h where
  substitute chg c = case c of
    Catch tv -> Catch (substitute chg tv)
    Filter tcn -> Filter (substitute chg tcn)

instance Substitutable (Cinst g) g (Cinst h) h where
  substitute chg x =
    let cid = change_LocalId chg
    in case x of
        I_alloca{..} -> I_alloca { inAllocaAttr = inAllocaAttr
                                 , dtype = dtype
                                 , size = substitute chg size
                                 , alignment = alignment
                                 , result = cid result
                                 }
        I_load{..} -> I_load { volatile = volatile
                             , pointer = (substitute chg) pointer
                             , alignment = alignment
                             , temporal = temporal
                             , invariantLoad = invariantLoad
                             , nonull = nonull
                             , result = substitute chg result
                             }
        I_loadatomic{..} -> I_loadatomic { atomicity = atomicity
                                         , volatile = volatile
                                         , pointer = substitute chg pointer
                                         , alignment = alignment
                                         , result = substitute chg result
                                         }
        I_store{..} -> I_store { volatile = volatile
                               , storedvalue = substitute chg storedvalue
                               , pointer = substitute chg pointer
                               , alignment = alignment
                               , nontemporal = nontemporal
                               }
        I_storeatomic{..} -> I_storeatomic { atomicity = atomicity
                                           , volatile = volatile
                                           , storedvalue = substitute chg storedvalue
                                           , pointer = substitute chg pointer
                                           , alignment = alignment
                                           }
        I_fence{..} -> I_fence { singleThread = singleThread
                               , ordering = ordering
                               }
        I_cmpxchg_I{..} -> I_cmpxchg_I { weak = weak
                                       , volatile = volatile
                                       , pointer = substitute chg pointer
                                       , cmpi = substitute chg cmpi
                                       , newi = substitute chg newi
                                       , singlethread = singlethread
                                       , success_ordering = success_ordering
                                       , failure_ordering = failure_ordering
                                       , result = substitute chg result
                                       }
        I_cmpxchg_F{..} -> I_cmpxchg_F { weak = weak
                                       , volatile = volatile
                                       , pointer = substitute chg pointer
                                       , cmpf = substitute chg cmpf
                                       , newf = substitute chg newf
                                       , singlethread = singlethread
                                       , success_ordering = success_ordering
                                       , failure_ordering = failure_ordering
                                       , result = substitute chg result
                                       }
        I_cmpxchg_P{..} -> I_cmpxchg_P { weak = weak
                                       , volatile = volatile
                                       , pointer = substitute chg pointer
                                       , cmpp = substitute chg cmpp
                                       , newp = substitute chg newp
                                       , singlethread = singlethread
                                       , success_ordering = success_ordering
                                       , failure_ordering = failure_ordering
                                       , result = substitute chg result
                                       }
        I_atomicrmw{..} -> I_atomicrmw { volatile = volatile
                                       , atomicOp = atomicOp
                                       , pointer = substitute chg pointer
                                       , val = substitute chg val
                                       , singlethread = singlethread
                                       , ordering = ordering
                                       , result = substitute chg result
                                       }
        I_call_fun{..} -> I_call_fun { call_ptr = substitute chg call_ptr
                                     , call_fun_interface = substitute chg call_fun_interface
                                     , call_return = substitute chg call_return
                                     }
        I_call_asm{..} -> I_call_asm { call_asmcode = call_asmcode
                                     , call_asm_interface = substitute chg call_asm_interface
                                     , call_return = substitute chg call_return
                                     }
        I_extractelement_I{..} -> I_extractelement_I { vectorI = substitute chg vectorI
                                                     , index = substitute chg index
                                                     , result = substitute chg result
                                                     }
        I_extractelement_F{..} -> I_extractelement_F { vectorF = substitute chg vectorF
                                                     , index = substitute chg index
                                                     , result = substitute chg result
                                                     }
        I_extractelement_P{..} -> I_extractelement_P { vectorP = substitute chg vectorP
                                                     , index = substitute chg index
                                                     , result = substitute chg result
                                                     }
        I_insertelement_I{..} -> I_insertelement_I { vectorI = (substitute chg) vectorI
                                                   , elementI = (substitute chg) elementI
                                                   , index = (substitute chg) index
                                                   , result = cid result
                                                   }
        I_insertelement_F{..} -> I_insertelement_F { vectorF = (substitute chg) vectorF
                                                   , elementF = (substitute chg) elementF
                                                   , index = (substitute chg) index
                                                   , result = cid result
                                                   }
        I_insertelement_P{..} -> I_insertelement_P { vectorP = (substitute chg) vectorP
                                                   , elementP = (substitute chg) elementP
                                                   , index = (substitute chg) index
                                                   , result = cid result
                                                   }
        I_shufflevector_I{..} -> I_shufflevector_I { vector1I = (substitute chg) vector1I
                                                   , vector2I = (substitute chg) vector2I
                                                   , vectorIdx = (substitute chg) vectorIdx
                                                   , result = cid result
                                                   }
        I_shufflevector_F{..} -> I_shufflevector_F { vector1F = (substitute chg) vector1F
                                                   , vector2F = (substitute chg) vector2F
                                                   , vectorIdx = (substitute chg) vectorIdx
                                                   , result = cid result
                                                   }
        I_shufflevector_P{..} -> I_shufflevector_P { vector1P = (substitute chg) vector1P
                                                   , vector2P = (substitute chg) vector2P
                                                   , vectorIdx = (substitute chg) vectorIdx
                                                   , result = cid result
                                                   }
        I_extractvalue{..} -> I_extractvalue { record = (substitute chg) record
                                             , windices = windices
                                             , result = cid result
                                             }
        I_insertvalue{..} -> I_insertvalue { record = (substitute chg) record
                                           , element = (substitute chg) element
                                           , windices = windices                                                       
                                           , result = cid result
                                           }
        I_landingpad{..} -> I_landingpad { resultType = resultType
                                         , persFnType = persFnType
                                         , persFn = substitute chg persFn
                                         , cleanup = cleanup
                                         , clauses = substitute chg clauses
                                         , result = cid result
                                         }
        I_getelementptr{..} -> I_getelementptr { inBounds = inBounds
                                               , pointer = (substitute chg) pointer
                                               , indices = substitute chg indices
                                               , result = cid result
                                               }
        I_getelementptr_V{..} -> I_getelementptr_V { inBounds = inBounds
                                                   , vpointer = (substitute chg) vpointer
                                                   , vindices = substitute chg vindices
                                                   , result = cid result
                                                   }
        I_icmp{..} -> I_icmp { icmpOp = icmpOp
                             , icmpType = icmpType
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_icmp_V{..} -> I_icmp_V { icmpOp = icmpOp
                                 , icmpTypeV = icmpTypeV
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_fcmp{..} -> I_fcmp { fcmpOp = fcmpOp
                             , fcmpTypeF = fcmpTypeF
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_fcmp_V{..} -> I_fcmp_V { fcmpOp = fcmpOp
                                 , fcmpTypeVF = fcmpTypeVF
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_add{..} -> I_add { flagI = flagI
                           , typeI = typeI
                           , operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        I_sub{..} -> I_sub { flagI = flagI
                           , typeI = typeI
                           , operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        I_mul{..} -> I_mul { flagI = flagI
                           , typeI = typeI
                           , operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        I_udiv{..} -> I_udiv { flagE = flagE
                             , typeI = typeI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_sdiv{..} -> I_sdiv { flagE = flagE
                             , typeI = typeI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_urem{..} -> I_urem { typeI = typeI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_srem{..} -> I_srem { typeI = typeI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_shl{..} -> I_shl { flagW = flagW
                           , typeI = typeI
                           , operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        I_lshr{..} -> I_lshr { flagE = flagE
                             , typeI = typeI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_ashr{..} -> I_ashr { flagE = flagE
                             , typeI = typeI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_and{..} -> I_and { typeI = typeI
                           , operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        I_or{..} -> I_or { typeI = typeI
                         , operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        I_xor{..} -> I_xor { typeI = typeI
                           , operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        I_add_V{..} -> I_add_V { flagI = flagI
                               , typeVI = typeVI
                               , operand1 = substitute chg operand1
                               , operand2 = substitute chg operand2
                               , result = cid result
                               }
        I_sub_V{..} -> I_sub_V { flagI = flagI
                               , typeVI = typeVI
                               , operand1 = substitute chg operand1
                               , operand2 = substitute chg operand2
                               , result = cid result
                               }
        I_mul_V{..} -> I_mul_V { flagI = flagI
                               , typeVI = typeVI
                               , operand1 = substitute chg operand1
                               , operand2 = substitute chg operand2
                               , result = cid result
                               }
        I_udiv_V{..} -> I_udiv_V { flagE = flagE
                                 , typeVI = typeVI
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_sdiv_V{..} -> I_sdiv_V { flagE = flagE
                                 , typeVI = typeVI
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_urem_V{..} -> I_urem_V { typeVI = typeVI
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_srem_V{..} -> I_srem_V { typeVI = typeVI
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_shl_V{..} -> I_shl_V { flagW = flagW
                               , typeVI = typeVI
                               , operand1 = substitute chg operand1
                               , operand2 = substitute chg operand2
                               , result = cid result
                               }
        I_lshr_V{..} -> I_lshr_V { flagE = flagE
                                 , typeVI = typeVI
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_ashr_V{..} -> I_ashr_V { flagE = flagE
                                 , typeVI = typeVI
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_and_V{..} -> I_and_V { typeVI = typeVI
                               , operand1 = substitute chg operand1
                               , operand2 = substitute chg operand2
                               , result = cid result
                               }
        I_or_V{..} -> I_or_V { typeVI = typeVI
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_xor_V{..} -> I_xor_V { typeVI = typeVI
                               , operand1 = substitute chg operand1
                               , operand2 = substitute chg operand2
                               , result = cid result
                               }
        I_fadd{..} -> I_fadd { flagF = flagF
                             , typeF = typeF
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_fsub{..} -> I_fsub { flagF = flagF
                             , typeF = typeF
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_fmul{..} -> I_fmul { flagF = flagF
                             , typeF = typeF
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_fdiv{..} -> I_fdiv { flagF = flagF
                             , typeF = typeF
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_frem{..} -> I_frem { flagF = flagF
                             , typeF = typeF
                             , operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        I_fadd_V{..} -> I_fadd_V { flagF = flagF
                                 , typeVF = typeVF
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_fsub_V{..} -> I_fsub_V { flagF = flagF
                                 , typeVF = typeVF
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_fmul_V{..} -> I_fmul_V { flagF = flagF
                                 , typeVF = typeVF
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_fdiv_V{..} -> I_fdiv_V { flagF = flagF
                                 , typeVF = typeVF
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_frem_V{..} -> I_frem_V { flagF = flagF
                                 , typeVF = typeVF
                                 , operand1 = substitute chg operand1
                                 , operand2 = substitute chg operand2
                                 , result = cid result
                                 }
        I_trunc{..} -> I_trunc { srcI = (substitute chg) srcI
                               , toI = toI
                               , result = cid result
                               }
        I_zext{..} -> I_zext { srcI = (substitute chg) srcI
                             , toI = toI                                      
                             , result = cid result
                             }
        I_sext{..} -> I_sext { srcI = (substitute chg) srcI
                             , toI = toI                                      
                             , result = cid result
                             }
        I_fptrunc{..} -> I_fptrunc { srcF = (substitute chg) srcF
                                   , toF = toF
                                   , result = cid result
                                   }
        I_fpext{..} -> I_fpext { srcF = (substitute chg) srcF
                               , toF = toF                                        
                               , result = cid result
                               }
        I_fptoui{..} -> I_fptoui { srcF = (substitute chg) srcF
                                 , toI = toI
                                 , result = cid result
                                 }
        I_fptosi{..} -> I_fptosi { srcF = (substitute chg) srcF
                                 , toI = toI                                          
                                 , result = cid result
                                 }
        I_uitofp{..} -> I_uitofp { srcI = (substitute chg) srcI
                                 , toF = toF                                          
                                 , result = cid result
                                 }
        I_sitofp{..} -> I_sitofp { srcI = (substitute chg) srcI
                                 , toF = toF                                          
                                 , result = cid result
                                 }
        I_ptrtoint{..} -> I_ptrtoint { srcP = (substitute chg) srcP
                                     , toI = toI                                              
                                     , result = cid result
                                     }
        I_inttoptr{..} -> I_inttoptr { srcI = (substitute chg) srcI
                                     , toP = toP
                                     , result = cid result
                                     }
        I_addrspacecast{..} -> I_addrspacecast { srcP = (substitute chg) srcP
                                               , toP = toP
                                               , result = cid result
                                               }
        I_bitcast{..} -> I_bitcast { srcP = (substitute chg) srcP
                                   , toP = toP
                                   , result = cid result
                                   }
        I_bitcast_D{..} -> I_bitcast_D { srcD = (substitute chg) srcD
                                       , toD = toD
                                       , result = cid result
                                       }
        I_trunc_V{..} -> I_trunc_V { srcVI = (substitute chg) srcVI
                                   , toVI = toVI
                                   , result = cid result
                                   }
        I_zext_V{..} -> I_zext_V { srcVI = (substitute chg) srcVI
                                 , toVI = toVI
                                 , result = cid result
                                 }
        I_sext_V{..} -> I_sext_V { srcVI = (substitute chg) srcVI
                                 , toVI = toVI
                                 , result = cid result
                                 }
        I_fptrunc_V{..} -> I_fptrunc_V { srcVF = (substitute chg) srcVF
                                       , toVF = toVF
                                       , result = cid result
                                       }
        I_fpext_V{..} -> I_fpext_V { srcVF = (substitute chg) srcVF
                                   , toVF = toVF
                                   , result = cid result
                                   }
        I_fptoui_V{..} -> I_fptoui_V { srcVF = (substitute chg) srcVF
                                     , toVI = toVI
                                     , result = cid result
                                     }
        I_fptosi_V{..} -> I_fptosi_V { srcVF = (substitute chg) srcVF
                                     , toVI = toVI
                                     , result = cid result
                                     }
        I_uitofp_V{..} -> I_uitofp_V { srcVI = (substitute chg) srcVI
                                     , toVF = toVF                                               
                                     , result = cid result
                                     }
        I_sitofp_V{..} -> I_sitofp_V { srcVI = (substitute chg) srcVI
                                     , toVF = toVF
                                     , result = cid result
                                     }
        I_ptrtoint_V{..} -> I_ptrtoint_V { srcVP = (substitute chg) srcVP
                                         , toVI = toVI  
                                         , result = cid result
                                         }
        I_inttoptr_V{..} -> I_inttoptr_V { srcVI = (substitute chg) srcVI
                                         , toVP = toVP
                                         , result = cid result
                                         }
        I_addrspacecast_V{..} -> I_addrspacecast_V { srcVP = (substitute chg) srcVP
                                                   , toVP = toVP                                                             
                                                   , result = cid result
                                                   }
        I_select_I{..} -> I_select_I { cond = (substitute chg) cond
                                     , trueI = (substitute chg) trueI
                                     , falseI = (substitute chg) falseI
                                     , result = cid result
                                     }
        I_select_F{..} -> I_select_F { cond = (substitute chg) cond
                                     , trueF = (substitute chg) trueF
                                     , falseF = (substitute chg) falseF
                                     , result = cid result
                                     }
        I_select_P{..} -> I_select_P { cond = (substitute chg) cond
                                     , trueP = (substitute chg) trueP
                                     , falseP = (substitute chg) falseP
                                     , result = cid result
                                     }
        I_select_VI{..} -> I_select_VI { condVI = substitute chg condVI
                                       , trueVI = (substitute chg) trueVI
                                       , falseVI = (substitute chg) falseVI
                                       , result = cid result
                                       }
        I_select_VF{..} -> I_select_VF { condVF = substitute chg condVF
                                       , trueVF = (substitute chg) trueVF
                                       , falseVF = (substitute chg) falseVF
                                       , result = cid result
                                       }
        I_select_VP{..} -> I_select_VP { condV = substitute chg condV
                                       , trueVP = (substitute chg) trueVP
                                       , falseVP = (substitute chg) falseVP
                                       , result = cid result
                                       }
        I_select_First{..} -> I_select_First { cond = (substitute chg) cond
                                             , trueFirst = (substitute chg) trueFirst
                                             , falseFirst = (substitute chg) falseFirst
                                             , result = cid result
                                             }
        I_va_arg{..} -> I_va_arg { dv = (substitute chg) dv
                                 , typeD = typeD
                                 , result = cid result
                                 }
        I_llvm_va_start{..} -> I_llvm_va_start { arglist = substitute chg arglist }
        I_llvm_va_end{..} -> I_llvm_va_end { arglist = substitute chg arglist }
        I_llvm_va_copy{..} -> I_llvm_va_copy { destarglist = substitute chg destarglist
                                  , srcarglist = substitute chg srcarglist
                                  }
        I_llvm_gcroot{..} -> I_llvm_gcroot { ptrloc = substitute chg ptrloc
                                 , metadata = substitute chg metadata
                                 }
        I_llvm_gcread{..} -> errorLoc FLC "unsupport"
        I_llvm_gcwrite{..} -> errorLoc FLC "unsupport"
        I_llvm_returnaddress{..} -> I_llvm_returnaddress { level = substitute chg level }
        I_llvm_frameaddress{..} -> I_llvm_frameaddress { level = substitute chg level }
        I_llvm_frameescape vs -> I_llvm_frameescape (substitute chg vs)
        I_llvm_framerecover{..} -> errorLoc FLC "unsupport"
        I_llvm_read_register {..} -> I_llvm_read_register { memLen = memLen
                                                          , meta = substitute chg meta
                                                          , result = substitute chg result 
                                                          }
        I_llvm_write_register {..} -> I_llvm_write_register { memLen = memLen
                                                            , meta = substitute chg meta
                                                            , value = substitute chg value 
                                                            }
        I_llvm_stacksave{..} -> I_llvm_stacksave { result = cid result }
        I_llvm_stackrestore{..} -> I_llvm_stackrestore { pointer = (substitute chg) pointer }
        I_llvm_prefetch{..} -> errorLoc FLC "unsupport"
        I_llvm_pcmarker{..} -> errorLoc FLC "unsupport"
        I_llvm_readcyclecounter{..} -> I_llvm_readcyclecounter { result = cid result}
        I_llvm_clear_cache {..} -> errorLoc FLC "unsupport"
        I_llvm_instprof_increment{..} -> errorLoc FLC "unsupport"
        I_llvm_memcpy{..} -> I_llvm_memcpy { memlen = memlen
                                           , dest = (substitute chg) dest
                                           , src = (substitute chg) src
                                           , len = (substitute chg) len
                                           , align = (substitute chg) align
                                           , isvolatile = (substitute chg) isvolatile
                                           }
        I_llvm_memmove{..} -> I_llvm_memmove { memlen = memlen
                                             , dest = (substitute chg) dest
                                             , src = (substitute chg) src
                                             , len = (substitute chg) len
                                             , align = (substitute chg) align
                                             , isvolatile = (substitute chg) isvolatile
                                             }
        I_llvm_memset{..} -> I_llvm_memset { memlen = memlen
                                           , dest = (substitute chg) dest
                                           , setValue = substitute chg setValue
                                           , len = (substitute chg) len
                                           , align = (substitute chg) align
                                           , isvolatile = (substitute chg) isvolatile
                                           }
        I_llvm_ctpop{..} -> I_llvm_ctpop { suffix = suffix
                                         , dv = substitute chg dv
                                         , result = cid result
                                         }
        I_llvm_lifetime_start {..} -> I_llvm_lifetime_start { objsize = substitute chg objsize
                                                            , pointer = substitute chg pointer
                                                            }
        I_llvm_lifetime_end {..} -> I_llvm_lifetime_end { objsize = substitute chg objsize
                                                        , pointer = substitute chg pointer
                                                        }                                      
                           {-
  I_llvm_math_f32 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  I_llvm_math_f64 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  I_llvm_math_f80 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  I_llvm_math_f128 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  I_llvm_math_ppcf128 :: MathUnaryOp -> Value -> LocalId -> Cinst;

  I_llvm_powi_f32 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_powi_f64 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_powi_f80 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_powi_f128 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_powi_ppcf128 :: Value -> Value -> LocalId -> Cinst;

  I_llvm_pow_f32 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_pow_f64 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_pow_f80 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_pow_f128 :: Value -> Value -> LocalId -> Cinst;
  I_llvm_pow_ppcf128 :: Value -> Value -> LocalId -> Cinst;

  I_llvm_bitset_test :: Value -> Value -> LocalId -> Cinst;
  I_llvm_donothing :: Cinst;
  -}


instance Substitutable (Minst g) g (Minst h) h where
  substitute chg mi = case mi of
    Minst cs g mps -> Minst cs (substitute chg g) (substitute chg mps) 
    M_llvm_dbg_declare m1 m2 -> M_llvm_dbg_declare (substitute chg m1) (substitute chg m2)
    M_llvm_dbg_func_start m -> M_llvm_dbg_func_start (substitute chg m)
    M_llvm_dbg_stoppoint m1 m2 m3 -> M_llvm_dbg_stoppoint (substitute chg m1) (substitute chg m2) (substitute chg m3)
    M_llvm_dbg_value m1 m2 m3 -> M_llvm_dbg_value (substitute chg m1) (substitute chg m2) (substitute chg m3)    
    M_llvm_dbg_region_end m -> M_llvm_dbg_region_end (substitute chg m)

instance Substitutable (MetaOperand g) g (MetaOperand h) h where
  substitute chg mp = case mp of
    MetaOperandMeta x -> MetaOperandMeta $ substitute chg x
    MetaOperandData dt pa ma v -> MetaOperandData dt pa ma (substitute chg v)

instance Substitutable (Tinst g) g (Tinst h) h where
  substitute chg x = case x of
    T_unreachable -> T_unreachable
    T_ret_void -> T_ret_void
    T_return tvs ->  T_return (substitute chg tvs)
    T_br l -> T_br l
    T_cbr cnd t f -> T_cbr (substitute chg cnd) t f
    T_indirectbr tv ls -> T_indirectbr (substitute chg tv) ls
    T_switch d o -> T_switch (substitute chg d) (substitute chg o)
    t@T_invoke{..} -> t { invoke_ptr = substitute chg invoke_ptr
                        , invoke_fun_interface = substitute chg invoke_fun_interface
                        , invoke_return = substitute chg invoke_return
                        }
    t@T_invoke_asm{..} -> t { invoke_asm_interface = substitute chg invoke_asm_interface
                            , invoke_return = substitute chg invoke_return
                            }
    T_resume (T dt v) -> T_resume (T dt (substitute chg v))
    T_unwind -> T_unwind

instance Substitutable a g b h => Substitutable (FunSignature a) g (FunSignature b) h where
  substitute chg x@FunSignature{..} = x { fs_params = substitute chg fs_params }
                                      
instance Substitutable (CallFunInterface g) g (CallFunInterface h) h where
  substitute chg x@CallFunInterface{..} = x { cfi_signature = substitute chg cfi_signature }
  
instance Substitutable (InvokeFunInterface g) g (InvokeFunInterface h) h where
  substitute chg x@InvokeFunInterface{..} = x { ifi_signature = substitute chg ifi_signature }

instance Substitutable (CallAsmInterface g) g (CallAsmInterface h) h where
  substitute chg x@CallAsmInterface{..} = x { cai_actualParams = substitute chg cai_actualParams}

instance Substitutable a g b h => Substitutable (FunOperand a) g (FunOperand b) h where
  substitute chg c = case c of
    FunOperandData dt pa ma v -> FunOperandData dt pa ma (substitute chg v)
    FunOperandExt e dt pa ma v -> FunOperandExt e dt pa ma (substitute chg v)
    FunOperandByVal dt pa ma v -> FunOperandByVal dt pa ma (substitute chg v)
    FunOperandLabel t pa ma v -> FunOperandLabel t pa ma (substitute chg v)
    FunOperandAsRet dt pa ma v -> FunOperandAsRet dt pa ma (substitute chg v)

instance Substitutable Fparam g Fparam h where
  substitute chg fp = case fp of
    FimplicitParam -> fp
    FexplicitParam x -> FexplicitParam (substitute chg x)

instance Substitutable (Dbg g) g (Dbg h) h where
  substitute chg (Dbg mv mc) = Dbg mv (substitute chg mc)

instance Substitutable (TlGlobal g) g (TlGlobal h) h where
  substitute chg tg = case tg of
    tl@TlGlobalDtype{..} -> tl { tlg_lhs = substitute chg tlg_lhs
                               , tlg_const = substitute chg tlg_const
                               , tlg_comdat = substitute chg tlg_comdat
                               }
    tl@TlGlobalOpaque{..} -> tl { tlg_lhs = substitute chg tlg_lhs
                                , tlg_const = substitute chg tlg_const
                                , tlg_comdat = substitute chg tlg_comdat
                                }

instance Substitutable (TlIntrinsic g) g (TlIntrinsic h) h where
  substitute chg tli = case tli of
    x@TlIntrinsic_llvm_used{..}  -> x { tli_const = substitute chg tli_const }
    x@TlIntrinsic_llvm_compiler_used{..}  -> x { tli_const = substitute chg tli_const }
    x@TlIntrinsic_llvm_global_ctors{..}  -> x { tli_const = substitute chg tli_const }
    x@TlIntrinsic_llvm_global_dtors{..}  -> x { tli_const = substitute chg tli_const }

instance Substitutable a g b h => Substitutable (Toplevel g a) g (Toplevel h b) h where
  substitute chg@Changer{..} tpl = case tpl of
    ToplevelAlias x -> ToplevelAlias (substitute chg x)
    ToplevelUnamedMd x -> ToplevelUnamedMd (substitute chg x)
    ToplevelNamedMd x -> ToplevelNamedMd (substitute chg x)
    ToplevelDeclare x -> ToplevelDeclare (substitute chg x)
    ToplevelDefine x -> ToplevelDefine (substitute chg x)
    ToplevelGlobal x -> ToplevelGlobal (substitute chg x)
    ToplevelTypeDef x -> ToplevelTypeDef (substitute chg x)
    ToplevelDepLibs x -> ToplevelDepLibs (substitute chg x)
    ToplevelUnamedType x -> ToplevelUnamedType (substitute chg x)
    ToplevelModuleAsm x -> ToplevelModuleAsm (substitute chg x)
    ToplevelAttribute x -> ToplevelAttribute (substitute chg x)
    ToplevelComdat x -> ToplevelComdat (substitute chg x)
    ToplevelIntrinsic x -> ToplevelIntrinsic (substitute chg x)


instance Substitutable TlTypeDef g TlTypeDef h  where
  substitute chg = id

instance Substitutable (TlComdat g) g (TlComdat h) h where
  substitute chg (TlComdat lhs v) = TlComdat (substitute chg lhs) v

instance Substitutable (Comdat g) g (Comdat h) h where
  substitute chg (Comdat x) = Comdat (substitute chg x)

instance Substitutable TlAttribute g TlAttribute h where
  substitute chg = id

instance Substitutable TlModuleAsm g TlModuleAsm h where
  substitute chg = id

instance Substitutable TlUnamedType g TlUnamedType h where
  substitute chg = id

instance Substitutable TlDepLibs g TlDepLibs h where
  substitute chg = id

instance Substitutable g g h h where
  substitute chg = change_GlobalId chg

instance Substitutable (TlAlias g) g (TlAlias h) h where
  substitute chg tla@TlAlias{..} = tla { tla_lhs = substitute chg tla_lhs
                                       , tla_aliasee = substitute chg tla_aliasee
                                       }

instance Substitutable v g w h => Substitutable (Conversion s v) g (Conversion s w) h where
  substitute chg cv = case cv of
    Trunc x t -> Trunc (substitute chg x) t
    Zext x t -> Zext (substitute chg x) t
    Sext x t -> Sext (substitute chg x) t
    FpTrunc x t -> FpTrunc (substitute chg x) t
    FpExt x t -> FpExt (substitute chg x) t
    FpToUi x t -> FpToUi (substitute chg x) t
    FpToSi x t -> FpToSi (substitute chg x) t
    UiToFp x t -> UiToFp (substitute chg x) t
    SiToFp x t -> SiToFp (substitute chg x) t
    PtrToInt x t -> PtrToInt (substitute chg x) t
    IntToPtr x t -> IntToPtr (substitute chg x) t
    Bitcast x t -> Bitcast (substitute chg x) t
    AddrSpaceCast x t -> AddrSpaceCast (substitute chg x) t

instance Substitutable (Aliasee g) g (Aliasee h) h where
  substitute chg al = case al of
    Aliasee x -> Aliasee (substitute chg x)
    AliaseeTyped d x -> AliaseeTyped d (substitute chg x)    
    AliaseeConversion x -> AliaseeConversion (substitute chg x)
    AliaseeGEP x -> AliaseeGEP (substitute chg x)

instance Substitutable TlNamedMd g TlNamedMd h where
  substitute chg x = case x of
    TlNamedMd mv mns -> TlNamedMd mv (substitute chg mns)

instance Substitutable MdName g MdName h where
  substitute _ = id

instance Substitutable MdNode g MdNode h where
  substitute _ = id

instance Substitutable (TlUnamedMd g) g (TlUnamedMd h) h where
  substitute chg x = case x of
    TlUnamedMd s mc -> TlUnamedMd s (substitute chg mc)
    TlUnamedMd_Tagged n tag lst -> TlUnamedMd_Tagged n tag (substitute chg lst)

instance Substitutable (MetaKindedConst g) g (MetaKindedConst h) h where
  substitute chg mk = case mk of
    MetaKindedConst m mc -> MetaKindedConst m (substitute chg mc)
    UnmetaKindedNull -> UnmetaKindedNull

instance Substitutable (FunPtr g) g (FunPtr h) h where
  substitute chg@Changer{..} fp = case fp of
    FunId g -> FunId (substitute chg g)
    FunSsa l -> FunSsa (substitute chg l)
    FunIdBitcast (T st c) dt -> FunIdBitcast (T st (substitute chg c)) dt
    FunIdInttoptr (T st c) dt -> FunIdInttoptr (T st (substitute chg c)) dt
    Fun_null -> Fun_null
    Fun_undef -> Fun_undef


instance Substitutable (MetaConst g) g (MetaConst h) h where
  substitute chg mc = case mc of
    McStruct l -> McStruct (substitute chg l)
    McString x -> McString x
    McMdRef x -> McMdRef x
    McSsa s -> McSsa (substitute chg s)
    McSimple c -> McSimple (substitute chg c)

instance Substitutable a g b h => Substitutable (Module g a) g (Module h b) h where
  substitute chg (Module l) = Module $ substitute chg l

instance Substitutable g g g g where
  substitute _ = id

instance (Ord g, Ord k, Ord h) => Substitutable (M.Map (g, k) v) g (M.Map (h, k) v) h where
  substitute chg m = M.mapKeys (\(gid, k) -> (substitute chg gid, k)) m

instance Substitutable (Prefix g) g (Prefix h) h where
  substitute chg (Prefix x) = Prefix $ substitute chg x

instance Substitutable (Prologue g) g (Prologue h) h where
  substitute chg (Prologue x) = Prologue $ substitute chg x
