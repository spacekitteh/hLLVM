{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards,  TypeSynonymInstances, FlexibleInstances #-}
module Llvm.Pass.Substitution where

import Control.Monad
import Data.Maybe
import Prelude hiding (succ)

import qualified Compiler.Hoopl as H

import Llvm.Hir.Data
import Llvm.Query.HirType
import Llvm.Util.Monadic (maybeM)
import Debug.Trace
import Control.Monad
import qualified Data.Map as M

import Llvm.Pass.Changer

class Substitutable a where
  substitute :: Changer -> a -> a

instance Substitutable a => Substitutable (Maybe a) where
  substitute chg (Just a) = Just $ substitute chg a
  substitute chg Nothing = Nothing

instance Substitutable a => Substitutable [a] where
  substitute chg l = fmap (substitute chg) l

instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
  substitute chg (Left a) = Left $ substitute chg a
  substitute chg (Right a) = Right $ substitute chg a

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  substitute chg (a, b) = (substitute chg a, substitute chg b)

instance Substitutable TypedConstOrNull where
  substitute chg@Changer{..} x = case x of
    TypedConst (T dt c) -> TypedConst (T dt (substitute chg c))
    UntypedNull -> x

instance Substitutable Value where
  substitute chg@Changer{..} cst = case cst of
    Val_ssa lid -> Val_ssa $ substitute chg lid
    Val_const c -> Val_const $ substitute chg c

instance Substitutable Label where
  substitute _ = id

instance Substitutable v => Substitutable (Select s r v) where
  substitute chg (Select cnd t f) =
    Select (substitute chg cnd) (substitute chg t) (substitute chg f)

instance Substitutable v => Substitutable (GetElementPtr s v) where
  substitute chg (GetElementPtr b base indices) =
    GetElementPtr b (substitute chg base) (substitute chg indices)

instance Substitutable v => Substitutable (Icmp s v) where
  substitute chg (Icmp op t v1 v2) =
    Icmp op t (substitute chg v1) (substitute chg v2)

instance Substitutable v => Substitutable (Fcmp s v) where
  substitute chg (Fcmp op t v1 v2) =
    Fcmp op t (substitute chg v1) (substitute chg v2)

instance Substitutable v => Substitutable (ShuffleVector r v) where
  substitute chg (ShuffleVector v1 v2 i) =
    ShuffleVector (substitute chg v1) (substitute chg v2) (substitute chg i)

instance Substitutable v => Substitutable (ExtractElement r v) where
  substitute chg (ExtractElement v i) =
    ExtractElement (substitute chg v) (substitute chg i)


instance Substitutable v => Substitutable (InsertElement r v) where
  substitute chg (InsertElement v e i) =
    InsertElement (substitute chg v) (substitute chg e) (substitute chg i)

instance Substitutable v => Substitutable (ExtractValue v) where
  substitute chg (ExtractValue v ns) = ExtractValue (substitute chg v) ns

instance Substitutable v => Substitutable (InsertValue v) where
  substitute chg (InsertValue v e ns) =
    InsertValue (substitute chg v) (substitute chg e) ns


instance Substitutable Const where
  substitute chg@Changer{..} cst =
    let change2c cf a b = cf (substitute chg a) (substitute chg b)
        cst1 = case cst of
          C_u8 _ -> cst
          C_u16 _ -> cst
          C_u32 _ -> cst
          C_u64 _ -> cst
          C_u96 _ -> cst
          C_u128 _ -> cst
          C_s8 _ -> cst
          C_s16 _ -> cst
          C_s32 _ -> cst
          C_s64 _ -> cst
          C_s96 _ -> cst
          C_s128 _ -> cst
          C_int _ -> cst
          C_uhex_int _ -> cst
          C_shex_int _ -> cst
          C_float _ -> cst
          C_true -> cst
          C_false -> cst
          C_null -> cst
          C_undef -> cst
          C_zeroinitializer -> cst
          C_str _ -> cst
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


instance Substitutable x => Substitutable (T t x) where
  substitute chg (T t a) = T t (substitute chg a)

instance Substitutable FunctionInterface where
  substitute chg fp@FunctionInterface {..} =
    fp { fi_fun_name = substitute chg fi_fun_name
       , fi_param_list = substitute chg fi_param_list
       , fi_comdat = substitute chg fi_comdat
       , fi_prefix = substitute chg fi_prefix
       , fi_prologue = substitute chg fi_prologue
       }

instance Substitutable FunctionDeclareType where
  substitute chg fp@FunctionDeclareType {..} =
    fp { fd_fun_name = substitute chg fd_fun_name
       , fd_param_list = substitute chg fd_param_list
       , fd_comdat = substitute chg fd_comdat
       , fd_prefix = substitute chg fd_prefix
       , fd_prologue = substitute chg fd_prologue
       }

instance Substitutable TlDeclare where
  substitute chg (TlDeclare fp) = TlDeclare (substitute chg fp)

instance Substitutable a => Substitutable (TlDefine a) where
  substitute chg (TlDefine fp e g) =
    TlDefine (substitute chg fp) e (H.mapGraph (substitute chg) g)

instance Substitutable LocalId where
  substitute chg = change_LocalId chg

instance Substitutable Pinst where
  substitute chg p@Pinst{..} = p { flowins = substitute chg flowins
                                 , flowout = substitute chg flowout
                                 }

instance Substitutable a => Substitutable (Node a e x) where
  substitute chg node = case node of
    Lnode _ -> node
    Pnode x dbgs -> Pnode (substitute chg x) (substitute chg dbgs)
    Cnode x dbgs -> Cnode (substitute chg x) (substitute chg dbgs)
    Mnode x dbgs -> Mnode (substitute chg x) (substitute chg dbgs)
    Comment _ -> node
    Enode x -> Enode (substitute chg x)
    Tnode ti dbgs -> Tnode (substitute chg ti) (substitute chg dbgs)

instance Substitutable Clause where
  substitute chg c = case c of
    Catch tv -> Catch (substitute chg tv)
    Filter tcn -> Filter (substitute chg tcn)

instance Substitutable Cinst where
  substitute chg x =
    let cid = change_LocalId chg
    in case x of
        i@I_alloca{..} -> i { size = substitute chg size
                            , result = cid result
                            }
        i@I_load{..} -> i { pointer = (substitute chg) pointer
                          , result = substitute chg result
                          }
        i@I_loadatomic{..} -> i { pointer = substitute chg pointer
                                , result = substitute chg result
                                }
        i@I_store{..} -> i { storedvalue = substitute chg storedvalue
                           , pointer = substitute chg pointer
                           }
        i@I_storeatomic{..} -> i { storedvalue = substitute chg storedvalue
                                 , pointer = substitute chg pointer
                                 }
        i@I_fence{..} -> i
        i@I_cmpxchg_I{..} -> i { pointer = substitute chg pointer
                               , cmpi = substitute chg cmpi
                               , newi = substitute chg newi
                               , result = substitute chg result
                               }
        i@I_cmpxchg_F{..} -> i { pointer = substitute chg pointer
                               , cmpf = substitute chg cmpf
                               , newf = substitute chg newf
                               , result = substitute chg result
                               }
        i@I_cmpxchg_P{..} -> i { pointer = substitute chg pointer
                               , cmpp = substitute chg cmpp
                               , newp = substitute chg newp
                               , result = substitute chg result
                               }
        i@I_atomicrmw{..} -> i { pointer = substitute chg pointer
                               , val = substitute chg val
                               , result = substitute chg result
                               }
        i@I_call_fun{..} -> i { call_ptr = substitute chg call_ptr
                              , call_fun_interface = substitute chg call_fun_interface
                              , call_return = substitute chg call_return
                              }
        i@I_call_asm{..} -> i { call_asm_interface = substitute chg call_asm_interface
                              , call_return = substitute chg call_return
                              }
        i@I_extractelement_I{..} -> i { vectorI = substitute chg vectorI
                                      , index = substitute chg index
                                      , result = substitute chg result
                                      }
        i@I_extractelement_F{..} -> i { vectorF = substitute chg vectorF
                                      , index = substitute chg index
                                      , result = substitute chg result
                                      }
        i@I_extractelement_P{..} -> i { vectorP = substitute chg vectorP
                                      , index = substitute chg index
                                      , result = substitute chg result
                                      }
        i@I_insertelement_I{..} -> i { vectorI = (substitute chg) vectorI
                                     , elementI = (substitute chg) elementI
                                     , index = (substitute chg) index
                                     , result = cid result
                                     }
        i@I_insertelement_F{..} -> i { vectorF = (substitute chg) vectorF
                                     , elementF = (substitute chg) elementF
                                     , index = (substitute chg) index
                                     , result = cid result
                                     }
        i@I_insertelement_P{..} -> i { vectorP = (substitute chg) vectorP
                                     , elementP = (substitute chg) elementP
                                     , index = (substitute chg) index
                                     , result = cid result
                                     }
        i@I_shufflevector_I{..} -> i { vector1I = (substitute chg) vector1I
                                     , vector2I = (substitute chg) vector2I
                                     , vectorIdx = (substitute chg) vectorIdx
                                     , result = cid result
                                     }
        i@I_shufflevector_F{..} -> i { vector1F = (substitute chg) vector1F
                                     , vector2F = (substitute chg) vector2F
                                     , vectorIdx = (substitute chg) vectorIdx
                                     , result = cid result
                                     }
        i@I_shufflevector_P{..} -> i { vector1P = (substitute chg) vector1P
                                     , vector2P = (substitute chg) vector2P
                                     , vectorIdx = (substitute chg) vectorIdx
                                     , result = cid result
                                     }
        i@I_extractvalue{..} -> i { record = (substitute chg) record
                                  , result = cid result
                                  }
        i@I_insertvalue{..} -> i { record = (substitute chg) record
                                 , element = (substitute chg) element
                                 , result = cid result
                                 }
        i@I_landingpad{..} -> i { persFn = substitute chg persFn
                                , clauses = substitute chg clauses
                                , result = cid result
                                }
        i@I_getelementptr{..} -> i { pointer = (substitute chg) pointer
                                   , indices = substitute chg indices
                                   , result = cid result
                                   }
        i@I_getelementptr_V{..} -> i { vpointer = (substitute chg) vpointer
                                     , vindices = substitute chg vindices
                                     , result = cid result
                                     }
        i@I_icmp{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_icmp_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_fcmp{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_fcmp_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_add{..} -> i { operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        i@I_sub{..} -> i { operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        i@I_mul{..} -> i { operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        i@I_udiv{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_sdiv{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_urem{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_srem{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_shl{..} -> i { operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        i@I_lshr{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_ashr{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_and{..} -> i { operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        i@I_or{..} -> i { operand1 = substitute chg operand1
                        , operand2 = substitute chg operand2
                        , result = cid result
                        }
        i@I_xor{..} -> i { operand1 = substitute chg operand1
                         , operand2 = substitute chg operand2
                         , result = cid result
                         }
        i@I_add_V{..} -> i { operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        i@I_sub_V{..} -> i { operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        i@I_mul_V{..} -> i { operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        i@I_udiv_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_sdiv_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_urem_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_srem_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_shl_V{..} -> i {  operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        i@I_lshr_V{..} -> i {  operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_ashr_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_and_V{..} -> i { operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        i@I_or_V{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_xor_V{..} -> i { operand1 = substitute chg operand1
                           , operand2 = substitute chg operand2
                           , result = cid result
                           }
        i@I_fadd{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_fsub{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_fmul{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_fdiv{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_frem{..} -> i { operand1 = substitute chg operand1
                          , operand2 = substitute chg operand2
                          , result = cid result
                          }
        i@I_fadd_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@ I_fsub_V{..} -> i { operand1 = substitute chg operand1
                             , operand2 = substitute chg operand2
                             , result = cid result
                             }
        i@I_fmul_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_fdiv_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_frem_V{..} -> i { operand1 = substitute chg operand1
                            , operand2 = substitute chg operand2
                            , result = cid result
                            }
        i@I_trunc{..} -> i { srcI = (substitute chg) srcI
                           , result = cid result
                           }
        i@I_zext{..} -> i { srcI = (substitute chg) srcI
                          , result = cid result
                          }
        i@I_sext{..} -> i { srcI = (substitute chg) srcI
                          , result = cid result
                          }
        i@I_fptrunc{..} -> i { srcF = (substitute chg) srcF
                             , result = cid result
                             }
        i@I_fpext{..} -> i { srcF = (substitute chg) srcF
                           , result = cid result
                           }
        i@I_fptoui{..} -> i { srcF = (substitute chg) srcF
                            , result = cid result
                            }
        i@I_fptosi{..} -> i { srcF = (substitute chg) srcF
                            , result = cid result
                            }
        i@I_uitofp{..} -> i { srcI = (substitute chg) srcI
                            , result = cid result
                            }
        i@I_sitofp{..} -> i { srcI = (substitute chg) srcI
                            , result = cid result
                            }
        i@I_ptrtoint{..} -> i { srcP = (substitute chg) srcP
                              , result = cid result
                              }
        i@I_inttoptr{..} -> i { srcI = (substitute chg) srcI
                              , result = cid result
                              }
        i@I_addrspacecast{..} -> i { srcP = (substitute chg) srcP
                                   , result = cid result
                                   }
        i@I_bitcast{..} -> i { srcP = (substitute chg) srcP
                             , result = cid result
                             }
        i@I_bitcast_D{..} -> i { srcD = (substitute chg) srcD
                               , result = cid result
                               }
        i@I_trunc_V{..} -> i { srcVI = (substitute chg) srcVI
                             , result = cid result
                             }
        i@I_zext_V{..} -> i { srcVI = (substitute chg) srcVI
                            , result = cid result
                            }
        i@I_sext_V{..} -> i { srcVI = (substitute chg) srcVI
                            , result = cid result
                            }
        i@I_fptrunc_V{..} -> i { srcVF = (substitute chg) srcVF
                               , result = cid result
                               }
        i@I_fpext_V{..} -> i { srcVF = (substitute chg) srcVF
                             , result = cid result
                             }
        i@I_fptoui_V{..} -> i { srcVF = (substitute chg) srcVF
                              , result = cid result
                              }
        i@I_fptosi_V{..} -> i { srcVF = (substitute chg) srcVF
                              , result = cid result
                              }
        i@I_uitofp_V{..} -> i { srcVI = (substitute chg) srcVI
                              , result = cid result
                              }
        i@I_sitofp_V{..} -> i { srcVI = (substitute chg) srcVI
                              , result = cid result
                              }
        i@I_ptrtoint_V{..} -> i { srcVP = (substitute chg) srcVP
                                , result = cid result
                                }
        i@I_inttoptr_V{..} -> i { srcVI = (substitute chg) srcVI
                                , result = cid result
                                }
        i@I_addrspacecast_V{..} -> i { srcVP = (substitute chg) srcVP
                                     , result = cid result
                                     }
        i@I_select_I{..} -> i { cond = (substitute chg) cond
                              , trueI = (substitute chg) trueI
                              , falseI = (substitute chg) falseI
                              , result = cid result
                              }
        i@I_select_F{..} -> i { cond = (substitute chg) cond
                              , trueF = (substitute chg) trueF
                              , falseF = (substitute chg) falseF
                              , result = cid result
                              }
        i@I_select_P{..} -> i { cond = (substitute chg) cond
                              , trueP = (substitute chg) trueP
                              , falseP = (substitute chg) falseP
                              , result = cid result
                              }
        i@I_select_VI{..} -> i { condVI = substitute chg condVI
                               , trueVI = (substitute chg) trueVI
                               , falseVI = (substitute chg) falseVI
                               , result = cid result
                               }
        i@I_select_VF{..} -> i { condVF = substitute chg condVF
                               , trueVF = (substitute chg) trueVF
                               , falseVF = (substitute chg) falseVF
                               , result = cid result
                               }
        i@I_select_VP{..} -> i { condV = substitute chg condV
                               , trueVP = (substitute chg) trueVP
                               , falseVP = (substitute chg) falseVP
                               , result = cid result
                               }
        i@I_select_First{..} -> i { cond = (substitute chg) cond
                                  , trueFirst = (substitute chg) trueFirst
                                  , falseFirst = (substitute chg) falseFirst
                                  , result = cid result
                                  }
        i@I_va_arg{..} -> i { dv = (substitute chg) dv
                            , result = cid result
                            }
        i@I_llvm_va_start{..} -> i { arglist = substitute chg arglist }
        i@I_llvm_va_end{..} -> i { arglist = substitute chg arglist }
        i@I_llvm_va_copy{..} -> i { destarglist = substitute chg destarglist
                                  , srcarglist = substitute chg srcarglist
                                  }
        i@I_llvm_gcroot{..} -> i { ptrloc = substitute chg ptrloc
                                 , metadata = substitute chg metadata
                                 }
        i@I_llvm_gcread{..} -> undefined
        i@I_llvm_gcwrite{..} -> undefined
        i@I_llvm_returnaddress{..} -> i { level = substitute chg level }
        i@I_llvm_frameaddress{..} -> i { level = substitute chg level }
        I_llvm_frameescape vs -> I_llvm_frameescape (substitute chg vs)
        i@I_llvm_framerecover{..} -> undefined
        i@I_llvm_read_register{..} -> i
        i@I_llvm_write_register{..} -> i { value = substitute chg value
                                         }
        i@I_llvm_stacksave{..} -> i { result = cid result }
        i@I_llvm_stackrestore{..} -> i { pointer = (substitute chg) pointer }
        i@I_llvm_prefetch{..} -> undefined
        i@I_llvm_pcmarker{..} -> undefined
        i@I_llvm_readcyclecounter{..} -> i { result = cid result}
        i@I_llvm_clear_cache {..} -> undefined
        i@I_llvm_instprof_increment{..} -> undefined
        i@I_llvm_memcpy{..} -> i { dest = (substitute chg) dest
                                 , src = (substitute chg) src
                                 , len = (substitute chg) len
                                 , align = (substitute chg) align
                                 , isvolatile = (substitute chg) isvolatile
                                 }
        i@I_llvm_memmove{..} -> i { dest = (substitute chg) dest
                                  , src = (substitute chg) src
                                  , len = (substitute chg) len
                                  , align = (substitute chg) align
                                  , isvolatile = (substitute chg) isvolatile
                                  }
        i@I_llvm_memset{..} -> i { dest = (substitute chg) dest
                                 , setValue = substitute chg setValue
                                 , len = (substitute chg) len
                                 , align = (substitute chg) align
                                 , isvolatile = (substitute chg) isvolatile
                                 }
                           {-
  i@I_llvm_math_f32 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  i@I_llvm_math_f64 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  i@I_llvm_math_f80 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  i@I_llvm_math_f128 :: MathUnaryOp -> Value -> LocalId -> Cinst;
  i@I_llvm_math_ppcf128 :: MathUnaryOp -> Value -> LocalId -> Cinst;

  i@I_llvm_powi_f32 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_powi_f64 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_powi_f80 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_powi_f128 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_powi_ppcf128 :: Value -> Value -> LocalId -> Cinst;

  i@I_llvm_pow_f32 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_pow_f64 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_pow_f80 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_pow_f128 :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_pow_ppcf128 :: Value -> Value -> LocalId -> Cinst;

  i@I_llvm_bitset_test :: Value -> Value -> LocalId -> Cinst;
  i@I_llvm_donothing :: Cinst;
  -}


instance Substitutable Minst where
  substitute chg (Minst cs g mps mlid) =
    Minst cs (substitute chg g) (substitute chg mps) (substitute chg mlid)

instance Substitutable MetaParam where
  substitute chg mp = case mp of
    MetaParamMeta x -> MetaParamMeta $ substitute chg x
    MetaParamData dt pa1 ma v pa2 -> MetaParamData dt pa1 ma (substitute chg v) pa2

instance Substitutable Tinst where
  substitute chg x = case x of
    T_unreachable -> x
    T_ret_void -> x
    T_return tvs ->  T_return (substitute chg tvs)
    T_br _ -> x
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

instance Substitutable CallFunInterface where
  substitute chg x@CallFunInterface{..} = x { cfi_actualParams = substitute chg cfi_actualParams}

instance Substitutable InvokeFunInterface where
  substitute chg x@InvokeFunInterface{..} = x { ifi_actualParams = substitute chg ifi_actualParams}

instance Substitutable CallAsmInterface where
  substitute chg x@CallAsmInterface{..} = x { cai_actualParams = substitute chg cai_actualParams}

instance Substitutable ActualParam where
  substitute chg c = case c of
    ActualParamData dt pa ma v -> ActualParamData dt pa ma (substitute chg v)
    ActualParamByVal dt pa ma v -> ActualParamByVal dt pa ma (substitute chg v)    
    ActualParamLabel t pa ma v -> ActualParamLabel t pa ma (substitute chg v)

instance Substitutable FormalParamTypeList where
  substitute chg (FormalParamTypeList fps mvp fa) = FormalParamTypeList (substitute chg fps) mvp fa

instance Substitutable FormalParamType where
  substitute chg fp = case fp of
    FormalParamDataType dt pa1 ma -> FormalParamDataType dt pa1 ma 
    FormalParamByValType dt pa1 ma -> FormalParamByValType dt pa1 ma
    FormalParamMetaType mk x -> FormalParamMetaType mk (substitute chg x)


instance Substitutable FunParamList where
  substitute chg (FunParamList fps mvp fa) = FunParamList (substitute chg fps) mvp fa

instance Substitutable FunParam where
  substitute chg fp = case fp of
    FunParamData dt pa1 ma x -> FunParamData dt pa1 ma (substitute chg x)
    FunParamByVal dt pa1 ma x -> FunParamByVal dt pa1 ma (substitute chg x)  
    FunParamMeta mk x -> FunParamMeta mk (substitute chg x)

instance Substitutable Fparam where
  substitute chg fp = case fp of
    FimplicitParam -> fp
    FexplicitParam x -> FexplicitParam (substitute chg x)

instance Substitutable Dbg where
  substitute chg (Dbg mv mc) = Dbg (substitute chg mv) (substitute chg mc)


instance Substitutable TlGlobal where
  substitute chg tg = case tg of
    tl@TlGlobalDtype{..} -> tl { tlg_lhs = substitute chg tlg_lhs
                               , tlg_const = substitute chg tlg_const
                               , tlg_comdat = substitute chg tlg_comdat
                               }
    tl@TlGlobalOpaque{..} -> tl { tlg_lhs = substitute chg tlg_lhs
                                , tlg_const = substitute chg tlg_const
                                , tlg_comdat = substitute chg tlg_comdat
                                }

instance Substitutable TlIntrinsic where
  substitute chg tli = case tli of
    x@TlIntrinsic_llvm_used{..}  -> x { tli_const = substitute chg tli_const }
    x@TlIntrinsic_llvm_compiler_used{..}  -> x { tli_const = substitute chg tli_const }
    x@TlIntrinsic_llvm_global_ctors{..}  -> x { tli_const = substitute chg tli_const }
    x@TlIntrinsic_llvm_global_dtors{..}  -> x { tli_const = substitute chg tli_const }

instance Substitutable a => Substitutable (Toplevel a) where
  substitute chg@Changer{..} tpl = case tpl of
    ToplevelTriple _ -> tpl
    ToplevelDataLayout _ -> tpl
    ToplevelAlias x -> ToplevelAlias (substitute chg x)
    ToplevelDbgInit x -> ToplevelDbgInit (substitute chg x)
    ToplevelStandaloneMd x -> ToplevelStandaloneMd (substitute chg x)
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


instance Substitutable TlTypeDef where
  substitute chg = id

instance Substitutable TlComdat where
  substitute chg (TlComdat lhs v) = TlComdat (substitute chg lhs) v

instance Substitutable DollarId where
  substitute chg di = globalId2DollarId $ substitute chg $ dollarIdToGlobalId di

dollarIdToGlobalId :: DollarId -> GlobalId
dollarIdToGlobalId v = case v of
  DollarIdNum x -> GlobalIdNum x
  DollarIdAlphaNum x -> GlobalIdAlphaNum x
  DollarIdDqString x -> GlobalIdDqString x

globalId2DollarId :: GlobalId -> DollarId
globalId2DollarId v = case v of
  GlobalIdNum x -> DollarIdNum x
  GlobalIdAlphaNum x -> DollarIdAlphaNum x
  GlobalIdDqString x -> DollarIdDqString x

instance Substitutable Comdat where
  substitute chg (Comdat x) = Comdat (substitute chg x)

instance Substitutable TlAttribute where
  substitute chg = id

instance Substitutable TlModuleAsm where
  substitute chg = id

instance Substitutable TlUnamedType where
  substitute chg = id

instance Substitutable TlDepLibs where
  substitute chg = id

instance Substitutable TlDbgInit where
  substitute chg = id

instance Substitutable GlobalId where
  substitute chg = change_GlobalId chg

instance Substitutable TlAlias where
  substitute chg tla@TlAlias{..} = tla { tla_lhs = substitute chg tla_lhs
                                       , tla_aliasee = substitute chg tla_aliasee
                                       }

instance Substitutable v => Substitutable (Conversion s v) where
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

instance Substitutable Aliasee where
  substitute chg al = case al of
    AliaseeTv x -> AliaseeTv (substitute chg x)
    AliaseeConversion x -> AliaseeConversion (substitute chg x)
    AliaseeConversionV x -> AliaseeConversionV (substitute chg x)
    AliaseeGEP x -> AliaseeGEP (substitute chg x)
    AliaseeGEPV x -> AliaseeGEPV (substitute chg x)

instance Substitutable TlNamedMd where
  substitute chg (TlNamedMd mv mns) =
    TlNamedMd (substitute chg mv) (substitute chg mns)

instance Substitutable MdVar where
  substitute _ = id

instance Substitutable MdNode where
  substitute _ = id

instance Substitutable TlStandaloneMd where
  substitute chg (TlStandaloneMd s mk) = TlStandaloneMd s (substitute chg mk)

instance Substitutable MetaKindedConst where
  substitute chg mk = case mk of
    MetaKindedConst m mc -> MetaKindedConst m (substitute chg mc)
    UnmetaKindedNull -> mk

instance Substitutable FunPtr where
  substitute chg@Changer{..} fp = case fp of
    FunId g -> FunId (substitute chg g)
    FunSsa l -> FunSsa (substitute chg l)
    FunIdBitcast (T st c) dt -> FunIdBitcast (T st (substitute chg c)) dt
    FunIdInttoptr (T st c) dt -> FunIdInttoptr (T st (substitute chg c)) dt
    Fun_null -> fp
    Fun_undef -> fp


instance Substitutable MetaConst where
  substitute chg mc = case mc of
    McStruct l -> McStruct (substitute chg l)
    McString s -> mc
    McMn n -> McMn (substitute chg n)
    McMv s -> McMv (substitute chg s)
    McRef s -> McRef (substitute chg s)
    McSimple c -> McSimple (substitute chg c)

instance Substitutable a => Substitutable (Module a) where
  substitute chg (Module l) = Module $ substitute chg l


instance Substitutable NOOP where
  substitute _  = id

instance Ord k => Substitutable (M.Map (GlobalId, k) v) where
  substitute chg m = M.mapKeys (\(gid, k) -> (substitute chg gid, k)) m


instance Substitutable Prefix where
  substitute chg (Prefix x) = Prefix $ substitute chg x

instance Substitutable Prologue where
  substitute chg (Prologue x) = Prologue $ substitute chg x
