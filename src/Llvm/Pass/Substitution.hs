{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards,  TypeSynonymInstances, FlexibleInstances #-}
module Llvm.Pass.Substitution where

import Control.Monad
import Data.Maybe
import Prelude hiding (succ)

import qualified Compiler.Hoopl as H

import Llvm.Data.Ir
import Llvm.Query.TypeConstValue
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
  
instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
  substitute chg (Left a) = Left $ substitute chg a
  substitute chg (Right a) = Right $ substitute chg a
  
instance Substitutable TypedConstOrNull where
  substitute chg@Changer{..} x = case x of
    TypedConst (T dt c) -> TypedConst (T dt (substitute chg c))
    UntypedNull -> x

instance Substitutable Value where
  substitute chg@Changer{..} cst = case cst of
    Val_ssa lid -> Val_ssa $ change_LocalId lid
    Val_const c -> Val_const (substitute chg c)

instance Substitutable Const where
  substitute chg@Changer{..} cst = 
    let change2c cf a b = cf (substitute chg a) (substitute chg b)
        cst1 = case cst of
          C_struct pk l -> C_struct pk (fmap (substitute chg) l)
          C_vector l -> C_vector (fmap (substitute chg) l)
          C_vectorN n e -> C_vectorN n (substitute chg e)
          C_array l -> C_array (fmap (substitute chg) l)
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

          C_getelementptr ib (T t c) l -> cst
          C_getelementptr_V ib (T t c) l -> cst

          C_select_I _ -> cst
          C_select_F _ -> cst
          C_select_P _ -> cst

          C_select_First _ _ _ -> cst

          C_select_VI _ -> cst
          C_select_VF _ -> cst
          C_select_VP _ -> cst

          C_icmp _ -> cst
          C_icmp_V _ -> cst
          C_fcmp _ -> cst
          C_fcmp_V _ -> cst
          C_shufflevector_I _ -> cst
          C_shufflevector_F _ -> cst
          C_shufflevector_P _ -> cst

          C_extractelement_I _ -> cst
          C_extractelement_F _ -> cst
          C_extractelement_P _ -> cst
          C_insertelement_I _ -> cst
          C_insertelement_F _ -> cst
          C_insertelement_P _ -> cst

          C_extractvalue _ -> cst
          C_insertvalue _ -> cst
          _ -> cst
    in change_Const cst1

        
instance Substitutable x => Substitutable (T t x) where
  substitute chg (T t a) = T t (substitute chg a)

instance Substitutable FunctionPrototype where
  substitute chg fp@FunctionPrototype {..} = 
    fp { fp_fun_name = change_GlobalId chg fp_fun_name }

instance Substitutable TlDeclare where
  substitute chg tl@(TlDeclare fp) = TlDeclare (substitute chg fp)

instance Substitutable a => Substitutable (TlDefine a) where
  substitute chg td@(TlDefine fp e g) = TlDefine (substitute chg fp) e (H.mapGraph (substitute chg) g)


{--
instance Substitutable (H.Graph (Node a) H.C H.C) where
  substitute chg graph = H.mapGraph (substitute chg) graph
-}

instance Substitutable a => Substitutable (Node a e x) where
  substitute chg node = case node of
    Lnode _ -> node
    Pnode _ _ -> node
    Cnode ci dbgs -> Cnode (substitute chg ci) (fmap (substitute chg) dbgs)
    Mnode mi dbgs -> Mnode (substitute chg mi) (fmap (substitute chg) dbgs)
    Comment _ -> node
    Enode _ -> node
    Tnode ti dbgs -> Tnode (substitute chg ti) (fmap (substitute chg) dbgs)

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
                        , result = change_LocalId chg result
                        }
        i@I_loadatomic{..} -> i { pointer = substitute chg pointer
                              , result = change_LocalId chg result
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
                             , result = change_LocalId chg result
                             }
        i@I_cmpxchg_F{..} -> i { pointer = substitute chg pointer
                             , cmpf = substitute chg cmpf
                             , newf = substitute chg newf
                             , result = change_LocalId chg result
                             }
        i@I_cmpxchg_P{..} -> i { pointer = substitute chg pointer
                             , cmpp = substitute chg cmpp
                             , newp = substitute chg newp
                             , result = change_LocalId chg result
                             }
        i@I_atomicrmw{..} -> i { pointer = substitute chg pointer
                             , val = substitute chg val
                             , result = change_LocalId chg result
                             }
        i@I_call_fun{..} -> i { call_actualParams = fmap (substitute chg) call_actualParams
                            , call_return = fmap (change_LocalId chg) call_return
                            }
        i@I_call_asm{..} -> i { call_actualParams = fmap (substitute chg) call_actualParams
                            , call_return = fmap (change_LocalId chg) call_return
                            }
        i@I_extractelement_I{..} -> i { vectorI = substitute chg vectorI
                                    , index = substitute chg index
                                    , result = change_LocalId chg result
                                    }
        i@I_extractelement_F{..} -> i { vectorF = substitute chg vectorF
                                    , index = substitute chg index
                                    , result = change_LocalId chg result
                                    }
        i@I_extractelement_P{..} -> i { vectorP = substitute chg vectorP
                                    , index = substitute chg index
                                    , result = change_LocalId chg result
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
                              , clauses = fmap (substitute chg) clauses
                              , result = cid result
                              }
        i@I_getelementptr{..} -> i { pointer = (substitute chg) pointer
                                 , indices = fmap (substitute chg) indices
                                 , result = cid result
                                 }
        i@I_getelementptr_V{..} -> i { vpointer = (substitute chg) vpointer
                                   , vindices = fmap (substitute chg) vindices
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
        I_llvm_frameescape vs -> I_llvm_frameescape (fmap (substitute chg) vs)
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
                                 , operand1 = substitute chg operand1
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
  substitute chg x = x


instance Substitutable Tinst where
  substitute chg x = case x of
    T_unreachable -> x
    T_ret_void -> x
    T_return tvs ->  T_return (fmap (substitute chg) tvs)
    T_br _ -> x
    T_cbr cnd t f -> T_cbr (substitute chg cnd) t f
    T_indirectbr tv ls -> T_indirectbr (substitute chg tv) ls
    T_switch d o -> T_switch (substitute chg (fst d), snd d)
                    (fmap (\x -> (substitute chg (fst x), snd x)) o)
    t@T_invoke{..} -> t { invoke_ptr = substitute chg invoke_ptr
                        , invoke_actualParams = fmap (substitute chg) invoke_actualParams
                        , invoke_return = fmap (change_LocalId chg) invoke_return
                        }
    t@T_invoke_asm{..} -> t { invoke_actualParams = fmap (substitute chg) invoke_actualParams 
                            , invoke_return = fmap (change_LocalId chg) invoke_return
                            }
    T_resume (T dt v) -> T_resume (T dt (substitute chg v))
    T_unwind -> T_unwind
                    
instance Substitutable ActualParam where
  substitute chg c = case c of
    ActualParamData dt pa ma v pa2 -> ActualParamData dt pa ma (substitute chg v) pa2
    ActualParamLabel t pa ma v pa2 -> ActualParamLabel t pa ma (substitute chg v) pa2


instance Substitutable Dbg where
  substitute chg x = x


instance Substitutable TlGlobal where
  substitute chg tg = case tg of
    tl@TlGlobalDtype{..} -> tl { tlg_lhs = fmap (change_GlobalId chg) tlg_lhs
                               , tlg_const = fmap (substitute chg) tlg_const
                               }
    tl@TlGlobalOpaque{..} -> tl { tlg_lhs = fmap (change_GlobalId chg) tlg_lhs
                                , tlg_const = fmap (substitute chg) tlg_const
                                }                          
  
instance Substitutable a => Substitutable (Toplevel a) where
  substitute chg@Changer{..} tpl = case tpl of
    ToplevelNamedMd x -> ToplevelNamedMd (substitute chg x)
    ToplevelStandaloneMd x -> ToplevelStandaloneMd (substitute chg x)
    ToplevelDeclare x -> ToplevelDeclare (substitute chg x)
    ToplevelDefine x -> ToplevelDefine (substitute chg x)
    ToplevelGlobal x -> ToplevelGlobal (substitute chg x)
    _ -> tpl

instance Substitutable TlNamedMd where
  substitute chg@Changer{..} md = md

instance Substitutable TlStandaloneMd where
  substitute chg@Changer{..} (TlStandaloneMd s mk) = TlStandaloneMd s (substitute chg mk)

instance Substitutable MetaKindedConst where
  substitute chg@Changer{..} mk = case mk of
    MetaKindedConst mk mc -> MetaKindedConst mk (substitute chg mc)
    UnmetaKindedNull -> mk
  
instance Substitutable FunPtr where  
  substitute chg@Changer{..} fp = case fp of
    FunId g -> FunId (change_GlobalId g)
    FunIdBitcast (T st c) dt -> FunIdBitcast (T st (substitute chg c)) dt
    FunIdInttoptr (T st c) dt -> FunIdInttoptr (T st (substitute chg c)) dt
    Fun_null -> fp
    Fun_undef -> fp


instance Substitutable MetaConst where  
  substitute chg@Changer{..} mc = case mc of
    McStruct l -> McStruct (fmap (substitute chg) l)
    McString s -> mc
    McMn n -> mc
    McMv s -> mc
    McRef s -> mc
    McSimple c -> McSimple (substitute chg c)

instance Substitutable a => Substitutable (Module a) where
  substitute chg (Module l) = Module $ fmap (substitute chg) l
  
  
instance Substitutable NOOP where  
  substitute chg x = x
  
instance Ord k => Substitutable (M.Map (GlobalId, k) v) where
  substitute chg m = M.mapKeys (\(gid, k) -> (change_GlobalId chg gid, k)) m
  