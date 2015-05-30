{-# LANGUAGE GADTs #-}
module Llvm.Asm.Parser.Rhs where
import Llvm.Asm.Data
import Llvm.Asm.Parser.Basic
import Llvm.Asm.Parser.Type
import Llvm.Asm.Parser.Const


pTmetadata :: P MetaConst
pTmetadata = do { reserved "metadata" 
                ; mc <- pMetaConst
                ; return mc
                }

pTypedValue :: P (Typed Value)
pTypedValue = liftM2 Typed pType pValue

pValue :: P Value
pValue = choice [ (liftM Val_const pConst)
                , (liftM Val_local pLocalId) 
                ]


pTypedPointer :: P (Pointer (Typed Value))
pTypedPointer = do { t <- pType
                   ; v <- pValue
                   ; return (Pointer (Typed t v))
                   }
                
pAllocate :: P MemOp
pAllocate = do { reserved "alloca"
               ; ina <- option (IsNot InAllocaAttr) (reserved "inalloca" >> return (Is InAllocaAttr))
               ; t <- pType 
               ; n <- opt (comma >> pTypedValue)
               ; a <- opt (comma >> pAlign)
               ; return $ Alloca ina t n a
               }

pLoad :: P MemOp
pLoad = do { reserved "load"
           ; at <- option False (reserved "atomic" >> return True)
           ; b <- option (IsNot Volatile) (reserved "volatile" >> return (Is Volatile))
           ; tv <- pTypedPointer
           ; if at then
               do { st <- option (IsNot SingleThread) (reserved "singlethread" >> return (Is SingleThread))
                  ; ord <- pFenceOrder
                  ; a <- optCommaSep pAlign
                  ; return $ LoadAtomic (Atomicity st ord) b tv a
                  }
             else
               do { a <- optCommaSep pAlign
                  ; nt <- optCommaSep pNontemporal
                  ; inv <- optCommaSep pInvariantLoad
                  ; nn <- optCommaSep pNonnull
                  ; return $ Load b tv a nt inv nn
                  }
           }
        
pStore :: P MemOp
pStore = do { reserved "store"
            ; at <- option False (reserved "atomic" >> return True)
            ; b <- option (IsNot Volatile) (reserved "volatile" >> return (Is Volatile))
            ; tv <- pTypedValue
            ; ignore comma
            ; ptr <- pTypedPointer
            ; if at then 
                do { st <- option (IsNot SingleThread) (reserved "singlethread" >> return (Is SingleThread))
                   ; ord <- pFenceOrder
                   ; a <- optCommaSep pAlign
                   ; return $ StoreAtomic (Atomicity st ord) b tv ptr a 
                   }
              else
                do { a <- optCommaSep pAlign
                   ; nt <- optCommaSep pNontemporal
                   ; return $ Store b tv ptr a nt
                   }
            }
         
         
pFence :: P MemOp
pFence = do { reserved "fence"
            ; st <- option (IsNot SingleThread) (reserved "singlethread" >> return (Is SingleThread))
            ; ordering <- pFenceOrder
            ; return $ Fence st ordering
            }
   
pCmpXchg :: P MemOp
pCmpXchg = do { reserved "cmpxchg"
              ; wk <- option (IsNot Weak) (reserved "weak" >> return (Is Weak))
              ; v <- option (IsNot Volatile) (reserved "volatile" >> return (Is Volatile))
              ; p <- pTypedPointer
              ; ignore comma
              ; (c, n) <- pTuple pTypedValue
              ; st <- option (IsNot SingleThread) (reserved "singlethread" >> return (Is SingleThread))
              ; sord <- pFenceOrder
              ; ford <- pFenceOrder
              ; return $ CmpXchg wk v p c n st sord ford
              }
           
pAtomicRmw :: P MemOp
pAtomicRmw = do { reserved "atomicrmw"
                ; v <- option (IsNot Volatile) (reserved "volatile" >> return (Is Volatile))
                ; op <- pAtomicOp
                ; p <- pTypedPointer
                ; ignore comma
                ; vl <- pTypedValue
                ; st <- option (IsNot SingleThread) (reserved "singlethread" >> return (Is SingleThread))
                ; ord <- pFenceOrder
                ; return $ AtomicRmw v op p vl st ord
                }    

     
          

pMemOp :: P MemOp
pMemOp = choice [ pAllocate
                , pLoad
                , pStore
                , pFence
                , pCmpXchg
                , pAtomicRmw
                ]


pExpr :: P Expr 
pExpr = choice [ liftM ExprBinExpr pBinaryOperation
               , liftM ExprIcmp pIcmp
               , liftM ExprFcmp pFcmp
               , liftM ExprSelect pSelect
               , liftM ExprConversion pConversion
               , liftM ExprGetElementPtr pGetElemPtr
               ]

pIbinaryOperation :: P (IbinExpr Value)
pIbinaryOperation = do { op <- pIbinaryOperator
                       ; l <- many pCarry
                       ; t <- pType
                       ; (v1, v2) <- pTuple pValue
                       ; return $ IbinExpr op l t v1 v2
                       }

pFbinaryOperation :: P (FbinExpr Value)
pFbinaryOperation = do { op <- pFbinaryOperator
                       ; l <- pFastMathFlags 
                       ; t <- pType
                       ; (v1, v2) <- pTuple pValue
                       ; return $ FbinExpr op l t v1 v2
                       }
                    
pBinaryOperation :: P (BinExpr Value)                    
pBinaryOperation = choice [ liftM Ie pIbinaryOperation, liftM Fe pFbinaryOperation]

              
pIcmp :: P (Icmp Value)
pIcmp = do { reserved "icmp"
           ; op <- pIcmpOp
           ; t <- pType
           ; (v1, v2) <- pTuple pValue
           ; return (Icmp op t v1 v2)
           }

pFcmp :: P (Fcmp Value)
pFcmp = do { reserved "fcmp"
           ; op <- pFcmpOp
           ; t <- pType
           ; (v1, v2) <- pTuple pValue
           ; return $ Fcmp op t v1 v2
           }
                
                

                        
pSelect :: P (Select Value)
pSelect = do { reserved "select"
             ; t <- pSelTy
             ; v <- pValue
             ; ignore comma
             ; (tv2, tv3) <- pTuple pTypedValue
             ; return $ Select (Typed t v) tv2 tv3
             }

pConversion :: P (Conversion Value)
pConversion = do { op <- pConvertOp
              ; tv <- pTypedValue
              ; reserved "to"
              ; t <- pType
              ; return $ Conversion op tv t
              }

pGetElemPtr :: P (GetElementPtr Value)
pGetElemPtr = do { reserved "getelementptr"
                 ; ib <- option (IsNot InBounds) (reserved "inbounds" >> return (Is InBounds))
                 ; tc1 <- pTypedPointer
                 ; idx <- many (try (comma >> pTypedValue))
                 ; return $ GetElementPtr ib tc1 idx
                 }

pExtractElement :: P (ExtractElement Value)
pExtractElement = reserved "extractelement" >> pTuple pTypedValue >>= return . (uncurry ExtractElement)

pInsertElement :: P (InsertElement Value)
pInsertElement = reserved "insertelement" >> pTriple pTypedValue >>= return . (uncurry3 InsertElement)

pShuffleVector :: P (ShuffleVector Value)
pShuffleVector = reserved "shufflevector" >> pTriple pTypedValue >>= return . (uncurry3 ShuffleVector)
                     
pExtractValue :: P (ExtractValue Value)
pExtractValue = do { reserved "extractvalue"
                   ; tc1 <- pTypedValue
                   ; ls <- many (try (comma >> unsignedInt))
                   ; return $ ExtractValue tc1 ls
                   }

pInsertValue :: P (InsertValue Value)
pInsertValue = do { reserved "insertvalue"
                  ; (tv1, tv2) <- pTuple pTypedValue
                  ; ls <- many (try (comma >> unsignedInt))
                  ; return $ InsertValue tv1 tv2 ls
                  }
                  
               
pVaArg :: P Rhs
pVaArg = reserved "va_arg" >> pTuple2 pTypedValue pType >>= return . (RhsVaArg . uncurry VaArg)
         
pFunName :: P FunName
pFunName = choice [ symbol "null" >> return FunName_null
                  , symbol "0" >> return FunName_null
                  , symbol "undef" >> return FunName_undef
                  , liftM FunNameGlobal pGlobalOrLocalId
                  , do { reserved "bitcast"
                       ; ignore $ chartok '('
                       ; tc <- pTypedConst
                       ; reserved "to"
                       ; t <- pType
                       ; ignore $ chartok ')'
                       ; return $ FunNameBitcast (extractTypedConst tc) t
                       }
                  , do { reserved "inttoptr"
                       ; ignore $ chartok '('
                       ; tc <- pTypedConst
                       ; reserved "to"
                       ; t <- pType
                       ; ignore $ chartok ')'
                       ; return $ FunNameInttoptr (extractTypedConst tc) t
                       }
                  ]
           
              
pCallFun :: P CallSite
pCallFun = do { cc <- opt pCallConv
              ; atts0 <- many pParamAttr -- CallRetAttr
              ; t <- pType
              ; i <- pFunName
              ; params <- parens (sepBy pActualParam comma)
              ; atts1 <- pFunAttrCollection
              ; return (CallSiteFun cc atts0 t i params atts1)
              }

pAsm ::  P InlineAsmExp
pAsm = do { t <- pType
          ; reserved "asm"
          ; se <- opt (reserved "sideeffect" >> return SideEffect)
          ; as <- opt (reserved "alignstack" >> return AlignStack)
          ; dialect <- option AsmDialectAtt (reserved "inteldialect" >> return AsmDialectIntel)
          ; (s1, s2) <- pTuple pQuoteStr
          ; params <- parens (sepBy pActualParam comma)
          ; atts1 <- pFunAttrCollection
          ; return (InlineAsmExp t se as dialect (DqString s1) (DqString s2) params atts1)
          }



pCallSite :: P CallSite
pCallSite = choice [try pCallFun]
         
pCall :: P Rhs
pCall = do { tl <- option TcNon pTailCall 
           ; reserved "call"
           ; (liftM (RhsCall tl) pCallSite)
           }
        
pRhsInlineAsm :: P Rhs        
pRhsInlineAsm = do { tl <- option TcNon pTailCall
                   ; reserved "call"
                   ; liftM RhsInlineAsm pAsm
                   }

pActualParam :: P ActualParam
pActualParam = choice [do { t <- pType
                          ; atts0 <- many pParamAttr
                          ; case t of
                            Tprimitive TpLabel -> 
                              do { v <- pPercentLabel
                                 ; atts1 <- many pParamAttr
                                 ; return $ ActualParamLabel t (atts0++atts1) v
                                 }
                            _  -> 
                              do { v <- pValue
                                 ; atts1 <- many pParamAttr
                                 ; return $ ActualParamData t (atts0++atts1) v
                                 }
                          }
                      ,liftM ActualParamMeta pMetaKindedConst
                      ]
                  
pRhs :: P Rhs
pRhs = choice [ liftM RhsExpr pExpr
              , liftM RhsMemOp pMemOp
              , liftM RhsExtractElement pExtractElement
              , liftM RhsInsertElement pInsertElement
              , liftM RhsShuffleVector pShuffleVector
              , liftM RhsExtractValue pExtractValue
              , liftM RhsInsertValue pInsertValue
              , pVaArg
              , try pCall
              , pRhsInlineAsm
              , pLandingPad
              ]

pLandingPad :: P Rhs
pLandingPad = do { reserved "landingpad"
                 ; rt <- pType
                 ; reserved "personality"
                 ; ft <- pType
                 ; ix <- pFunName
                 ; cl <- option Nothing (reserved "cleanup" >> return (Just Cleanup))
                 ; c <- many pClause
                 ; return $ RhsLandingPad $ LandingPad rt ft ix cl c
                 }
    where pClause = choice [ reserved "catch" >> liftM ClauseCatch pTypedValue
                           , reserved "filter" >> liftM ClauseFilter pTypedConst
                           ]
