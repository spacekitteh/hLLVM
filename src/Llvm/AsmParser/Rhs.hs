{-# OPTIONS_GHC -Wall #-}
module Llvm.AsmParser.Rhs where
import Llvm.VmCore.Ast
import Llvm.AsmParser.Basic
import Llvm.AsmParser.Type
import Llvm.AsmParser.Const


pTypedValue :: P TypedValue
pTypedValue = liftM2 TypedValue pType pValue

pValue :: P Value
pValue = choice [ (liftM VgOl pGlobalOrLocalId)
                , (liftM Vc pConst)
                , (liftM Ve pExpr) 
                ]

pTypedPointer :: P TypedPointer
pTypedPointer = liftM2 TypedPointer pType pPointer

pPointer :: P Pointer
pPointer = choice [liftM Pointer pValue]
           

pAllocate :: P MemOp
pAllocate = do { ma <- choice [ reserved "alloca" >> return OnStack
                             , reserved "malloc" >> return InHeap ]
               ; t <- pType 
               ; n <- opt (comma >> pTypedValue)
               ; a <- opt (comma >> pAlign)
               ; return $ Allocate ma t n a
               }
            
pFree :: P MemOp
pFree = liftM Free (reserved "free" >> pTypedValue)


pLoad :: P MemOp
pLoad = do { reserved "load"
           ; at <- option False (reserved "atomic" >> return True)
           ; b <- option False (reserved "volatile" >> return True)
           ; tv <- pTypedPointer
           ; st <- option False (reserved "singlethread" >> return True)
           ; ord <- opt pFenceOrder
           ; a <- optCommaSep pAlign
           ; let atom = if at then Atomic b st ord
                        else NonAtomic b
           ; return $ Load atom tv a
           }
        
pStore :: P MemOp
pStore = do { reserved "store"
            ; at <- option False (reserved "atomic" >> return True)
            ; b <- option False (reserved "volatile" >> return True)
            ; tv <- pTypedValue
            ; ignore comma
            ; ptr <- pTypedPointer
            ; st <- option False (reserved "singlethread" >> return True)
            ; ord <- opt pFenceOrder
            ; a <- optCommaSep pAlign
            ; let atom = if at then Atomic b st ord
                         else NonAtomic b
            ; return $ Store atom tv ptr a
            }
         
         
pFence :: P MemOp
pFence = do { reserved "fence"
            ; st <- option False (reserved "singlethread" >> return True)
            ; ordering <- pFenceOrder
            ; return $ Fence st ordering
            }
   
pCmpXchg :: P MemOp
pCmpXchg = do { reserved "cmpxchg"
              ; v <- option False (reserved "volatile" >> return True)
              ; p <- pTypedPointer
              ; ignore comma
              ; (c, n) <- pTuple pTypedValue
              ; st <- option False (reserved "singlethread" >> return True)
              ; ord <- opt pFenceOrder
              ; return $ CmpXchg v p c n st ord
              }
           
pAtomicRmw :: P MemOp
pAtomicRmw = do { reserved "atomicrmw"
                ; v <- option False (reserved "volatile" >> return True)
                ; op <- pAtomicOp
                ; p <- pTypedPointer
                ; ignore comma
                ; vl <- pTypedValue
                ; st <- option False (reserved "singlethread" >> return True)
                ; ord <- opt pFenceOrder
                ; return $ AtomicRmw v op p vl st ord
                }    

     
          

pMemOp :: P (Bool, MemOp)
pMemOp = choice [ pAllocate >>= \x -> return (True, x)
                , pFree >>= \x -> return (False, x)
                , pLoad >>= \x -> return (True, x)
                , pStore >>= \x -> return (False, x)
                , pFence >>= \x -> return (False, x)
                , pCmpXchg >>= \x -> return (False, x)
                , pAtomicRmw >>= \x -> return (False, x)
                ]


pExpr :: P Expr 
pExpr = choice [ liftM Eb pBinaryOperation
               , liftM EiC pIcmp
               , liftM EfC pFcmp        
               , liftM Es pSelect
               , liftM Ec pConversion
               , liftM EgEp pGetElemPtr
               ]

pBinaryOperation :: P (BinExpr Value)
pBinaryOperation = do { op <- pBinaryOperator
                      ; l <- many pCarry
                      ; t <- pType
                      ; (v1, v2) <- pTuple pValue
                      ; return $ BinExpr op l t v1 v2
                      }
                      
              
pIcmp :: P (Icmp Value)
pIcmp = do { reserved "icmp"
           ; op <- pIcmpOp
           ; t <- pType
           ; (v1, v2) <- pTuple pValue
           ; return $ Icmp op t v1 v2
           }

pFcmp :: P (Fcmp Value)
pFcmp = do { reserved "fcmp"
           ; op <- pFcmpOp
           ; t <- pType
           ; (v1, v2) <- pTuple pValue
           ; return $ Fcmp op t v1 v2
           }
                
                

                        
pSelect :: P (Select TypedValue)
pSelect = do { reserved "select"
             ; (tv1, tv2, tv3) <- pTriple pTypedValue
             ; return $ Select tv1 tv2 tv3
             }


                  
pConversion :: P (Conversion TypedValue)
pConversion = do { op <- pConvertOp
              ; tv <- pTypedValue
              ; reserved "to"
              ; t <- pType
              ; return $ Conversion op tv t
              }
                   
                   
pGetElemPtr :: P (GetElemPtr TypedValue)
pGetElemPtr = do { reserved "getelementptr"
                 ; ib <- option False (reserved "inbounds" >> return True)
                 ; tc1 <- pTypedValue
                 ; idx <- many (try (comma >> pTypedValue))
                 ; return $ GetElemPtr ib tc1 idx
                 }


                   
                   
pExtractElement :: P (ExtractElem TypedValue)
pExtractElement = reserved "extractelement" >> pTuple pTypedValue >>= return . (uncurry ExtractElem)
                  
pInsertElement :: P (InsertElem TypedValue)
pInsertElement = reserved "insertelement" >> pTriple pTypedValue >>= return . (uncurry3 InsertElem)

pShuffleVector :: P (ShuffleVector TypedValue)
pShuffleVector = reserved "shufflevector" >> pTriple pTypedValue >>= return . (uncurry3 ShuffleVector)
                     
pExtractValue :: P (ExtractValue TypedValue)
pExtractValue = do { reserved "extractvalue"
                   ; tc1 <- pTypedValue
                   ; ls <- many (try (comma >> intStrToken))
                   ; return $ ExtractValue tc1 ls
                   }
                        
                        
pInsertValue :: P (InsertValue TypedValue)
pInsertValue = do { reserved "insertvalue"
                  ; (tv1, tv2) <- pTuple pTypedValue
                  ; ls <- many (try (comma >> intStrToken))
                  ; return $ InsertValue tv1 tv2 ls
                  }
                  
               
pVaArg :: P Rhs
pVaArg = reserved "va_arg" >> pTuple2 pTypedValue pType >>= return . (uncurry VaArg)
         
              
pCallFun :: P (Bool, CallSite)
pCallFun = do { cc <- opt pCallConv
              ; atts0 <- many pParamAttr
              ; t <- pType
              ; i <- choice [ liftM FunNameString (choice [symbol "null", symbol "undef"])
                           , liftM FunNameGlobal pGlobalOrLocalId
                           ]
              ; params <- parens (sepBy pActualParam comma)
              ; atts1 <- many pFunAttr
              ; return (not $ isVoidType t, CallFun cc atts0 t i params atts1)
              }

pCallAsm :: P (Bool, CallSite)
pCallAsm = do { t <- pType
              ; reserved "asm"
              ; se <- option False (reserved "sideeffect" >> return True)
              ; as <- option False (reserved "alignstack" >> return True)
              ; (s1, s2) <- pTuple pQuoteStr
              ; params <- parens (sepBy pActualParam comma)
              ; atts1 <- many pFunAttr
              ; return (not $ isVoidType t, CallAsm t se as (QuoteStr s1) (QuoteStr s2) params atts1)
              }

pCallConversion :: P (Bool, CallSite)
pCallConversion = do { a <- many pParamAttr
                  ; t <- pType
                  ; convert <- pConstConversion
                  ; params <- parens (sepBy pActualParam comma)
                  ; atts1 <- many pFunAttr
                  ; return (not $ isVoidType t, CallConversion a t convert params atts1)
                  }
               
pCallSite :: P (Bool, CallSite)
pCallSite = choice [ try pCallFun
                   , try pCallAsm
                   , pCallConversion
                   ]
         
pCall :: P (Bool, Rhs)     
pCall = do { tl <- option False (reserved "tail" >> return True)
           ; reserved "call"
           ; (b, callSite) <- pCallSite
           ; return (b, Call tl callSite)
           }

pActualParam :: P ActualParam
pActualParam = do { t <- pType
                  ; atts0 <- many pParamAttr
                  ; a <- opt pAlign
                  ; v <- pValue
                  ; atts1 <- many pParamAttr
                  ; return $ ActualParam t atts0 a v atts1
                  }
                  
pRhs :: P (Bool, Rhs)
pRhs = choice [ try (liftM (\x -> (True, Re x)) pExpr)
              , try (liftM (\(b,x) -> (b, RmO x)) pMemOp)
              , try (liftM (\x -> (True, ReE x)) pExtractElement)
              , try (liftM (\x -> (True, RiE x)) pInsertElement)
              , try (liftM (\x -> (True, RsV x)) pShuffleVector)
              , try (liftM (\x -> (True, ReV x)) pExtractValue)
              , try (liftM (\x -> (True, RiV x)) pInsertValue)
              , try (liftM (\x -> (True, x)) pVaArg)
              , try (liftM (\(b, x) -> (b, x)) pCall)
              , try (liftM (\x -> (True, x)) pLandingPad)
              ]
       
       


pPersFn :: P PersFn
pPersFn = choice [ liftM PersFnId pGlobalOrLocalId
                 , do { op <- pConvertOp
                      ; ignore (chartok '(')
                      ; ot <- pType
                      ; ix <- pGlobalOrLocalId
                      ; reserved "to"
                      ; dt <- pType
                      ; ignore (chartok ')')
                      ; return $ PersFnCast (Conversion op (ot,ix) dt)
                      }
                 , reserved "undef" >> return PersFnUndef
                 ]
          
pLandingPad :: P Rhs
pLandingPad = do { reserved "landingpad"
                 ; rt <- pType
                 ; reserved "personality"
                 ; ft <- pType
                 ; ix <- pPersFn
                 ; cl <- option False (reserved "cleanup" >> return True)
                 ; c <- many pClause
                 ; return $ LandingPad rt ft ix cl c
                 }
    where pClause = choice [ reserved "catch" >> liftM Catch pTypedValue
                           , reserved "filter" >> liftM Filter pTypedConst
                           ]