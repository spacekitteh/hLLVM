module Llvm.AsmParser.Const where
import Llvm.VmCore.Ast
import Llvm.AsmParser.Basic
import Llvm.AsmParser.Type


pConst :: P Const
pConst = choice [ liftM Ccp pSimpleConstant
                , liftM Cca pComplexConstant
                , pBlockAddr
                , liftM Cb pConstBinaryOperation
                , liftM CiC pConstIcmp
                , liftM CfC pConstFcmp
                , liftM CeE pConstExtractElement
                , liftM CiE pConstInsertElement
                , liftM CsV pConstShuffleVector
                , liftM CeV pConstExtractValue
                , liftM CiV pConstInsertValue
                , liftM Cs pConstSelect
                , liftM CgEp pConstGetElemPtr
                , liftM Cconv pConstConversion
                , liftM CmC pMetaConst
                ]
         
pTypedConst :: P TypedConst
pTypedConst = do { t <- pType
                 ; case t of 
                   Tprimitive TpNull -> return TypedConstNull
                   _ -> liftM (TypedConst t) pConst 
                 }

pTypedConst1 :: P TypedConst
pTypedConst1 = do { t <- pType
                  ; case t of 
                    Tprimitive TpNull -> return TypedConstNull
                    _ -> (choice [pConst, liftM CmL pLocalId] >>= return . (TypedConst t))
                  }


pSimpleConstant :: P SimpleConstant
pSimpleConstant = choice [ pIntOrFloat 
                         , (reserved "undef" >> return CpUndef) 
                         , (reserved "null" >> return CpNull) 
                         , (reserved "false" >> return CpFalse) 
                         , (reserved "true" >> return CpTrue) 
                         , (reserved "zeroinitializer" >> return CpZero) 
                         , liftM CpGlobalAddr pGlobalId
                         , (char 'c' >> liftM CpStr pQuoteStr)
                         ]


signedIntStr :: P String
signedIntStr = do { sign <- option "" (string "-" <|> string "+")
                  ; n <- numericLit
                  ; return (sign ++ n)
                  }

pIntOrFloat :: P SimpleConstant
pIntOrFloat = lexeme (choice [try k, try u, try s, intOrFloat])
 where
   k = do { hd <- string "0x"
          ; t <- option "" (choice [ string "K", string "M", string "H", string "L"])
          ; cs <- many1 hexDigit
          ; return $ CpFloat (hd ++ t ++ cs)
          }
   u = do { ignore (string "u0x")
          ; cs <- many1 hexDigit
          ; return $ CpUhexInt cs
          }
   s = do { ignore (string "s0x")
          ; cs <- many1 hexDigit
          ; return $ CpShexInt cs
          }
   intOrFloat = do { i <- signedIntStr
                   ; option (CpInt i) 
                                (do { ignore (char '.')
                                    ; ne <- option "" (do { n <- numericLit
                                                         ; e <- option "" (do { e <- (string "E" <|> string "e")
                                                                             ; x <- signedIntStr
                                                                             ; return (e ++ x)
                                                                             })
                                                         ; return (n ++ e)
                                                         })
                                    ; return $ CpFloat (i ++ "." ++ ne)
                                    })
                   }


pComplexConstant :: P ComplexConstant
pComplexConstant = choice [ pConstStruct
                          , (try pConstVector)
                          , pPackedStructConst
                          , pConstArray
                          ]

pConstStruct :: P ComplexConstant
pConstStruct = liftM (Cstruct False) (braces (sepBy pTypedConst comma))

pMetaStruct :: P Const
pMetaStruct = liftM (Cca . (Cstruct False)) (braces (sepBy pTypedConst1 comma))



pPackedStructConst :: P ComplexConstant
pPackedStructConst = liftM (Cstruct True) (anglebraces (sepBy pTypedConst comma))
                        
pConstVector :: P ComplexConstant
pConstVector = liftM Cvector (angles (sepBy pTypedConst comma))
                  
pConstArray :: P ComplexConstant
pConstArray = liftM Carray (brackets (sepBy pTypedConst comma))


pBlockAddr :: P Const
pBlockAddr = reserved "blockaddress" >> parens (pTuple2 pGlobalId pPercentLabel) >>= return . (uncurry CblockAddress)
                
pConstIbinaryOperation :: P (IbinExpr Const)
pConstIbinaryOperation = do { op <- pIbinaryOperator
                            ; l <- many pCarry
                            ; (tc1, tc2) <- parens (pTuple pTypedConst)
                            ; return $ IbinExpr op l (getType tc1 tc2) (getConst tc1) (getConst tc2)
                            }
                  

pConstFbinaryOperation :: P (FbinExpr Const)
pConstFbinaryOperation = do { op <- pFbinaryOperator
                            ; l <- pFastMathFlags
                            ; (tc1, tc2) <- parens (pTuple pTypedConst)
                            ; return $ FbinExpr op l (getType tc1 tc2) (getConst tc1) (getConst tc2)
                            }
                         
pConstBinaryOperation :: P (BinExpr Const)                         
pConstBinaryOperation = choice [liftM Ie pConstIbinaryOperation, liftM Fe pConstFbinaryOperation]

              
getType :: TypedConst -> TypedConst -> Type
getType (TypedConst t1 _) (TypedConst t2 _) = if t1 == t2 then t1 
                                              else error "t1 != t2"
getType (TypedConstNull) (TypedConst t2 _) = t2
getType (TypedConst t1 _) TypedConstNull = t1
getType TypedConstNull TypedConstNull = error "unexpected case"

getConst :: TypedConst -> Const
getConst TypedConstNull = Ccp CpNull
getConst (TypedConst _ c) = c
              
pConstIcmp :: P (Icmp Const)
pConstIcmp = do { reserved "icmp"
                ; op <- pIcmpOp
                ; (tc1, tc2) <- parens (pTuple pTypedConst)
                ; return $ Icmp op (getType tc1 tc2) (getConst tc1) (getConst tc2)
                }
                
pConstFcmp :: P (Fcmp Const)
pConstFcmp = do { reserved "fcmp"
                ; op <- pFcmpOp
                ; (tc1, tc2) <- parens (pTuple pTypedConst)
                ; return $ Fcmp op (getType tc1 tc2) (getConst tc1) (getConst tc2)
                }
                
                

pConstExtractElement :: P (ExtractElem TypedConst)
pConstExtractElement = reserved "extractelement" >> parens (pTuple pTypedConst) >>= return . (uncurry ExtractElem)
                  
pConstInsertElement :: P (InsertElem TypedConst)
pConstInsertElement = do { reserved "insertelement"
                         ; (tc1, tc2, idx) <- parens (pTriple3 pTypedConst pTypedConst pConst)
                         ; return $ InsertElem tc1 tc2 (TypedConst (Tprimitive (TpI 32)) idx)
                         }

pConstShuffleVector :: P (ShuffleVector TypedConst)
pConstShuffleVector = reserved "shufflevector" >> parens (pTriple pTypedConst) >>= return . (uncurry3 ShuffleVector)
                     
pConstExtractValue :: P (ExtractValue TypedConst)
pConstExtractValue = reserved "extractvalue" >> parens (pTuple2 pTypedConst (sepBy intStrToken comma)) >>= return . (uncurry ExtractValue)
                        
pConstInsertValue :: P (InsertValue TypedConst)
pConstInsertValue = reserved "insertvalue" >> parens (pTriple3 pTypedConst pTypedConst (sepBy intStrToken comma)) >>= return . (uncurry3 InsertValue)
                        
                        
pConstSelect :: P (Select TypedConst)
pConstSelect = reserved "select" >> parens (pTriple pTypedConst) >>= return . (uncurry3 Select)
                  
pConstConversion :: P (Conversion TypedConst)
pConstConversion = do { op <- pConvertOp
                   ; ignore (chartok '(')
                   ; tc <- pTypedConst
                   ; reserved "to"
                   ; t <- pType
                   ; ignore (chartok ')')
                   ; return $ Conversion op tc t
                   }
                   
                   
pConstGetElemPtr :: P (GetElemPtr TypedConst)
pConstGetElemPtr = do { reserved "getelementptr"
                      ; ib <- option (IsNot InBounds) (reserved "inbounds" >> return (Is InBounds))
                      ; ignore (chartok '(')
                      ; tc1 <- pTypedConst
                      ; idx <- option [] (do { ignore comma
                                            ; idx <- sepBy pTypedConst comma 
                                            ; return idx
                                            })
                      ; ignore (chartok ')')
                      ; return $ GetElemPtr ib tc1 idx
                      }
                      
                      
pMetaConst :: P MetaConst
pMetaConst = char '!' >> choice [ liftM MdConst pMetaStruct
                                , liftM (MdString . DqString) pQuoteStr
                                , liftM (McMn . MdNode) intStrToken
                                , liftM MdRef pLocalId ]
                    
