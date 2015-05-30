{-# LANGUAGE GADTs #-}
module Llvm.Asm.Parser.Const where
import Llvm.Asm.Data
import Llvm.Asm.Parser.Basic
import Llvm.Asm.Parser.Type


pConst :: P Const
pConst = choice [ liftM C_simple pSimpleConstant
                , liftM C_complex pComplexConstant
                , pBlockAddr
                , liftM C_binexp pConstBinaryOperation
                , liftM C_icmp pConstIcmp
                , liftM C_fcmp pConstFcmp
                , liftM C_extractelement pConstExtractElement
                , liftM C_insertelement pConstInsertElement
                , liftM C_shufflevector pConstShuffleVector
                , liftM C_extractvalue pConstExtractValue
                , liftM C_insertvalue pConstInsertValue
                , liftM C_select pConstSelect
                , liftM C_gep pConstGetElemPtr
                , liftM C_conv pConstConversion
                ]
         
pTypedConst :: P (TypedConstOrNull)
pTypedConst = do { t <- pType
                 ; case t of 
                   (Tprimitive TpNull) -> return UntypedNull
                   _ -> liftM (TypedConst . Typed t) pConst 
                 }

pSimpleConstant :: P SimpleConstant
pSimpleConstant = choice [ pIntOrFloat 
                         , (reserved "undef" >> return CpUndef) 
                         , (reserved "null" >> return CpNull) 
                         , (reserved "false" >> return CpFalse) 
                         , (reserved "true" >> return CpTrue) 
                         , (reserved "zeroinitializer" >> return CpZeroInitializer)
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
pConstStruct = liftM (Cstruct Unpacked) (braces (sepBy pTypedConst comma))



pPackedStructConst :: P ComplexConstant
pPackedStructConst = liftM (Cstruct Packed) (anglebraces (sepBy pTypedConst comma))
                        
pConstVector :: P ComplexConstant
pConstVector = liftM Cvector (angles (sepBy pTypedConst comma))
                  
pConstArray :: P ComplexConstant
pConstArray = liftM Carray (brackets (sepBy pTypedConst comma))


pBlockAddr :: P Const
pBlockAddr = reserved "blockaddress" >> parens (pTuple2 pGlobalId pPercentLabel) 
             >>= return . (uncurry C_blockAddress)
                
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

              
getType :: TypedConstOrNull -> TypedConstOrNull -> Type
getType (TypedConst (Typed t1 _)) (TypedConst (Typed t2 _)) = if t1 == t2 then t1 
                                                              else error "t1 != t2"
getType UntypedNull (TypedConst (Typed t2 _)) = t2
getType (TypedConst (Typed t1 _)) UntypedNull = t1
getType UntypedNull UntypedNull = error "unexpected case"

getConst :: TypedConstOrNull -> Const
getConst UntypedNull = C_simple CpNull
getConst (TypedConst (Typed _ c)) = c
              
pConstIcmp :: P (Icmp Const) 
pConstIcmp = do { reserved "icmp"
                ; op <- pIcmpOp
                ; (tc1, tc2) <- parens (pTuple pTypedConst)
                ; return (Icmp op (getType tc1 tc2) (getConst tc1) (getConst tc2))
                }
                
pConstFcmp :: P (Fcmp Const)
pConstFcmp = do { reserved "fcmp"
                ; op <- pFcmpOp
                ; (tc1, tc2) <- parens (pTuple pTypedConst)
                ; return $ Fcmp op (getType tc1 tc2) (getConst tc1) (getConst tc2)
                }
                
                
extractTypedConst :: TypedConstOrNull -> Typed Const            
extractTypedConst (TypedConst x) = x
extractTypedConst _ = error " error rorooror"

pConstExtractElement :: P (ExtractElement Const)
pConstExtractElement = reserved "extractelement" >> parens (pTuple pTypedConst) >>= 
                       \(x,y) -> return $ ExtractElement (extractTypedConst x) (extractTypedConst y)
                  
pConstInsertElement :: P (InsertElement Const)
pConstInsertElement = do { reserved "insertelement"
                         ; (tc1, tc2, idx) <- parens (pTriple3 pTypedConst pTypedConst pConst)
                         ; return $ InsertElement (extractTypedConst tc1) (extractTypedConst tc2) (Typed (Tprimitive $ TpI 32) idx)
                         }

pConstShuffleVector :: P (ShuffleVector Const)
pConstShuffleVector = reserved "shufflevector" >> parens (pTriple pTypedConst) >>= 
                      \(x,y,z) -> return $  ShuffleVector (extractTypedConst x) (extractTypedConst y) (extractTypedConst z)
                     
pConstExtractValue :: P (ExtractValue Const)
pConstExtractValue = reserved "extractvalue" >> parens (pTuple2 pTypedConst (sepBy unsignedInt comma)) >>= 
                     \(x, y) -> return $ ExtractValue (extractTypedConst x) y 
                        
pConstInsertValue :: P (InsertValue Const)
pConstInsertValue = reserved "insertvalue" >> parens (pTriple3 pTypedConst pTypedConst (sepBy unsignedInt comma)) >>= 
                    \(x,y,z) -> return $ InsertValue (extractTypedConst x) (extractTypedConst y) z
                        
                        
pConstSelect :: P (Select Const)
pConstSelect = reserved "select" >> parens (pTriple pTypedConst) >>= 
               \(x,y,z) -> return $ Select (extractTypedConst x) (extractTypedConst y) (extractTypedConst z)
                  
pConstConversion :: P (Conversion Const)
pConstConversion = do { op <- pConvertOp
                      ; ignore $ chartok '('
                      ; tc <- pTypedConst
                      ; reserved "to"
                      ; t <- pType
                      ; ignore $ chartok ')'
                      ; return $ Conversion op (extractTypedConst tc) t
                      }
                   

                   
pConstGetElemPtr :: P (GetElementPtr Const)
pConstGetElemPtr = do { reserved "getelementptr"
                      ; ib <- option (IsNot InBounds) (reserved "inbounds" >> return (Is InBounds))
                      ; ignore (chartok '(')
                      ; (Typed t c) <- liftM extractTypedConst pTypedConst
                      ; idx <- option [] (do { ignore comma
                                            ; idx <- sepBy pTypedConst comma 
                                            ; return $ fmap extractTypedConst idx
                                            })
                      ; ignore (chartok ')')
                      ; return $ GetElementPtr ib (Pointer (Typed t c)) idx
                      }

pMetaKindedConst :: P (MetaKindedConst)
pMetaKindedConst = choice [ reserved "null" >> return UnmetaKindedNull
                          , liftM2 MetaKindedConst pMetaKind pMetaConst
                          ]


pMetaConst :: P MetaConst
pMetaConst = choice[ char '!' >> choice [ liftM McStruct (braces (sepBy pMetaKindedConst comma))
                                        , liftM (McString . DqString) pQuoteStr
                                        , liftM (McMdRef . MdRefNode . MdNode) unsignedInt
                                        ]
                    , liftM McSimple pConst
                    , liftM McSsa pLocalId 
                    ]
                                
                    
