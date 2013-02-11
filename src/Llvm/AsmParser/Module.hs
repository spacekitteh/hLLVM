{-# OPTIONS_GHC -Wall #-}
module Llvm.AsmParser.Module where
import Llvm.VmCore.Ast
import Llvm.AsmParser.Basic
import Llvm.AsmParser.Type
import Llvm.AsmParser.Block
import Llvm.AsmParser.Const
import Llvm.AsmParser.Rhs


pModule :: P Module
pModule = do { l <- many toplevel
             ; eof 
             ; return $ Module l
             }

toplevel :: P Toplevel
toplevel = choice [ try pNamedGlobal 
                  , try pToplevelTypeDef
                  , try pToplevelTarget
                  , try pToplevelDepLibs
                  , try pToplevelDeclare
                  , try pToplevelDefine
                  , pToplevelModuleAsm
                  , pStandaloneMd
                  ]


-- GlobalVar '=' OptionalVisibility ALIAS ...
-- GlobalVar '=' OptionalLinkage OptionalVisibility ... -> global variable
pNamedGlobal :: P Toplevel
pNamedGlobal = do { lhsOpt <- opt (pGlobalId >>= \x->chartok '=' >> return x)
                  ; link <- opt (choice [try pExternalLinkage, pLinkage])
                  ; vis <- opt pVisibility
                  ; hasAlias <- option False (reserved "alias" >> return True)
                  ; case (link, hasAlias) of
                      (Just _, _) -> pGlobal lhsOpt link vis
                      (Nothing, False) -> pGlobal lhsOpt Nothing vis
                      (Nothing, True) -> pAlias lhsOpt vis
                  }

-- ParseAlias:
--   ::= GlobalVar '=' OptionalVisibility 'alias' OptionalLinkage Aliasee
-- Aliasee
--   ::= TypeAndValue
--   ::= 'bitcast' '(' TypeAndValue 'to' Type ')'
--   ::= 'getelementptr' 'inbounds'? '(' ... ')'
--
-- Everything through visibility has already been parsed.
--
pAlias :: Maybe GlobalId -> Maybe Visibility -> P Toplevel
pAlias lhs vis = do { link <- option External pAliasLinkage
                    ; aliasee <- pAliasee
                    ; return $ ToplevelAlias lhs vis (Just link) aliasee
                    }
    where pAliasee = 
            choice [ liftM AtV pTypedValue
                   , liftM Ac pConstConversion
                   , liftM AgEp pConstGetElemPtr
                   ]


-- ParseGlobal
--   ::= GlobalVar '=' OptionalLinkage OptionalVisibility OptionalThreadLocal
--       OptionalAddrSpace GlobalType Type Const
--   ::= OptionalLinkage OptionalVisibility OptionalThreadLocal
--       OptionalAddrSpace GlobalType Type Const
--
-- Everything through visibility has been parsed already.
--
pGlobal :: Maybe GlobalId -> Maybe Linkage -> Maybe Visibility -> P Toplevel
pGlobal lhs link vis = do { local_thread <- option False (reserved "thread_local" >> return True)
                          ; (un,addrOpt) <- permute ((,) <$?> (False, reserved "unnamed_addr" >> return True)
                                                        <|?> (Nothing, liftM Just pAddrSpace))
                          --; addrOpt <- opt pAddrSpace
                          ; globalOpt <- pGlobalType
                          ; t <- pType
                          ; c <- if (hasInit link) then
                                    liftM Just pConst
                                else
                                    return Nothing
                          ; (s, a) <- permute ((,) <$?> (Nothing, try (comma >> liftM Just pSection))
                                                      <|?> (Nothing, try (comma >> liftM Just pAlign))
                                             )
                          ; return $ ToplevelGlobal lhs link vis local_thread un addrOpt 
                                   globalOpt t c s a
                          }
    where hasInit x = case x of 
                           Just(ExternWeak) -> False
                           Just(External) -> False
                           Just(DllImport) -> False
                           Just(_) -> True
                           Nothing -> True

data LocalIdOrQuoteStr = L LocalId | Q QuoteStr 
                       deriving (Eq,Show)

pLhsType :: P LocalIdOrQuoteStr 
pLhsType = do { lhs <- choice [ liftM L pLocalId
                              , liftM (Q . QuoteStr) pQuoteStr
                              ] 
              ; _ <- chartok '='
              ; reserved "type"              
              ; return lhs
              }
           
pToplevelTypeDef :: P Toplevel           
pToplevelTypeDef = do { lhsOpt <- opt pLhsType
                      ; case lhsOpt of
                        Nothing -> liftM (ToplevelUnamedType 1) pType
                        Just (L x) -> liftM (ToplevelTypeDef x) pType
                        Just (Q _) -> error "irrefutable"
                      }

pToplevelTarget :: P Toplevel
pToplevelTarget = do { reserved "target"
                     ; k <- pTargetKind
                     ; _ <- chartok '='
                     ; s <- pQuoteStr
                     ; return $ ToplevelTarget k (QuoteStr s)
                     }

pToplevelDepLibs :: P Toplevel
pToplevelDepLibs = do { reserved "deplibs"
                      ; _ <- chartok '='
                      ; l <- brackets (sepBy pQuoteStr comma)
                      ; return $ ToplevelDepLibs (fmap QuoteStr l)
                      }

          
pFunctionPrototype :: P FunctionPrototype
pFunctionPrototype = do { lopt <- opt allLinkage
                        ; vopt <- opt pVisibility
                        ; copt <- opt pCallConv
                        ; as <- many pParamAttr
                        ; ret <- pType
                        ; name <- pGlobalId
                        ; params <- pFormalParamList
                        ; sopt <- opt pSection
                        ; aopt <- opt pAlign
                        ; attrs <- many pFunAttr
                        ; gopt <- opt (liftM (Gc . QuoteStr) (reserved "gc" >> pQuoteStr))
                        ; return (FunctionPrototype lopt vopt copt 
                                  as ret name params attrs sopt aopt gopt)
                        }
                     

pToplevelDefine :: P Toplevel
pToplevelDefine = do { reserved "define"
                     ; fh <- pFunctionPrototype
                     ; bs <- braces pBlocks
                     ; return $ ToplevelDefine fh bs
                     }
                  
pToplevelDeclare :: P Toplevel                  
pToplevelDeclare = liftM ToplevelDeclare 
                   (reserved "declare" >> pFunctionPrototype)

pToplevelModuleAsm :: P Toplevel
pToplevelModuleAsm = do { reserved "module"
                        ; reserved "asm"
                        ; s <- pQuoteStr
                        ; return $ ToplevelModuleAsm $ QuoteStr s
                        }
                     


                

                   

pMdNode :: P MdNode
pMdNode = (char '!' >> liftM MdNode intStrToken)

pStandaloneMd :: P Toplevel
pStandaloneMd = do { _ <- char '!' 
                   ; choice [ do { n <- intStrToken
                                 ; _ <- chartok '='
                                 ; choice [ do { t <- pTypedValue
                                               ; return (ToplevelStandaloneMd n t)
                                               }
                                          ]
                                 }
                            , do { i <- lexeme pId
                                 ; _ <- chartok '='
                                 ; _ <- lexeme (string "!{")
                                 ; l <- sepBy pMdNode comma 
                                 ; _ <- chartok '}'
                                 ; return $ ToplevelNamedMd (MdVar i) l
                                 }
                            ]
                   }
                   
