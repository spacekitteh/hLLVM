module Llvm.AsmParser.Type where
import Llvm.VmCore.Ast
import Llvm.AsmParser.Basic


pType :: P Type
pType =
    do { t <- choice [ array
                    , try vector
                    , struct
                    , pstruct
                    , liftM Tprimitive pPrimType
                    , pTypeName
                    , upref
                    , pOther ]
       ; post t
       }
 where
   upref    = chartok '\\' >> liftM TupRef decimal
   array    = brackets (avbody Tarray)
   vector   = angles (avbody Tvector)
   avbody f = do { n <- lexeme decimal
                 ; chartok 'x'
                 ; t <- pType
                 ; return (f n t)
                 }
   struct   = liftM (Tstruct False) (braces (sepBy pType comma))
   pstruct  = angles $ liftM (Tstruct True) (braces (sepBy pType comma))
   pTypeName = do { x <- pLocalId 
                  ; case x of 
                      LocalIdNum n -> return $ Tno n 
                      LocalIdAlphaNum s -> return $ Tname s
                      LocalIdQuoteStr s -> return $ TquoteName s
                  }
   pOther = choice [ reserved "opaque" >> return Topaque
                   , reserved "metadata" >> return Tmetadata
                   ]


pStar :: P (Maybe AddrSpace)
pStar = opt pAddrSpace >>= \x -> chartok '*' >> return x

post :: Type -> P Type
post t = do { pt <- option t (pStar >>= \x -> post (Tpointer t x))
            ; option pt $ do { fp <- pTypeParamList
                             ; fattrs <- many pFunAttr
                             ; post (Tfunction pt fp fattrs)
                             }
            }

pTypeI :: P TypePrimitive
pTypeI = (char 'i' >> liftM TpI decimal)

pTypeF :: P TypePrimitive
pTypeF = (char 'f' >> liftM TpF decimal)

pTypeV :: P TypePrimitive
pTypeV = (char 'v' >> liftM TpV decimal)


pPrimType :: P TypePrimitive
pPrimType = choice [ try pTypeI
                   , reserved "half"      >> return TpHalf
                   , reserved "float"     >> return TpFloat
                   , reserved "fp128"     >> return TpFp128
                   , pTypeF
                   , reserved "label"     >> return TpLabel
                   , reserved "double"    >> return TpDouble
                   , reserved "x86_fp80"  >> return TpX86Fp80
                   , reserved "x86_mmx"   >> return TpX86Mmx
                   , reserved "ppc_fp128" >> return TpPpcFp128
                   , reserved "void"      >> return TpVoid
                   , pTypeV
                   , reserved "null" >> return TpNull
                   ]
            
            
pTypeParamList :: P TypeParamList
pTypeParamList = do { chartok '('
                    ; (l, f) <- args []
--                    ; funAttrs <- many pFunAttr
                    ; return $ TypeParamList l f -- funAttrs
                    }
 where
   args :: [Type] -> P ([Type], Bool)
   args l =  choice [ do { p <- pType
                         ; let l' = p:l
                         ; choice [ comma >> args l'
                                  , chartok ')' >> return (reverse l', False)
                                  ]
                         }
                    , symbol "..." >> chartok ')' >> return (reverse l, True)
                    , chartok ')' >> return (reverse l, False)
                    ]
  

pFormalParamList :: P FormalParamList
pFormalParamList = do { chartok '('
                      ; (l, f) <- args []
                      ; funAttrs <- many pFunAttr
                      ; return $ FormalParamList l f funAttrs
                      }
 where
   args :: [FormalParam] -> P ([FormalParam], Bool)
   args l =  choice [ do { p <- param
                         ; let l' = p:l
                         ; choice [ comma >> args l'
                                  , chartok ')' >> return (reverse l', False)
                                  ]
                         }
                    , symbol "..." >> chartok ')' >> return (reverse l, True)
                    , chartok ')' >> return (reverse l, False)
                    ]
    where
        param = do { at <- pType
                   ; ar1 <- many pParamAttr
                   ; an <- opt pAlign
                   ; lv <- opt pLocalId
                   ; let lv' = maybe FimplicitParam FexplicitParam lv 
                   ; ar2 <- many pParamAttr
                   ; return $ FormalParam at  ar1 an lv' ar2
                   }
