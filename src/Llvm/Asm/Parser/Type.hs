module Llvm.Asm.Parser.Type where
import Llvm.Asm.Data
import Llvm.Asm.Parser.Basic


pType :: P Type
pType =
    do { t <- choice [ try (liftM Tprimitive pPrimType)
                     , pTypeName
                     --, upref
                     , array
                     , try pstruct
                     , vector
                     , struct
                     , pOther 
                     ]
       ; post t
       }
 where
--   upref    = chartok '\\' >> liftM TupRef decimal
   array    = brackets (avbody Tarray)
   vector   = angles (avbody Tvector)
   avbody f = do { n <- lexeme unsignedInt -- decimal
                 ; ignore $ chartok 'x'
                 ; t <- pType
                 ; return (f n t)
                 }
   struct   = liftM (Tstruct Unpacked) (braces (sepBy pType comma))
   pstruct  = do { ignore $ symbol "<{"
                 ; v <- sepBy pType comma
                 ; ignore $ symbol "}>"
                 ; return (Tstruct Packed v)
                 }
   pOther = choice [ reserved "opaque" >> return Topaque
                   , reserved "void" >> return Tvoid
                   ]


castTypeToMetaKind :: Type -> MetaKind
castTypeToMetaKind t = Mtype t                       


pMetaKind :: P MetaKind
pMetaKind = choice [ try (liftM castTypeToMetaKind pType)
                   , reserved "metadata" >> return Mmetadata
                   ]

pTypeName :: P Type
pTypeName = do { x <- pLocalId 
               ; case x of 
                 LocalIdNum n -> return $ Tno n
                 LocalIdAlphaNum s -> return $ Tname s
                 LocalIdDqString s -> return $ TquoteName s
               }


pStar :: P AddrSpace
pStar = option AddrSpaceUnspecified pAddrSpace >>= \x -> chartok '*' >> return x

post :: Type -> P Type
post t = do { pt <- option t (pStar >>= \x -> post (Tpointer t x))
            ; option pt $ do { fp <- pTypeParamList
                             ; fattrs <- many pFunAttr
                             ; post (Tfunction pt fp fattrs)
                             }
            }

pTypeI :: P TypePrimitive
pTypeI = (char 'i' >> liftM TpI unsignedInt)

pTypeF :: P TypePrimitive
pTypeF = (char 'f' >> liftM TpF unsignedInt)

pTypeV :: P TypePrimitive
pTypeV = (char 'v' >> liftM TpV unsignedInt)


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
                   , pTypeV
                   , reserved "null" >> return TpNull
                   ]
            
            
pTypeParamList :: P TypeParamList
pTypeParamList = do { ignore $ chartok '('
                    ; (l, f) <- args []
                    ; return $ TypeParamList l f
                    }
 where
   args :: [Type] -> P ([Type], Maybe VarArgParam)
   args l =  choice [ do { p <- pType
                         ; let l' = p:l
                         ; choice [ comma >> args l'
                                  , chartok ')' >> return (reverse l', Nothing)
                                  ]
                         }
                    , symbol "..." >> chartok ')' >> return (reverse l, Just VarArgParam)
                    , chartok ')' >> return (reverse l, Nothing)
                    ]
  

pFormalParamList :: P FormalParamList
pFormalParamList = do { ignore $ chartok '('
                      ; (l, f) <- args []
                      ; funAttrs <- many pFunAttr
                      ; return $ FormalParamList l f funAttrs
                      }
 where
   args :: [FormalParam] -> P ([FormalParam], Maybe VarArgParam)
   args l =  choice [ do { p <- param
                         ; let l' = p:l
                         ; choice [ comma >> args l'
                                  , chartok ')' >> return (reverse l', Nothing)
                                  ]
                         }
                    , symbol "..." >> chartok ')' >> return (reverse l, Just VarArgParam)
                    , chartok ')' >> return (reverse l, Nothing)
                    ]
    where
        param = choice [do { at <- pType
                           ; ar1 <- many pParamAttr
                           -- ; an <- opt pAlign
                           ; lv <- opt pLocalId
                           ; let lv' = maybe FimplicitParam FexplicitParam lv 
                           ; ar2 <- many pParamAttr
                           ; return $ FormalParamData at  ar1 {-an-} lv' ar2
                           }
                       , do { mt <- pMetaKind
                            ; lv <- opt pLocalId
                            ; let lv1 = maybe FimplicitParam FexplicitParam lv
                            ; return $ FormalParamMeta mt lv1
                            }
                       ]

pSelTy :: P Type
pSelTy = choice [ reserved "i1" >> return (Tprimitive $ TpI 1)
                , angles $ do { n <- unsignedInt -- decimal
                              ; ignore $ chartok 'x'
                              ; reserved "i1"
                              ; return $ Tvector n (Tprimitive $ TpI 1)
                              }
                , pTypeName
                ]
