module Llvm.AsmParser.Basic
    ( module Text.Parsec
    , module Llvm.AsmParser.Basic
    , module Control.Monad
    , module Text.Parsec.Perm
    )
    where

import Text.Parsec 
import Control.Monad
import Llvm.VmCore.Ast
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.Perm
import Data.Char
import Data.Functor.Identity (Identity)

-- dummy state
-- data DummyState = DummyState

type DummyState = ()

type P a = Parsec String DummyState a

initState :: DummyState
initState = () -- DummyState 

lexer :: T.GenTokenParser String u Data.Functor.Identity.Identity
lexer = T.makeTokenParser 
        (emptyDef 
         { T.commentLine = ";"
         , T.reservedNames = 
             [ "begin", "end", "true", "false", "declare", "define", "global", "constant"
             , "dllimport", "dllexport"
             , "extern_weak", "external", "thread_local", "zeroinitializer", "undef", "null", "to", "tail"
             , "target", "triple", "deplibs", "datalayout", "volatile", "nuw", "nsw", "exact", "inbounds", "align"
             , "addrspace", "section", "alias", "module", "asm", "sideeffect", "gc", "ccc", "cc", "fastcc"
             , "coldcc", "x86_stdcallcc", "x86_fastcallcc", "x86_thiscallcc", "arm_apcscc", "arm_aapcscc", "arm_aapcs_vfpcc"
             , "msp430_intrcc", "cc", "c", "signext", "zeroext", "inreg", "sret", "nounwind", "noreturn", "noalias"
             , "nocapture", "byval", "nest", "type", "opaque", "eq", "ne", "slt", "sgt"
             , "sle", "sge", "ult", "ugt", "ule", "uge", "oeq", "one", "olt", "ogt", "ole", "oge", "ord", "uno", "ueq"
             , "une", "x", "blockaddress"
             , "add",  "fadd", "sub",  "fsub", "mul",  "fmul", "udiv", "sdiv", "fdiv", "urem", "srem", "frem"
             , "shl",  "lshr", "ashr", "and",  "or",   "xor", "icmp", "fcmp", "phi", "call", "trunc"
             , "zext", "sext", "fptrunc", "fpext", "uitofp", "sitofp", "fptoui", "fptosi", "inttoptr", "ptrtoint"
             , "bitcast", "addrspacecast", "select", "va_arg", "ret", "br", "switch", "indirectbr"
             , "invoke", "unwind", "unreachable"
             , "alloca", "malloc", "load", "store", "getelementptr", "extractelement", "insertelement", "shufflevector"
             , "getresult", "extractvalue", "insertvalue", "free", "address_safety", "nonlazybind", "landingpad", "cleanup"
             , "catch", "filter", "personality", "half", "unnamed_addr", "singlethread", "acquire"
             , "release", "acq_rel", "seq_cst", "unordered", "monotonic", "atomic"
             , "atomicrmw", "xchg", "nand", "max", "min", "umax", "umin", "x86_mmx"
             , "call", "tail", "musttail"
             -- Linkage Types                                                             
             , "private", "internal", "available_externally", "linkonce", "weak", "common", "appending", "extern_weak"
             , "linkonce_odr", "weak_odr", "external"
             -- Calling Conventions
             , "ccc", "fastcc", "coldcc", "cc", "webkit_jscc", "anyregcc", "preserve_mostcc", "preserve_allcc"
             , "spir_kernel", "spir_func", "intel_ocl_bicc", "x86_stdcallcc", "x86_fastcallcc", "x86_thiscallcc"
             , "arm_apcscc", "arm_aapcscc", "arm_aapcs_vfpcc", "msp430_intrcc", "ptx_kernel", "ptx_device"
             , "x86_64_win64cc", "x86_64_sysvcc"
             -- Visibility Styles                                                                             
             , "default", "hidden", "protected"
             -- Parameter Attributes                                                             
             , "zeroext", "signext", "inreg", "byval", "inalloca", "sret", "noalias", "nocapture", "nest", "returned"
             , "nonnull", "dereferenceable"
             -- Function Attributes
             , "alignstack", "alwaysinline", "builtin", "cold", "inlinehint", "jumptable", "minsize", "naked", "nobuiltin"
             , "noduplicate", "noimplicitfloat", "noinline", "nonlazybind", "noredzone", "noreturn", "nounwind", "optnone"
             , "optsize", "readnone", "readonly", "returns_twice", "sanitize_address", "sanitize_memory", "sanitize_thread"
             , "ssp", "sspreq", "sspstrong", "uwtable"
             -- Fast math flags
             , "nnan", "ninf", "nsz", "arcp", "fast"
             , "inteldialect"
             , "<<label>>"
             ]
         })
          

integer :: P Integer
integer = T.integer lexer


decimal :: P Integer
decimal = lexeme (T.decimal lexer)


reserved :: String -> P ()
reserved = T.reserved lexer


symbol :: String -> P String
symbol = T.symbol lexer


lexeme :: P a -> P a
lexeme = T.lexeme lexer

intStrToken :: P String
intStrToken = lexeme numericLit <?> "intStringToken"


whiteSpace :: P ()
whiteSpace = T.whiteSpace lexer

ignore   :: P a -> P ()
ignore p = do { _ <- p ; return () }

chartok :: Char -> P Char
chartok = lexeme . char

complete :: P a -> P a
complete p = do { whiteSpace; x <- p; eof; return x }

-------------------------------------------------------------------------------
comma :: P String
comma = T.comma lexer

colon :: P String
colon = T.colon lexer

braces :: P a  -> P a
braces = T.braces lexer 

brackets :: P a -> P a
brackets = T.brackets lexer 

parens :: P a -> P a
parens = T.parens lexer

angles :: P a -> P a
angles = T.angles lexer

anglebraces :: P a -> P a
anglebraces p = between (symbol "<{") (symbol "}>") p


-------------------------------------------------------------------
idChar :: Char -> Bool
idChar c = isAlphaNum c || c == '$' || c == '.' || c == '_' || c == '-'    

pLocalId :: P LocalId
pLocalId = do { x <- lexeme (char '%' >> choice [ liftM LocalIdNum decimal
                                                , liftM (LocalIdAlphaNum . Lstring) (many1 (satisfy idChar))
                                                , liftM (LocalIdDqString . Lstring) pQuoteStr
                                                ])
              ; return x
              }

pGlobalId :: P GlobalId
pGlobalId = lexeme (char '@' >> choice [ liftM GlobalIdNum decimal
                                       , liftM (GlobalIdAlphaNum . Lstring) (many1 (satisfy idChar))
                                       , liftM (GlobalIdDqString . Lstring) pQuoteStr
                                       ])

pDollarId :: P DollarId
pDollarId = lexeme (char '$' >> choice [ liftM DollarIdNum decimal
                                       , liftM (DollarIdAlphaNum . Lstring) pId 
                                       , liftM (DollarIdDqString . Lstring) pQuoteStr
                                       ])


pMdVar :: P MdVar
pMdVar = lexeme $ do { ignore (char '!')
                     ; n <- choice [ liftM (\x -> [x]) (satisfy isAlpha)
                                   , char '\\' >> intStrToken ]
                     ; l <- many (satisfy idChar)
                     ; return $ MdVar (n++l)
                     }

pId :: P String
pId = do { n <- (satisfy isAlpha)
         ; l <- many (satisfy idChar)
         ; return $ n:l
         }
    
pGlobalOrLocalId :: P GlobalOrLocalId
pGlobalOrLocalId = do { x <- choice [liftM GolL pLocalId
                                    , liftM GolG pGlobalId ]
                      ; return x
                      }


numericLit :: P String
numericLit = many1 (satisfy isNumber)

pQuoteStr :: P String
pQuoteStr = lexeme ((char '"' >> (manyTill anyChar (char '"'))))
            
            
readInteger :: String -> Either String Integer            
readInteger x = case reads x :: [(Integer, String)] of
                  [(i,s)] | s == "" -> Right i
                  [] -> Left x                                  
                  _ -> Left x
                        
pLabelId :: P LabelId
pLabelId = choice [ do { s <- (many1 (satisfy lblChar))
                       ; case readInteger s of
                            Right x -> return $ LabelNumber x
                            Left x -> return $ LabelString $ Lstring x
                       }
                  , do { s <- pQuoteStr
                       ; case readInteger s of
                           Right x -> return $ LabelQuoteNumber x
                           Left x -> return $ LabelDqString $ Lstring x
                       }
                  ]
           where
             lblChar c = isAlphaNum c || c == '.' || c == '_' || c == '-' 
             
             
pLabelNumber :: P LabelId
pLabelNumber = do { x <- many1 (satisfy isDigit)
                  ; let i = read x:: Integer
                  ; return $ LabelNumber i
                  }
             
pAddrNaming :: P AddrNaming
pAddrNaming = option NamedAddr (reserved "unnamed_addr" >> return UnnamedAddr)
          
pPercentLabel :: P PercentLabel
pPercentLabel = lexeme (char '%' >> liftM PercentLabel pLabelId)

pTargetLabel :: P TargetLabel
pTargetLabel = reserved "label" >> liftM TargetLabel pPercentLabel

pExplicitBlockLabel :: P BlockLabel
pExplicitBlockLabel = lexeme (do { x <- pLabelId 
                                 ; _ <- char ':'
                                 ; return $ ExplicitBlockLabel x
                                 })
              
pImplicitBlockLabel :: P BlockLabel              
pImplicitBlockLabel = do { pos <- getPosition
                         ; return $ ImplicitBlockLabel (sourceName pos, sourceLine pos, sourceColumn pos)
                         }
                      

pBlockLabel :: P BlockLabel                      
pBlockLabel = choice [ try pExplicitBlockLabel
                     , pImplicitBlockLabel ]

pParamAttr :: P ParamAttr
pParamAttr = choice [ reserved "zeroext" >> return PaZeroExt
                    , reserved "signext" >> return PaSignExt
                    , reserved "inreg" >> return PaInReg
                    , reserved "byval" >> return PaByVal
                    , reserved "inalloca" >> return PaInAlloca
                    , reserved "sret" >> return PaSRet
                    , reserved "noalias" >> return PaNoAlias
                    , reserved "nocapture" >> return PaNoCapture
                    , reserved "nest" >> return PaNest
                    , reserved "returned" >> return PaReturned
                    , reserved "nonnull" >> return PaNonNull
                    , reserved "dereferenceable" >> liftM PaDereferenceable (parens decimal)
                    , reserved "readonly" >> return PaReadOnly
                    , reserved "readnone" >> return PaReadNone
                    , reserved "align" >> liftM PaAlign decimal
                    ]

pConvertOp :: P ConvertOp
pConvertOp = choice [ reserved "trunc" >> return Trunc
                    , reserved "zext" >> return Zext
                    , reserved "sext" >> return Sext
                    , reserved "fptrunc" >> return FpTrunc
                    , reserved "fpext" >> return FpExt
                    , reserved "fptoui" >> return FpToUi
                    , reserved "fptosi" >> return FpToSi
                    , reserved "uitofp" >> return UiToFp
                    , reserved "sitofp" >> return SiToFp
                    , reserved "ptrtoint" >> return PtrToInt
                    , reserved "inttoptr" >> return IntToPtr
                    , reserved "bitcast" >> return Bitcast
                    , reserved "addrspacecast" >> return AddrSpaceCast
                    ]

pCallConv :: P CallConv
pCallConv = choice [ try (reserved "ccc") >> return Ccc
                   , try (string "cc" >> liftM Cc intStrToken)
                   , reserved "cc" >> liftM Cc intStrToken
                   , reserved "fastcc" >> return CcFast
                   , reserved "coldcc" >> return CcCold
                   , reserved "webkit_jscc" >> return CcWebkitJs
                   , reserved "anyregcc" >> return CcAnyReg
                   , reserved "preserve_mostcc" >> return CcPreserveMost
                   , reserved "preserve_allcc" >> return CcPreserveAll
                   , reserved "spir_kernel" >> return CcSpirKernel
                   , reserved "spir_func" >> return CcSpirFunc
                   , reserved "intel_ocl_bicc" >> return CcIntelOclBi
                   , reserved "x86_stdcallcc" >> return CcX86StdCall
                   , reserved "x86_fastcallcc" >> return CcX86FastCall
                   , reserved "x86_thiscallcc" >> return CcX86ThisCall
                   , reserved "arm_apcscc" >> return CcArmApcs
                   , reserved "arm_aapcscc" >> return CcArmAapcs
                   , reserved "arm_aapcs_vfpcc" >> return CcArmAapcsVfp
                   , reserved "msp430_intrcc" >> return CcMsp430Intr
                   , reserved "ptx_kernel" >> return CcPtxKernel
                   , reserved "ptx_device" >> return CcPtxDevice
                   , reserved "x86_64_win64cc" >> return CcX86_64_Win64
                   , reserved "x86_64_sysvcc" >> return CcX86_64_SysV
                   ]


pVisibility :: P Visibility
pVisibility = (reserved "default" >> return VisDefault)
              <|> (reserved "hidden" >> return VisHidden)
              <|> (reserved "protected" >> return VisProtected)


pLinkage :: P Linkage
pLinkage = choice [ reserved "linker_private_weak_def_auto" >> return LinkagePrivate
                  , reserved "linker_private_weak" >> return LinkagePrivate
                  , reserved "private" >> return LinkagePrivate
                  , reserved "linker_private" >> return LinkagePrivate
                  , reserved "internal" >> return LinkageInternal
                  , reserved "external" >> return LinkageExternal
                  , reserved "available_externally" >> return LinkageAvailableExternally
                  , reserved "linkonce" >> return LinkageLinkonce
                  , reserved "weak" >> return LinkageWeak
                  , reserved "common" >> return LinkageCommon
                  , reserved "appending" >> return LinkageAppending
                  , reserved "extern_weak" >> return LinkageExternWeak
                  , reserved "linkonce_odr" >> return LinkageLinkonceOdr
                  , reserved "weak_odr" >> return LinkageWeakOdr
                  ]


opt :: P a -> P (Maybe a)
opt p = option Nothing (liftM Just (try p))


optCommaSep :: P a -> P (Maybe a)
optCommaSep p = opt (try (comma >> p))



pIbinaryOperator :: P IbinaryOperator
pIbinaryOperator = choice [ reserved "add" >> return Add
                          , reserved "sub" >> return Sub
                          , reserved "mul" >> return Mul
                          , reserved "udiv" >> return Udiv
                          , reserved "sdiv" >> return Sdiv
                          , reserved "urem" >> return Urem
                          , reserved "srem" >> return Srem
                          , reserved "shl" >> return Shl
                          , reserved "lshr" >> return Lshr
                          , reserved "ashr" >> return Ashr
                          , reserved "and" >> return And
                          , reserved "or" >> return Or
                          , reserved "xor" >> return Xor
                          ]

pFbinaryOperator :: P FbinaryOperator
pFbinaryOperator = choice [ reserved "fadd" >> return Fadd
                          , reserved "fsub" >> return Fsub
                          , reserved "fmul" >> return Fmul
                          , reserved "fdiv" >> return Fdiv
                          , reserved "frem" >> return Frem
                          ]


pIcmpOp :: P IcmpOp
pIcmpOp = choice [ reserved "eq" >> return IcmpEq
                 , reserved "ne" >> return IcmpNe
                 , reserved "slt" >> return IcmpSlt
                 , reserved "sgt" >> return IcmpSgt
                 , reserved "sle" >> return IcmpSle
                 , reserved "sge" >> return IcmpSge
                 , reserved "ult" >> return IcmpUlt
                 , reserved "ugt" >> return IcmpUgt
                 , reserved "ule" >> return IcmpUle
                 , reserved "uge" >> return IcmpUge
                 ]


pFcmpOp :: P FcmpOp
pFcmpOp = choice [ reserved "oeq" >> return FcmpOeq
                 , reserved "one" >> return FcmpOne
                 , reserved "olt" >> return FcmpOlt
                 , reserved "ogt" >> return FcmpOgt
                 , reserved "ole" >> return FcmpOle
                 , reserved "oge" >> return FcmpOge
                 , reserved "ord" >> return FcmpOrd
                 , reserved "uno" >> return FcmpUno
                 , reserved "ueq" >> return FcmpUeq
                 , reserved "une" >> return FcmpUne
                 , reserved "ult" >> return FcmpUlt
                 , reserved "ugt" >> return FcmpUgt
                 , reserved "ule" >> return FcmpUle
                 , reserved "uge" >> return FcmpUge
                 , reserved "true" >> return FcmpTrue
                 , reserved "false" >> return FcmpFalse
                 ]


pAliasLinkage :: P Linkage
pAliasLinkage = choice [ reserved "private" >> return LinkagePrivate
                       , reserved "internal" >> return LinkageInternal
                       , reserved "available_externally" >> return LinkageAvailableExternally
                       , reserved "linkonce" >> return LinkageLinkonce
                       , reserved "weak" >> return LinkageWeak
                       , reserved "common" >> return LinkageCommon
                       , reserved "appending" >> return LinkageAppending
                       , reserved "extern_weak" >> return LinkageExternWeak
                       , reserved "linkonce_odr" >> return LinkageLinkonceOdr
                       , reserved "weak_odr" >> return LinkageWeakOdr
                       , reserved "external" >> return LinkageExternal
                       ]

pDllStorageClass :: P DllStorageClass
pDllStorageClass = choice [ reserved "dllimport" >> return DscImport
                          , reserved "dllexport" >> return DscExport
                          ] 
pThreadLocalStorageClass :: P ThreadLocalStorage                   
pThreadLocalStorageClass = reserved "thread_local" >> option TlsNone (parens (choice [ reserved "localdynamic" >> return TlsLocalDynamic
                                                                                     , reserved "initialexec" >> return TlsInitialExec
                                                                                     , reserved "localexec" >> return TlsLocalExec
                                                                                     ]
                                                                             ))

pTailCall :: P TailCall
pTailCall = choice [ reserved "tail" >> return TcTailCall
                   , reserved "musttail" >> return TcMustTailCall
                   ]
{-
allLinkage :: P Either Linkage DllStorage
allLinkage = pExternalLinkage <|> pLinkage
-}

pFunAttr :: P FunAttr
pFunAttr =  choice [ reserved "alignstack" >> liftM FaAlignStack (parens decimal)
                   , reserved "alwaysinline" >> return FaAlwaysInline
                   , reserved "builtin" >> return FaBuiltin
                   , reserved "cold" >> return FaCold
                   , reserved "inlinehint" >> return FaInlineHint
                   , reserved "jumptable" >> return FaJumpTable
                   , reserved "minsize" >> return FaMinSize
                   , reserved "naked" >> return FaNaked
                   , reserved "nobuiltin" >> return FaNoBuiltin
                   , reserved "noduplicate" >> return FaNoDuplicate
                   , reserved "noimplicitfloat" >> return FaNoImplicitFloat
                   , reserved "noinline" >> return FaNoInline
                   , reserved "nonlazybind" >> return FaNonLazyBind
                   , reserved "noredzone" >> return FaNoRedZone
                   , reserved "noreturn" >> return FaNoReturn
                   , reserved "nounwind" >> return FaNoUnwind
                   , reserved "optnone" >> return FaOptNone
                   , reserved "optsize" >> return FaOptSize
                   , reserved "readnone" >> return FaReadNone
                   , reserved "readonly" >> return FaReadOnly
                   , reserved "returns_twice" >> return FaReturnsTwice
                   , reserved "sanitize_address" >> return FaSanitizeAddress
                   , reserved "sanitize_memory" >> return FaSanitizeMemory
                   , reserved "sanitize_thread" >> return FaSanitizeThread
                   , reserved "ssp" >> return FaSsp
                   , reserved "sspreq" >> return FaSspReq
                   , reserved "sspstrong" >> return FaSspStrong
                   , reserved "uwtable" >> return FaUwTable
                   , reserved "align" >> liftM FaAlign integer
                   , liftM DqString pQuoteStr >>= \s1 -> opt (symbol "=" >> liftM DqString pQuoteStr) >>= \s2 -> return (FaPair s1 s2)
                   ]

pCarry :: P TrapFlag
pCarry = choice [ reserved "nuw" >> return Nuw
                , reserved "nsw" >> return Nsw
                , reserved "exact" >> return Exact
                ]

pAlign :: P Alignment
pAlign = reserved "align" >> liftM Alignment decimal

pComdat :: P Comdat
pComdat = reserved "comdat" >> liftM Comdat (opt pDollarId)

pNontemporal :: P Nontemporal
pNontemporal =  char '!' >> reserved "nontemporal" >> char '!' >> liftM Nontemporal decimal 

pInvariantLoad :: P InvariantLoad
pInvariantLoad = char '!' >> reserved "invariant.load" >> char '!' >> liftM InvariantLoad decimal

pNonnull :: P Nonnull
pNonnull = char '!' >> reserved "nonnull" >> char '!' >> liftM Nonnull decimal

pSection :: P Section
pSection =  reserved "section" >> liftM (Section . DqString) pQuoteStr


pSelectionKind :: P SelectionKind
pSelectionKind = choice [ reserved "any" >> return Any
                        , reserved "exactmatch" >> return ExactMatch
                        , reserved "largest" >> return Largest
                        , reserved "noduplicates" >> return NoDuplicates
                        , reserved "samesize" >> return SameSize
                        ]

pAddrSpace :: P AddrSpace
pAddrSpace = reserved "addrspace" >> liftM AddrSpace (parens decimal)
             
pGlobalType :: P GlobalType 
pGlobalType = choice [ reserved "constant" >> return (GlobalType "constant")
                     , reserved "global" >> return (GlobalType "global")
                     ]


pFenceOrder :: P AtomicMemoryOrdering
pFenceOrder = choice [ reserved "acquire" >> return AmoAcquire
                     , reserved "release" >> return AmoRelease
                     , reserved "acq_rel" >> return AmoAcqRel
                     , reserved "seq_cst" >> return AmoSeqCst
                     , reserved "unordered" >> return AmoUnordered
                     , reserved "monotonic" >> return AmoMonotonic
                     ]

pAtomicOp :: P AtomicOp
pAtomicOp = choice [ reserved "xchg" >> return AoXchg
                   , reserved "add" >> return AoAdd
                   , reserved "sub" >> return AoSub
                   , reserved "and" >> return AoAnd
                   , reserved "nand" >> return AoAnd
                   , reserved "or" >> return AoOr
                   , reserved "xor" >> return AoXor
                   , reserved "max" >> return AoMax
                   , reserved "min" >> return AoMin
                   , reserved "umax" >> return AoUmax
                   , reserved "umin" >> return AoUmin
                   ]

pFunAttrCollection :: P [FunAttr] -- Collection          
pFunAttrCollection = many $ choice [ char '#' >> liftM FaGroup decimal
                                   , pFunAttr
                                   ]



pTuple2 :: P a -> P b -> P (a, b)
pTuple2 p1 p2 = do { a1 <- p1
                   ; ignore comma
                   ; a2 <- p2
                   ; return (a1, a2)
                   }

pTriple3 :: P a -> P b -> P c -> P (a, b, c)
pTriple3 p1 p2 p3 = do { a1 <- p1
                       ; ignore comma
                       ; a2 <- p2
                       ; ignore comma
                       ; a3 <- p3
                       ; return (a1, a2, a3)
                       }


pTuple :: P a -> P (a, a)
pTuple p = pTuple2 p p

pTriple :: P a -> P (a, a, a)
pTriple p = pTriple3 p p p

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d 
uncurry3 f (a1, b1, c1) = f a1 b1 c1
    
                          
                          
pFastMathFlag :: P FastMathFlag
pFastMathFlag = choice [ reserved "nnan" >> return Fmfnnan
                       , reserved "ninf" >> return Fmfninf
                       , reserved "nsz" >> return Fmfnsz
                       , reserved "arcp" >> return Fmfarcp
                       , reserved "fast" >> return Fmffast
                       ]
                
pFastMathFlags :: P FastMathFlags                
pFastMathFlags = liftM FastMathFlags (many pFastMathFlag)