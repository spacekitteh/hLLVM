{-# OPTIONS_GHC -Wall #-}
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

-- dummy state
data DummyState = DummyState


type P a = Parsec String DummyState a

initState :: DummyState
initState = DummyState 

lexer = T.makeTokenParser 
        (emptyDef 
         { T.commentLine = ";"
         , T.reservedNames = 
             [ "begin", "end", "true", "false", "declare", "define", "global", "constant", "private", "linker_private"
             , "linker_private_weak", "linker_private_weak_def_auto", "internal", "available_externally", "linkonce"
             , "linkonce_odr", "weak", "weak_odr", "appending", "dllimport", "dllexport", "common", "default", "hidden"
             , "protected", "extern_weak", "external", "thread_local", "zeroinitializer", "undef", "null", "to", "tail"
             , "target", "triple", "deplibs", "datalayout", "volatile", "nuw", "nsw", "exact", "inbounds", "align"
             , "addrspace", "section", "alias", "module", "asm", "sideeffect", "alignstack", "gc", "ccc", "cc", "fastcc"
             , "coldcc", "x86_stdcallcc", "x86_fastcallcc", "x86_thiscallcc", "arm_apcscc", "arm_aapcscc", "arm_aapcs_vfpcc"
             , "msp430_intrcc", "cc", "c", "signext", "zeroext", "inreg", "sret", "nounwind", "noreturn", "noalias"
             , "nocapture", "byval", "nest", "readnone", "readonly", "inlinehint", "noinline", "alwaysinline", "optsize"
             , "ssp", "sspreq", "noredzone", "noimplicitfloat", "naked", "type", "opaque", "eq", "ne", "slt", "sgt"
             , "sle", "sge", "ult", "ugt", "ule", "uge", "oeq", "one", "olt", "ogt", "ole", "oge", "ord", "uno", "ueq"
             , "une", "x", "blockaddress"
             , "add",  "fadd", "sub",  "fsub", "mul",  "fmul", "udiv", "sdiv", "fdiv", "urem", "srem", "frem"
             , "shl",  "lshr", "ashr", "and",  "or",   "xor", "icmp", "fcmp", "phi", "call", "trunc"
             , "zext", "sext", "fptrunc", "fpext", "uitofp", "sitofp", "fptoui", "fptosi", "inttoptr", "ptrtoint"
             , "bitcast", "select", "va_arg", "ret", "br", "switch", "indirectbr", "invoke", "unwind", "unreachable"
             , "alloca", "malloc", "load", "store", "getelementptr", "extractelement", "insertelement", "shufflevector"
             , "getresult", "extractvalue", "insertvalue", "free", "address_safety", "nonlazybind", "landingpad", "cleanup"
             , "catch", "filter", "personality", "half", "unnamed_addr", "uwtable", "singlethread", "acquire"
             , "release", "acq_rel", "seq_cst", "unordered", "monotonic", "atomic"
             , "atomicrmw", "xchg", "nand", "max", "min", "umax", "umin", "x86_mmx"
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
                                                , liftM (LocalIdQuoteStr . Lstring) pQuoteStr
                                                ])
              ; return x
              }

pGlobalId :: P GlobalId
pGlobalId = lexeme (char '@' >> choice [ liftM GlobalIdNum decimal
                                       , liftM (GlobalIdAlphaNum . Lstring) (many1 (satisfy idChar))
                                       , liftM (GlobalIdQuoteStr . Lstring) pQuoteStr
                                       ])

   
            
pMdVar :: P MdVar
pMdVar = lexeme $ do { ignore (char '!')
                     ; n <- (satisfy isAlpha)
                     ; l <- many (satisfy idChar)
                     ; return $ MdVar (n:l)
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
                           Left x -> return $ LabelQuoteString $ Lstring x
                       }
                  ]
           where
             lblChar c = isAlphaNum c || c == '.' || c == '_' || c == '-' 
             
             
pLabelNumber :: P LabelId
pLabelNumber = do { x <- many1 (satisfy isDigit)
                  ; let i = read x:: Integer
                  ; return $ LabelNumber i
                  }
             
          
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
pImplicitBlockLabel = return ImplicitBlockLabel 
                      

pBlockLabel :: P BlockLabel                      
pBlockLabel = choice [ try pExplicitBlockLabel
                     , pImplicitBlockLabel ]

pParamAttr :: P ParamAttr
pParamAttr = choice [ reserved "zeroext" >> return ZeroExt
                    , reserved "signext" >> return SignExt
                    , reserved "inreg" >> return InReg
                    , reserved "byval" >> return ByVal
                    , reserved "sret" >> return SRet
                    , reserved "noalias" >> return NoAlias
                    , reserved "nocapture" >> return NoCapture
                    , reserved "nest" >> return Nest
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
                    ]

pCallConv :: P CallConv
pCallConv = choice [ try (reserved "ccc") >> return Ccc
                   , try (string "cc" >> liftM Cc intStrToken)
                   , reserved "cc" >> liftM Cc intStrToken
                   , reserved "fastcc" >> return FastCc
                   , reserved "coldcc" >> return ColdCc
                   , reserved "x86_stdcallcc" >> return X86StdCall
                   , reserved "x86_fastcallcc" >> return X86FastCall
                   , reserved "x86_thiscallcc" >> return X86ThisCall
                   , reserved "arm_apcscc" >> return ArmApcs
                   , reserved "arm_aapcscc" >> return ArmAapcs
                   , reserved "arm_aapcs_vfpcc" >> return ArmAapcsVfp
                   , reserved "msp430_intrcc" >> return Msp430Intr
                   ]


pVisibility :: P Visibility
pVisibility = (reserved "default" >> return Default)
              <|> (reserved "hidden" >> return Hidden)
              <|> (reserved "protected" >> return Protected)


pLinkage :: P Linkage
pLinkage = choice [ reserved "private" >> return Private
                  , reserved "linker_private_weak_def_auto" >> return LinkerPrivateWeakDefAuto
                  , reserved "linker_private_weak" >> return LinkerPrivateWeak
                  , reserved "linker_private" >> return LinkerPrivate
                  , reserved "internal" >> return Internal
                  , reserved "external" >> return External
                  , reserved "available_externally" >> return AvailableExternally
                  , reserved "linkonce" >> return Linkonce
                  , reserved "weak" >> return Weak
                  , reserved "common" >> return Common
                  , reserved "appending" >> return Appending
                  , reserved "extern_weak" >> return ExternWeak
                  , reserved "linkonce_odr" >> return LinkonceOdr
                  , reserved "weak_odr" >> return WeakOdr
                  , reserved "dllimport" >> return DllImport
                  , reserved "dllexport" >> return DllExport
                  ]


opt :: P a -> P (Maybe a)
opt p = option Nothing (liftM Just (try p))


optCommaSep :: P a -> P (Maybe a)
optCommaSep p = opt (try (comma >> p))



pBinaryOperator :: P BinaryOperator
pBinaryOperator = choice [ reserved "add" >> return Add
                         , reserved "fadd" >> return Fadd
                         , reserved "sub" >> return Sub
                         , reserved "fsub" >> return Fsub
                         , reserved "mul" >> return Mul
                         , reserved "fmul" >> return Fmul
                         , reserved "udiv" >> return Udiv
                         , reserved "sdiv" >> return Sdiv
                         , reserved "fdiv" >> return Fdiv
                         , reserved "urem" >> return Urem
                         , reserved "srem" >> return Srem
                         , reserved "frem" >> return Frem
                         , reserved "shl" >> return Shl
                         , reserved "lshr" >> return Lshr
                         , reserved "ashr" >> return Ashr
                         , reserved "and" >> return And
                         , reserved "or" >> return Or
                         , reserved "xor" >> return Xor
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
pAliasLinkage = choice [ reserved "external" >> return External
                       , reserved "internal" >> return Internal
                       , reserved "weak" >> return Weak
                       , reserved "weak_odr" >> return WeakOdr
                       , reserved "private" >> return Private
                       , reserved "linker_private" >> return LinkerPrivate
                       , reserved "linker_private_weak" >> return LinkerPrivateWeak
                       , reserved "linker_private_weak_def_auto" >> 
                                  return LinkerPrivateWeakDefAuto
                       ]

pExternalLinkage :: P Linkage
pExternalLinkage = choice [ reserved "external" >> return External
                          , reserved "extern_weak" >> return ExternWeak
                          , reserved "dllimport" >> return DllImport
                          ]

allLinkage :: P Linkage
allLinkage = pExternalLinkage <|> pLinkage


pFunAttr :: P FunAttr
pFunAttr =  choice [ reserved "alignstack" >> liftM AlignStack (parens decimal)
                   , reserved "address_safety" >> return AddressSafety
                   , reserved "alwaysinline" >> return AlwaysInline
                   , reserved "nonlazybind" >> return NonLazyBind
                   , reserved "inlinehint" >> return InlineHint
                   , reserved "naked" >> return Naked
                   , reserved "noimplicitfloat" >> return NoImplicitFloat
                   , reserved "noinline" >> return NoInline
                   , reserved "noredzone" >> return NoRedZone
                   , reserved "noreturn" >> return NoReturn
                   , reserved "nounwind" >> return NoUnwind
                   , reserved "optsize" >> return OptSize
                   , reserved "readnone" >> return ReadNone
                   , reserved "readonly" >> return ReadOnly
                   , reserved "returns_twice" >> return ReturnsTwice
                   , reserved "ssp" >> return Ssp
                   , reserved "sspreq" >> return SspReq
                   , reserved "uwtable" >> return UwTable
                   , reserved "unnamed_addr" >> return UnnamedAddr
                   ]

pCarry :: P TrapFlag
pCarry = choice [ reserved "nuw" >> return Nuw
                , reserved "nsw" >> return Nsw
                , reserved "exact" >> return Exact
                ]

pAlign :: P Align
pAlign = reserved "align" >> liftM Align decimal


pSection :: P Section
pSection =  reserved "section" >> liftM (Section . QuoteStr) pQuoteStr


pTargetKind :: P TargetKind
pTargetKind = (reserved "triple" >> return Triple)
              <|> (reserved "datalayout" >> return Datalayout)


pAddrSpace :: P AddrSpace
pAddrSpace = reserved "addrspace" >> liftM AddrSpace (parens decimal)
             
pGlobalType :: P GlobalType 
pGlobalType = choice [ reserved "constant" >> return (GlobalType "constant")
                     , reserved "global" >> return (GlobalType "global")
                     ]


pFenceOrder :: P FenceOrder
pFenceOrder = choice [ reserved "acquire" >> return Acquire
                     , reserved "release" >> return Release
                     , reserved "acq_rel" >> return AcqRel
                     , reserved "seq_cst" >> return SeqCst
                     , reserved "unordered" >> return Unordered
                     , reserved "monotonic" >> return Monotonic
                     ]

pAtomicOp :: P AtomicOp
pAtomicOp = choice [ reserved "xchg" >> return Axchg
                   , reserved "add" >> return Aadd
                   , reserved "sub" >> return Asub
                   , reserved "and" >> return Aand
                   , reserved "nand" >> return Anand
                   , reserved "or" >> return Aor
                   , reserved "xor" >> return Axor
                   , reserved "max" >> return Amax
                   , reserved "min" >> return Amin
                   , reserved "umax" >> return Aumax
                   , reserved "umin" >> return Aumin
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
    
                          
                          
                          