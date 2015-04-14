{-# OPTIONS_GHC -Wall #-}
module Llvm.Syntax.Parser.Instruction where
import Llvm.Data.Ast
import Llvm.Syntax.Parser.Basic
import Llvm.Syntax.Parser.Const
import Llvm.Syntax.Parser.Type
import Llvm.Syntax.Parser.Rhs

pTerminatorInst :: P TerminatorInst
pTerminatorInst = choice [ pRet
                         , pBr
                         , pSwitch
                         , reserved "indirectbr" >> liftM (uncurry IndirectBr)
                           (pTuple2 pTypedValue (brackets (sepBy pTargetLabel comma)))
                         , reserved "unreachable" >> return Unreachable
                         , reserved "resume" >> liftM Resume pTypedValue
                         ]
                  <?> "control instruction"

pRet :: P TerminatorInst
pRet = do { reserved "ret"
          ; t <- pType
          ; case t of
              Tvoid -> return RetVoid
              _  -> do { v <- pValue
                       ; option (Return $ [Typed t v])
                         (do { ls <- many (try (comma >> pTypedValue))
                             ; return (Return ((Typed t v):ls))
                             }
                         )
                       }
          }



pSwitch :: P TerminatorInst
pSwitch = do { reserved "switch"
             ; (tv,l) <- tvl
             ; arms <- brackets (many tvl)
             ; return $ Switch tv l arms
             }
 where
   tvl = pTuple2 pTypedValue pTargetLabel


pBr :: P TerminatorInst
pBr = reserved "br" >> choice [ reserved "label" >> liftM (Br . TargetLabel) pPercentLabel
                              , reserved "i1" >> liftM (uncurry3 Cbr) (pTriple3 pValue pTargetLabel pTargetLabel)
                              ]


data Instruction = Comp ComputingInst
                 | Term TerminatorInst
                 | Phi PhiInst

data InstructionWithDbg = InstructionWithDbg Instruction [Dbg]

pInstruction :: P Instruction
pInstruction = do { lhs <- opt (do { x <- pLocalId
                                   ; ignore (chartok '=')
                                   ; return x
                                   })
                  ; choice [ try $ liftM Term $ pInvoke lhs
                           , liftM Comp $ pComputingInst lhs
                           , liftM Term pTerminatorInst
                           , liftM Phi $ pPhiInst lhs
                           ]
                  }

pDbg :: P Dbg
pDbg = do { x <- pMdVar
          ; mc <- pMetaConst
          ; return $ Dbg x mc
          }

pInstructionWithDbg :: P InstructionWithDbg
pInstructionWithDbg  = do { ins <- pInstruction
                          ; l <- many (comma >> pDbg)
                          ; return $ InstructionWithDbg ins l
                          }



pComputingInst ::  Maybe LocalId -> P ComputingInst
pComputingInst lhs = do { rhs <- pRhs
                        ; return $ ComputingInst lhs rhs
                        }


pInvoke :: Maybe LocalId -> P TerminatorInst
pInvoke lhs = do { reserved "invoke"
                 ; callSite <- pCallSite
                 ; reserved "to"
                 ; toLbl <- pTargetLabel
                 ; reserved "unwind"
                 ; unwindLbl <- pTargetLabel
                 ; return $ Invoke lhs callSite toLbl unwindLbl
                 }


pPhiInst :: Maybe LocalId -> P PhiInst
pPhiInst lhsOpt = do { reserved "phi"
                     ; t <- pType
                     ; one <- pair
                     ; ls <- many (try (comma >> pair))
                     ; return $ PhiInst lhsOpt t (one:ls)
                     }
  where pair = brackets (pTuple2 pValue pPercentLabel)

