{-# OPTIONS_GHC -Wall #-}
module Llvm.Syntax.Parser.Block where
import Llvm.Data.Ast
import Llvm.Syntax.Parser.Basic
import Llvm.Syntax.Parser.Instruction

-------------------------------------------------------------------------------
pBlocks :: P [Block]
pBlocks = try (many block)
 where
   block :: P Block
   block = do { n <- pBlockLabel
              ; restInsts (Block n) [] []
              }

restInsts :: ([PhiInstWithDbg] -> [ComputingInstWithDbg] -> TerminatorInstWithDbg -> Block) -> 
             [PhiInstWithDbg] -> [ComputingInstWithDbg] -> P Block 
restInsts bf phis insts = do { instOrTerm <- pInstructionWithDbg
                             ; case instOrTerm of
                                 InstructionWithDbg (Phi inst) dbgs ->
                                     restInsts bf ((PhiInstWithDbg inst dbgs):phis) insts
                                 InstructionWithDbg (Comp inst) dbgs -> 
                                     restInsts bf phis ((ComputingInstWithDbg inst dbgs):insts)
                                 InstructionWithDbg (Term x) dbgs -> 
                                     return $ bf (reverse phis) (reverse insts) (TerminatorInstWithDbg x dbgs)
                             }
