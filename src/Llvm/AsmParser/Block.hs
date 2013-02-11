{-# OPTIONS_GHC -Wall #-}
module Llvm.AsmParser.Block where
import Llvm.VmCore.Ast
import Llvm.AsmParser.Basic
import Llvm.AsmParser.Instruction

-------------------------------------------------------------------------------
pBlocks :: P [Block]
pBlocks = try (many block)
 where
   block :: P Block
   block = do { n <- pBlockLabel
              ; restInsts (Block n) [] []
              }

restInsts :: ([PhiInst] -> [ComputingInstWithDbg] -> TerminatorInstWithDbg -> Block) -> 
             [PhiInst] -> [ComputingInstWithDbg] -> P Block 
restInsts bf phis insts = do { instOrTerm <- pInstructionWithDbg
                             ; case instOrTerm of
                                 InstructionWithDbg (Phi inst) dbgs ->
                                     restInsts bf (inst:phis) insts
                                 InstructionWithDbg (Comp inst) dbgs -> 
                                     restInsts bf phis ((ComputingInstWithDbg inst dbgs):insts)
                                 InstructionWithDbg (Term x) dbgs -> 
                                     return $ bf (reverse phis) (reverse insts) (TerminatorInstWithDbg x dbgs)
                             }