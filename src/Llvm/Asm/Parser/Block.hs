module Llvm.Asm.Parser.Block where
import Llvm.Asm.Data
import Llvm.Asm.Parser.Basic
import Llvm.Asm.Parser.Instruction

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
