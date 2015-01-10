{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import System.Console.CmdArgs
import Data.Maybe
import ParserTester
import Ast2IrTester
import Ir2AstTester
import Llvm.Pass.Optimizer 
import qualified Llvm.Pass.Mem2Reg as M2R
import qualified Llvm.Pass.Liveness as L
import Llvm.Pass.PassManager
import qualified Compiler.Hoopl as H
import Llvm.VmCore.AstCanonicalization
import qualified Llvm.Pass.NormalGraph as N
import qualified Llvm.Pass.Optimizer as O
-- mport Llvm.VmCore.AstWriter

toStep "mem2reg" = Just Mem2Reg
toStep "dce" = Just Dce
toStep _ = Nothing


extractSteps :: [String] -> [Step]
extractSteps l = map (\x -> case toStep x of
                         Just s -> s
                         Nothing -> error (x ++ " is not step")) l
                 

data Sample = Parser { input :: FilePath, output :: Maybe String }
            | Ast2Ir { input :: FilePath, output :: Maybe String }
            | Ir2Ast { input :: FilePath, output :: Maybe String }
            | Pass { input :: FilePath, output :: Maybe String, step :: [String], fuel :: Int }
            | PhiElim { input :: FilePath, output :: Maybe String, fuel :: Int }
            | AstCanonic { input :: FilePath, output :: Maybe String }
            deriving (Show, Data, Typeable, Eq)
                                       
                     
outFlags x = x &= help "Output file, stdout is used if it's not specified" &= typFile
         
parser = Parser { input = def &= typ "<INPUT>" &= argPos 0
                , output = outFlags Nothing
                } &= help "Test LLVM Parser"
  
ast2ir = Ast2Ir { input = def &= typ "<INPUT>" &= argPos 0
                , output = outFlags Nothing
                } &= help "Test Ast to Ir conversion"
         
ir2ast = Ir2Ast { input = def &= typ "<INPUT>" &= argPos 0
                , output = outFlags Nothing
                } &= help "Test Ir to Ast conversion"
         
astcanonic = AstCanonic { input = def &= typ "<INPUT>" &= argPos 0
                          , output = outFlags Nothing
                          } &= help "Test Ir to Ast conversion"         
              
pass = Pass { input = def &= typ "<INPUT>" &= argPos 0
            , output = outFlags Nothing
            , fuel = def &= typ "FUEL" &= help "The fuel used to run the pass"
            , step = def &= typ "STEP" &= help "Supported steps : mem2reg, dce. Multiple passes are supported by specifying multiple --step s, e.g., --step=mem2reg --step=dce"
            } &= help "Test Optimization pass"
         

phielim = PhiElim { input = def &= typ "<INPUT>" &= argPos 0
                  , output = outFlags Nothing
                  , fuel = def &= typ "FUEL" &= help "The fuel used to run the pass"
                  } &= help "Test PhiElim pass"

         
mode = cmdArgsMode $ modes [parser, ast2ir, ir2ast, pass, astcanonic, phielim] &= help "Test sub components" 
       &= program "Test" &= summary "Test driver v1.0"
       
       
       

main :: IO ()
main = do { sel <- cmdArgsRun mode
          ; putStr $ show sel
          ; case sel of
            Parser ix ox -> do { inh <- openFile ix ReadMode
                               ; outh <- openFileOrStdout ox
                               ; m <- testParser ix inh 
                               ; writeOutLlvm m outh
                               ; hClose inh
                               ; closeFileOrStdout ox outh
                               }
            Ast2Ir ix ox -> do { inh <- openFile ix ReadMode
                               ; outh <- openFileOrStdout ox
                               ; ast <- testParser ix inh
                               ; let (m, ir) = testAst2Ir ast
                               ; writeOutIr ir outh
                               ; hClose inh
                               ; closeFileOrStdout ox outh
                               }
            Ir2Ast ix ox -> do { inh <- openFile ix ReadMode
                               ; outh <- openFileOrStdout ox
                               ; ast <- testParser ix inh
                               ; let (m, ir) = testAst2Ir ast
                                     ast' = testIr2Ast m ir
                               ; writeOutLlvm ast' outh
                               ; hClose inh
                               ; closeFileOrStdout ox outh
                               }
            PhiElim ix ox f -> do { inh <- openFile ix ReadMode
                                ; outh <- openFileOrStdout ox
                                ; ast <- testParser ix inh
                                ; let (m, ir) = testAst2Ir ast
                                ; let ir' = H.runSimpleUniqueMonad $ H.runWithFuel f (O.optModule1 () N.killphi ir)
                                ; let ast' = testIr2Ast m ir'
                                ; writeOutLlvm ast' outh
                                ; hClose inh
                                ; closeFileOrStdout ox outh
                                }
            AstCanonic ix ox -> do { inh <- openFile ix ReadMode
                                   ; outh <- openFileOrStdout ox
                                   ; ast <- testParser ix inh
                                   ; let ast' = canonicalize ast
                                   ; writeOutLlvm ast' outh
                                   ; hClose inh
                                   ; closeFileOrStdout ox outh
                                   }
            Pass ix ox passes f -> do { inh <- openFile ix ReadMode
                                      ; outh <- openFileOrStdout ox
                                      ; ast' <- testParser ix inh
                                      ; let ast = canonicalize ast'
                                      ; let (m, ir) = testAst2Ir ast
                                      ; let applySteps' = applySteps (extractSteps passes) ir
                                      ; let ir' = H.runSimpleUniqueMonad $ H.runWithFuel f applySteps'
                                      ; let ir'' = H.runSimpleUniqueMonad $ H.runWithFuel f (O.optModule1 () N.killphi ir')
                                      ; let ast' = testIr2Ast m ir''
                                      ; writeOutLlvm ast' outh
                                      ; hClose inh
                                      ; closeFileOrStdout ox outh
                                      }
            _ -> error $ "unexpected option " ++ show sel
          }


openFileOrStdout :: Maybe FilePath -> IO Handle
openFileOrStdout Nothing = return stdout
openFileOrStdout (Just x) = openFile x WriteMode


closeFileOrStdout :: Maybe FilePath -> Handle -> IO ()
closeFileOrStdout Nothing h = hFlush h
closeFileOrStdout (Just x) h = hClose h
