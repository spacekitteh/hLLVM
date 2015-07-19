{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
import System.IO
import System.Console.CmdArgs
import ParserTester
import Llvm.Pass.Optimizer ()
import qualified Llvm.Pass.Mem2Reg as M2R ()
import qualified Llvm.Pass.Liveness as L ()
import qualified Llvm.Hir.Data as I
import qualified Llvm.Asm as A
import Llvm.Query.HirCxt
import Llvm.Pass.PassManager
import qualified Compiler.Hoopl as H
import qualified Llvm.AsmHirConversion as Cv
import qualified Llvm.Pass.NormalGraph as N
import qualified Llvm.Pass.Optimizer as O
import qualified Llvm.Pass.PassTester as T
import qualified Llvm.Pass.DataUsage as Du
import qualified Llvm.Hir.Print as P
import qualified Llvm.Pass.Substitution as Sub
import qualified Llvm.Pass.Changer as Cg
import qualified Llvm.Pass.Visualization as Vis
import qualified Llvm.Pass.SizeofVerification as Tveri
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (stripPrefix)
import Llvm.Matcher

toStep "mem2reg" = Just Mem2Reg
toStep "dce" = Just Dce
toStep _ = Nothing

chg :: Cg.Changer () ()
chg = Cg.defaultChanger { Cg.change_GlobalId = \x -> case x of 
                             I.GlobalIdAlphaNum s -> case stripPrefix "llvm." s of
                               Nothing -> I.GlobalIdAlphaNum (s ++ "_g_")
                               Just _ -> x
                             _ -> x
                        , Cg.change_LocalId = \x -> case x of
                             I.LocalIdAlphaNum s -> case stripPrefix "llvm." s of
                               Nothing -> I.LocalIdAlphaNum (s ++ "_l_")
                               Just _ -> x
                             _ -> x
                        }
      
extractSteps :: [String] -> [Step]
extractSteps l = map (\x -> case toStep x of
                         Just s -> s
                         Nothing -> error (x ++ " is not step")
                     ) l

data Sample = Dummy { input :: FilePath, output :: Maybe String }
            | Parser { input :: FilePath, output :: Maybe String, showAst :: Bool }
            | Ast2Ir { input :: FilePath, output :: Maybe String }
            | Ir2Ast { input :: FilePath, output :: Maybe String }
            | SizeofVerification { input :: FilePath, output :: Maybe String }              
            | Pass { input :: FilePath, output :: Maybe String, step :: [String], fuel :: Int }
            | PhiFixUp { input :: FilePath, output :: Maybe String, fuel :: Int }
            | AstCanonic { input :: FilePath, output :: Maybe String }
            | DataUse { input :: FilePath, output :: Maybe String, fuel ::Int}
            | Change { input :: FilePath, output :: Maybe String}
            | Visualize { input :: FilePath, output :: Maybe String}
            | SrcInfoDbg { input :: FilePath, output :: Maybe String}              
            deriving (Show, Data, Typeable, Eq)

outFlags x = x &= help "Output file, stdout is used if it's not specified" &= typFile

dummy = Dummy { input = def &= typ "<INPUT>"
              , output = outFlags Nothing
              } &= help "Test LLVM Parser"

parser = Parser { input = def &= typ "<INPUT>"
                , output = outFlags Nothing
                , showAst = False
                } &= help "Test LLVM Parser"

ast2ir = Ast2Ir { input = def &= typ "<INPUT>"
                , output = outFlags Nothing
                } &= help "Test Ast2Ir conversion"

ir2ast = Ir2Ast { input = def &= typ "<INPUT>"
                , output = outFlags Nothing
                } &= help "Test Ir2Ast conversion"

typeq = SizeofVerification { input = def &= typ "<INPUT>"
                              , output = outFlags Nothing
                              } &= help "Test Type Query Verification pass"

astcanonic = AstCanonic { input = def &= typ "<INPUT>"
                        , output = outFlags Nothing
                        } &= help "Test AstCanonic conversion"

datause = DataUse { input = def &= typ "<INPUT>"
                  , output = outFlags Nothing
                  , fuel = H.infiniteFuel &= typ "FUEL" &= help "the fuel used to rewrite the code, the default is infiniteFuel"
                  } &= help "Test DataUsage Pass"

changer = Change { input = def &= typ "<INPUT>"
                 , output = outFlags Nothing
                 } &= help "Test Change Pass"

visual = Visualize { input = def &= typ "<INPUT>"
                   , output = outFlags Nothing
                   } &= help "Test Visialize Pass"

srcInfo = SrcInfoDbg { input = def &= typ "<INPUT>"
                     , output = outFlags Nothing
                     } &= help "SrcInfo test pass"


pass = Pass { input = def &= typ "<INPUT>"
            , output = outFlags Nothing
            , fuel = H.infiniteFuel &= typ "FUEL" &= help "The fuel used to run the pass, the default is infiniteFuel"
            , step = def &= typ "STEP" &= help "Supported steps : mem2reg, dce. Multiple passes are supported by specifying multiple --step s, e.g., --step=mem2reg --step=dce"
            } &= help "Test Optimization pass"

phifixup = PhiFixUp { input = def &= typ "<INPUT>"
                    , output = outFlags Nothing
                    , fuel = H.infiniteFuel &= typ "FUEL" &= help "The fuel used to run the pass"
                    } &= help "Test PhiFixUp pass"

mode = cmdArgsMode $ modes [dummy, parser, ast2ir, ir2ast
                           ,pass, astcanonic, phifixup
                           ,datause, changer, visual
                           ,srcInfo, typeq] &= help "Test sub components"
       &= program "Test" &= summary "Test driver v1.0"

main :: IO ()
main = do { sel <- cmdArgsRun mode
#ifdef DEBUG
          ; putStr $ show sel
#endif
          ; case sel of
            Parser ix ox sh -> do { inh <- openFile ix ReadMode
                                  ; m <- testParser ix inh
                                  ; if sh then
                                      do { swth <- openFileOrStdout (fmap (\x -> x ++ ".show") ox)
                                         ; writeOutShow m swth
                                         ; closeFileOrStdout ox swth
                                         }
                                      else 
                                      do { outh <- openFileOrStdout ox
                                         ; writeOutLlvm m outh
                                         ; closeFileOrStdout ox outh
                                         }
                                  ; hClose inh
                                  }
            AstCanonic ix ox -> do { inh <- openFile ix ReadMode
                                   ; outh <- openFileOrStdout ox
                                   ; ast <- testParser ix inh
                                   ; let ast' = A.simplify ast
                                   ; writeOutLlvm ast' outh
                                   ; hClose inh
                                   ; closeFileOrStdout ox outh
                                   }
            Ast2Ir ix ox -> do { inh <- openFile ix ReadMode
                               ; outh <- openFileOrStdout ox
                               ; ast <- testParser ix inh
                               ; let ast' = A.simplify ast
                                     {-
                               ; let (I.SpecializedModule _ ir) = mapModule id Nothing ast'
                                     -- ; let (m, ir) = H.runSimpleUniqueMonad ((Cv.asmToHir ast')::H.SimpleUniqueMonad (Cv.IdLabelMap, I.Module ()))
                               ; writeOutIr ir outh
                                     -}
                               ; hClose inh
                               ; closeFileOrStdout ox outh
                               }
            Ir2Ast ix ox -> do { inh <- openFile ix ReadMode
                               ; outh <- openFileOrStdout ox
                               ; ast <- testParser ix inh
                               ; let ast' = A.simplify ast
                               ; let ast'' = transformModule id Nothing ast' 
                               ; writeOutLlvm ast'' outh
                               ; hClose inh
                               ; closeFileOrStdout ox outh
                               }
            SizeofVerification ix ox -> 
              do { inh <- openFile ix ReadMode
                 ; outh <- openFileOrStdout ox
                 ; ast <- testParser ix inh
                 ; let ast' = A.simplify ast
                 ; let ast'' = transformModule (Tveri.mkVerificationModule) Nothing ast' -- testIr2Ast m testIr
                 ; writeOutLlvm ast'' outh
                 ; hClose inh
                 ; closeFileOrStdout ox outh
                 }                            
            Change ix ox -> do { inh <- openFile ix ReadMode
                               ; outh <- openFileOrStdout ox
                               ; ast <- testParser ix inh
                               ; let ast' = A.simplify ast
                               ; let ast'' = transformModule2  
                                             (\(I.SpecializedModule dlm m) -> 
                                               (I.SpecializedModule dlm $ Sub.substitute chg m, Sub.substitute chg)) Nothing ast' 
                               ; writeOutLlvm ast'' outh
                               ; hClose inh
                               ; closeFileOrStdout ox outh
                               }
            Visualize ix ox -> do { inh <- openFile ix ReadMode
                                  ; outh <- openFileOrStdout ox
                                  ; ast <- testParser ix inh
                                  ; let ast' = A.simplify ast
                                  ; let ast'' = transformModule (\(I.SpecializedModule dlm m) -> I.SpecializedModule dlm $ Vis.visualize (Vis.sampleVisualPlugin dlm) m) Nothing ast'
                                  ; writeOutLlvm ast'' outh
                                  ; hClose inh
                                  ; closeFileOrStdout ox outh
                                  }
            SrcInfoDbg ix ox -> do { inh <- openFile ix ReadMode
                                   ; outh <- openFileOrStdout ox
                                   ; ast <- testParser ix inh
                                   ; let ast' = A.simplify ast
                                   ; let (srcInfo2, sl2) = mapModule 
                                                           (\(I.SpecializedModule _ ir@(I.Module ls)) -> -- (m, ir@(I.Module ls)::I.Module I.NOOP) = testAst2Ir ast'
                                                             let irCxt = irCxtOfModule ir
                                                                 srcInfo1 = srcInfoMap (unamedMetadata $ globalCxt irCxt)
                                                                 sl = foldl (\p s -> case s of
                                                                                I.ToplevelDefine tld -> 
                                                                                  (localIdSrcInfoMap (unamedMetadata $ globalCxt irCxt) (dbgDeclares $ funCxtOfTlDefine tld)):p
                                                                                _ -> p
                                                                            ) [] ls
                                                             in (srcInfo1, sl)) Nothing ast'
                                   ; writeOutIr srcInfo2 outh
                                   ; writeOutIr (reverse sl2) outh
                                   ; hClose inh
                                   ; closeFileOrStdout ox outh
                                   }
            PhiFixUp ix ox f -> do { inh <- openFile ix ReadMode
                                   ; outh <- openFileOrStdout ox
                                   ; ast <- testParser ix inh
                                   ; let ast1 = A.simplify ast
                                   {-; let (m, ir) = testAst2Ir ast1-}
                                   {-; let ir1 = H.runSimpleUniqueMonad $ H.runWithFuel f 
                                               ((O.optModule1 () N.fixUpPhi ir):: H.SimpleFuelMonad (I.Module ())) -}
                                   ; let ast2 = ast1 -- undefined -- transformModule (\ir -> H.runSimpleUniqueMonad $ H.runWithFuel f (O.optModule1 () N.fixUpPhi ir)) Nothing ast1 --testIr2Ast m ir1
                                   ; writeOutLlvm ast2 outh
                                   ; hClose inh
                                   ; closeFileOrStdout ox outh
                                   }
            DataUse ix ox f -> do { inh <- openFile ix ReadMode
                                  ; outh <- openFileOrStdout ox
                                  ; ast <- testParser ix inh
                                  ; let ast' = A.simplify ast
                                  ; let liv = mapModule (\(I.SpecializedModule _ ir) -> 
                                                          let ic = irCxtOfModule ir
                                                          in 
                                                           H.runSimpleUniqueMonad $ H.runWithFuel f
                                                           ((Du.scanModule ir ic) :: H.SimpleFuelMonad (M.Map (I.FunctionInterface ()) (Du.DataUsage ())))
                                                        ) Nothing ast'
                                  ; writeOutIr liv outh
                                  ; hClose inh
                                  ; closeFileOrStdout ox outh
                                  }
            Pass ix ox passes f -> do { inh <- openFile ix ReadMode
                                      ; outh <- openFileOrStdout ox
                                      ; ast <- testParser ix inh
                                      ; let ast1 = A.simplify ast
                                            {-
                                      ; let (m, ir) = testAst2Ir ast1
                                      ; let applySteps' = applySteps (extractSteps passes) ir
                                      ; let ir1 = H.runSimpleUniqueMonad $ H.runWithFuel f 
                                                  (applySteps' :: H.SimpleFuelMonad (I.Module ()))
                                      ; let ir2 = H.runSimpleUniqueMonad $ H.runWithFuel f 
                                                  ((O.optModule1 () N.fixUpPhi ir1) :: H.SimpleFuelMonad (I.Module ())) -}
                                      ; let ast2 = ast1 -- undefined -- testIr2Ast m ir2
                                      ; writeOutLlvm ast2 outh
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
closeFileOrStdout (Just _) h = hClose h
