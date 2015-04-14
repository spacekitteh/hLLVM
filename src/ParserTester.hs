{-# LANGUAGE DeriveDataTypeable #-}
module ParserTester (testParser, writeOutLlvm, writeOutIr, writeOutShow) where
import System.IO
import Llvm.Syntax.Parser.Basic
import Llvm.Syntax.Parser.Module
import Llvm.Data.Ast
import Llvm.Syntax.Printer.LlvmPrint
import Llvm.Syntax.Printer.IrPrint
import Data.List
import Llvm.Data.Conversion

displayError f s = 
  let sLine = sourceLine $ errorPos s
  in do 
    { hPutStrLn stderr (hackFile sLine (sourceColumn $ errorPos s) f) 
    ; hPutStrLn stderr (show (sourceColumn $ errorPos s))
    ; hPutStrLn stderr (show s)
    ; error ""
    }
  where 
    hackFile :: Int -> Int -> [Char] -> [Char] 
    hackFile sLine sCol l = let (_,ls) = mapAccumL f (0,0) l
                            in ls
      where
        printx lx c x fx fc = if (lx < sLine) then ((fx lx,c), x)
                              else if (lx == sLine) then
                                     if (c < sCol) 
                                     then ((lx, c+1), '^')
                                     else if (c == sCol) 
                                          then ((lx, c+1), '\n')
                                          else ((lx, c), x) 
                                   else 
                                     ((lx,c), fc x) 
        f :: (Int,Int)-> Char -> ((Int,Int), Char)
        f (lx,c) '\n' = printx lx c '\n' (+1) id
        f (lx,c) x = printx lx c x id (\_ -> '_')
   

testParser :: FilePath -> Handle -> IO Module
testParser fileName inh = do { inpStr <- hGetContents inh
                             ; case runParser (complete pModule) initState fileName inpStr of
                               Left s -> displayError inpStr s
                               Right e -> return e
                             }
                                       
                          
writeOutLlvm :: AsmPrint a => a -> Handle -> IO ()
writeOutLlvm m outh = hPutStr outh (render $ toLlvm m)

writeOutShow :: Show a => a -> Handle -> IO ()
writeOutShow m outh = hPutStr outh (show m)

writeOutIr :: IrPrint a => a -> Handle -> IO ()
writeOutIr m outh = hPutStr outh (render $ printIr m)
