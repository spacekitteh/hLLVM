{-# LANGUAGE DeriveDataTypeable #-}
module ParserTester (testParser, writeOut) where
import System.IO
import Llvm.AsmParser.Basic
import Llvm.AsmParser.Module
import Llvm.VmCore.Ast
import Llvm.VmCore.AsmWriter
import Data.List
import Llvm.VmCore.Ast2Ir
import Llvm.VmCore.Ir2Ast
import Llvm.VmCore.IrWriter

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
                                       
                          
writeOut :: AsmWriter a => a -> Handle -> IO ()
writeOut m outh = hPutStr outh (toLlvm m)
