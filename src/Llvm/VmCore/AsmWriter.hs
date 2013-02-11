module Llvm.VmCore.AsmWriter where

class AsmWriter a where
  toLlvm :: a -> String
  
listToString :: String -> (a -> String) -> [a] -> String -> String -> String  
listToString before f [] sep after = ""
listToString before f ls sep after = Prelude.foldl g before ls ++ after
                                   where g a b = if a == before
                                                 then before ++ f b
                                                 else a ++ sep ++ f b
  
strListToString :: String -> [String] -> String -> String -> String
strListToString before ls sep after = listToString before id ls sep after
  
listToLlvm :: AsmWriter a => String -> [a] -> String -> String -> String
listToLlvm before ls sep after = listToString before toLlvm ls sep after 
                                 

optToLlvm :: AsmWriter a => Maybe a -> String
optToLlvm (Nothing) = ""
optToLlvm (Just x) = toLlvm x



sepOptToLlvm :: AsmWriter a => String -> Maybe a -> String
sepOptToLlvm _  (Nothing) = ""
sepOptToLlvm sep (Just x) = sep ++ toLlvm x



optSepToLlvm :: AsmWriter a => Maybe a -> String -> String
optSepToLlvm (Nothing) _ = ""
optSepToLlvm (Just x) sep = toLlvm x ++ sep

