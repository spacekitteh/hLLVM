module Llvm.VmCore.AsmWriter 
       (module Llvm.VmCore.AsmWriter
       ,module Text.PrettyPrint
       ) where

import Text.PrettyPrint
import Data.Monoid (mempty)
import qualified Data.Map as M(Map, foldWithKey) 

class AsmWriter a where
  toLlvm :: a -> Doc
  
  
angleBrackets :: Doc -> Doc  
angleBrackets x = char '<' <> x <> char '>'

listToDoc :: (a -> Doc) -> [a] -> (Doc -> Doc -> Doc) -> Doc
listToDoc _ [] _ = mempty
listToDoc f ls dcat = Prelude.foldl g mempty ls
  where g a b = if isEmpty a then f b else a `dcat` (f b)
  
mapToDoc :: (k -> Doc) -> (v -> Doc) -> M.Map k v -> (Doc -> Doc -> Doc) -> (Doc -> Doc -> Doc) -> Doc
mapToDoc kToStr vToStr m kvsep sepFun =
  M.foldWithKey (\k -> \a -> \p ->
                  let entry = kToStr k `kvsep` vToStr a
                  in if isEmpty p then entry
                     else p `sepFun` entry
                ) empty m

        
        

listToString :: String -> (a -> String) -> [a] -> String -> String -> String  
listToString before f [] sep after = ""
listToString before f ls sep after = Prelude.foldl g before ls ++ after
                                   where g a b = if a == before
                                                 then before ++ f b
                                                 else a ++ sep ++ f b
  
strListToString :: String -> [String] -> String -> String -> String
strListToString before ls sep after = listToString before id ls sep after
  
{-                                      
listToLlvm :: AsmWriter a => String -> [a] -> String -> String -> String
listToLlvm before ls sep after = listToString before toLlvm ls sep after 
-}                                 

optToLlvm :: AsmWriter a => Maybe a -> Doc
optToLlvm (Nothing) = empty
optToLlvm (Just x) = toLlvm x



sepOptToLlvm :: AsmWriter a => Doc -> Maybe a -> Doc
sepOptToLlvm _  (Nothing) = empty
sepOptToLlvm sep (Just x) = sep <+> toLlvm x


sepOptToLlvmX :: AsmWriter a => (Doc -> Doc) -> Maybe a -> Doc
sepOptToLlvmX _  (Nothing) = empty
sepOptToLlvmX sep (Just x) = sep $ toLlvm x


optSepToLlvm :: AsmWriter a => Maybe a -> Doc -> Doc
optSepToLlvm (Nothing) _ = empty
optSepToLlvm (Just x) sep = toLlvm x <+> sep


{-
commaSep :: Doc -> Doc -> Doc
commaSep a b = a <+> comma <+> b
-}

commaSepList :: [Doc] -> Doc
commaSepList l = hsep $ punctuate comma l