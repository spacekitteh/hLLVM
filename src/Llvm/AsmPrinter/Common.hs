module Llvm.AsmPrinter.Common 
       (module Llvm.AsmPrinter.Common
       ,module Text.PrettyPrint
       )
       where

import Text.PrettyPrint
import Data.Monoid (mempty)
import qualified Data.Map as M(Map, foldWithKey) 


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


commaSepList :: [Doc] -> Doc
commaSepList l = hsep $ punctuate comma l


sepMaybe :: (a -> Doc) -> (Doc -> Doc) -> Maybe a -> Doc
sepMaybe f _  (Nothing) = empty
sepMaybe f sep (Just x) = sep $ f x


{-
maybeSep :: (a -> Doc) -> Maybe a -> (Doc -> Doc) -> Doc
maybeSep f (Nothing) _  = empty
maybeSep f (Just x) sep = sep $ f x
-}