module Llvm.Asm.Printer.Common 
       (module Llvm.Asm.Printer.Common
       ,module Text.PrettyPrint
       )
       where

import Text.PrettyPrint
import Data.Monoid (mempty)
import qualified Data.Map as M(Map, foldWithKey) 


angleBrackets :: Doc -> Doc  
angleBrackets x = char '<' <> x <> char '>'

commaSepList :: [Doc] -> Doc
commaSepList l = hsep $ punctuate comma l

commaSepNonEmpty :: [Doc] -> Doc
commaSepNonEmpty l = commaSepList (filter (not . isEmpty) l)

sepMaybe :: (a -> Doc) -> (Doc -> Doc) -> Maybe a -> Doc
sepMaybe f _  (Nothing) = empty
sepMaybe f sep (Just x) = sep $ f x
