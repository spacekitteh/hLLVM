{-# LANGUAGE CPP, TemplateHaskell #-}

module Llvm.AsmHirConversion.LabelMapM
       (IdLabelMap,labelFor, getLabel, LabelMapM(..),runLabelMapM,emptyIdLabelMap
       ,a2h,invertMap)
       where
import qualified Compiler.Hoopl as H
import qualified Data.Map as M
import qualified Llvm.Asm.Data as A (LabelId(..), BlockLabel(..))
import qualified Llvm.Hir.Data as I
import Control.Applicative
import Control.Monad (ap, liftM)
#ifdef DEBUG
import Debug.Trace
#endif

#define FLC  (I.FileLoc $(I.srcLoc))

{-
-- LabelMapM monad is a CheckingFuelMonad with a data structure IdLabelMap to track
-- the mapping between LLVM labels and Hoopl labels and the original order LLVM labels
-- the mapping will be used to convert Hoopl labels back to LLVM labels to make 
-- llvm-as happy
-}

data IdLabelMap = IdLabelMap { a2h :: M.Map (I.Gname, A.LabelId) H.Label } deriving (Show)

data LabelMapM m a = LabelMapM { unIlM :: IdLabelMap -> m (IdLabelMap, a) }

instance Functor m => Functor (LabelMapM m) where 
  fmap f mla = LabelMapM $ \iLm -> let ma = unIlM mla iLm 
                                       fx = \(im, a) -> (im, f a) 
                                   in fmap fx ma 
  
instance (Functor m, Applicative m, Monad m, H.UniqueMonad m) => Applicative (LabelMapM m) where  
  pure = return
  (<*>) = ap 
  
-- | we need to get a fresh Hoopl label for each LLVM label, so we use Hoopl Unique Monad
instance (Applicative m, H.UniqueMonad m) => Monad (LabelMapM m) where
  return x = LabelMapM $ \iLm -> return (iLm, x)
  iLmM >>= k = LabelMapM $ \iLm -> unIlM iLmM iLm >>= \(iLm1, x) -> unIlM (k x) iLm1


labelFor :: H.UniqueMonad m => (I.Gname, A.LabelId) -> LabelMapM m H.Label
labelFor al = LabelMapM $ \iLm -> case M.lookup al (a2h iLm) of
                                    Just hl -> return (iLm, hl)
                                    Nothing -> do { hl <- H.freshLabel
                                                  ; let a2h' = M.insert al hl (a2h iLm)
                                                  ; return (iLm { a2h = a2h'}, hl)
                                                  }

getLabel :: H.UniqueMonad m => (I.Gname, A.LabelId) -> LabelMapM m (Maybe H.Label)
getLabel al = LabelMapM $ \iLm -> case M.lookup al (a2h iLm) of
  Just hl -> return (iLm, Just hl)
  Nothing -> return (iLm, Nothing)


revertMap :: A.LabelId -> A.BlockLabel
revertMap (A.LabelNumber _) = error "irrefutable"
revertMap x = A.ExplicitBlockLabel x

emptyIdLabelMap = IdLabelMap { a2h = M.empty}

runLabelMapM :: H.UniqueMonad m => IdLabelMap -> LabelMapM m a -> m (IdLabelMap, a)
runLabelMapM iLm (LabelMapM f) = f iLm

invertMap :: M.Map (I.Gname, A.LabelId) H.Label -> M.Map (I.Gname, H.Label) A.LabelId
invertMap m = foldl (\p ((g,k),v) -> if M.member (g,v) p 
                                     then error $ "irrefutable error in invertMap, the values are not unique"
                                     else M.insert (g,v) k p
                    ) M.empty (M.toList m)