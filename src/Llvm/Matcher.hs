{-# LANGUAGE RankNTypes #-}
module Llvm.Matcher where
import qualified Llvm.Asm.Data as A
import qualified Llvm.Hir.Data as I
import qualified Llvm.AsmHirConversion as Cv
import qualified Compiler.Hoopl as H
import qualified Data.Map as M
import Llvm.AsmHirConversion 
import Llvm.Hir.Target.Linux_Gnu
import Llvm.Hir.DataLayoutMetrics


filterOutDataLayoutAndTriple :: [A.Toplevel] -> (([A.LayoutSpec], A.TargetTriple), [A.Toplevel]) -- (A.DataLayout, A.Triple)
filterOutDataLayoutAndTriple tls = 
  let [A.ToplevelTriple (A.TlTriple triple)] = filter (\x -> case x of
                                                          A.ToplevelTriple a -> True
                                                          _ -> False) tls
      [A.ToplevelDataLayout (A.TlDataLayout (A.DataLayout dl))] = filter (\x -> case x of
                                                                             A.ToplevelDataLayout a -> True
                                                                             _ -> False) tls
  in ((dl, triple), filter (\x -> case x of
                               A.ToplevelTriple _ -> False
                               A.ToplevelDataLayout _ -> False
                               _ -> True) tls)


getDT :: A.Module -> ([A.LayoutSpec], A.TargetTriple)
getDT (A.Module l) = fst $ filterOutDataLayoutAndTriple l

mapModule :: (forall x. DataLayoutMetrics x => I.SpecializedModule x () -> b) -> Maybe dlm -> A.Module -> b
mapModule f Nothing m = let (ls, tt) = getDT m
                        in if isI386_Pc_Linux_Gnu ls tt then
                             let (_, m1) = H.runSimpleUniqueMonad $ Cv.asmToHir I386_Pc_Linux_Gnu m
                             in f m1
                           else
                             error $ "unsupported target"


transformModule :: (forall x. DataLayoutMetrics x => I.SpecializedModule x () -> I.SpecializedModule x ()) -> Maybe dlm -> A.Module -> A.Module 
transformModule f Nothing m = transformModule2 (\x -> (f x, id)) Nothing m
       
transformModule2 :: (forall x. DataLayoutMetrics x => I.SpecializedModule x () -> 
                     (I.SpecializedModule x (), M.Map (A.GlobalId, H.Label) A.LabelId -> M.Map (A.GlobalId, H.Label) A.LabelId)) -> Maybe dlm -> A.Module -> A.Module 
transformModule2 f  Nothing m = 
  let (ls, tt) = getDT m
  in if isI386_Pc_Linux_Gnu ls tt then
       let (idmap, m1) = H.runSimpleUniqueMonad $ Cv.asmToHir I386_Pc_Linux_Gnu m
       in let (m2, labelMap) = f m1
          in Cv.hirToAsm (labelMap $ Cv.invertMap (Cv.a2h idmap)) m2
     else
       error $ "unsupported target"       



{-
transformModuleIO :: (forall x. DataLayoutMetrics x => I.SpecializedModule x () -> IO (I.SpecializedModule x ())) -> Maybe dlm -> A.Module -> IO A.Module 
transformModuleIO f Nothing m = transformModule2IO f id Nothing m
       
transformModule2IO :: (forall x. DataLayoutMetrics x => I.SpecializedModule x () -> IO (I.SpecializedModule x ())) -> 
                    (M.Map (A.GlobalId, H.Label) A.LabelId -> M.Map (A.GlobalId, H.Label) A.LabelId) -> Maybe dlm -> A.Module -> IO A.Module 
transformModule2IO f flabelMap Nothing m = 
  let (ls, tt) = getDT m
  in if isI386_Pc_Linux_Gnu ls tt then
       let (idmap, m1) = H.runSimpleUniqueMonad $ Cv.asmToHir I386_Pc_Linux_Gnu m
       in Cv.hirToAsm (flabelMap $ Cv.invertMap (Cv.a2h idmap)) (f m1)
     else
       error $ "unsupported target"       
-}
