
module Pretty where

import qualified Data.Tree as DT
import Data.Tree.Pretty
import RoseTree

rtToTree :: RT a -> DT.Tree a
rtToTree = foldRT (flip DT.Node []) DT.Node

showTree :: (Show a) => DT.Tree a -> String
showTree t = drawVerticalTree (fmap show t)

printTree :: (Show a) => DT.Tree a -> IO ()
printTree = putStr . showTree

printDTForest :: (Show a) => DT.Forest a -> IO ()
printDTForest ts = putStr $ drawVerticalForest $ fmap (fmap show) ts

printForest :: (Show a) => Forest a -> IO ()
printForest = printDTForest . fmap rtToTree . trees

showRT :: (Show a) => RT a -> String
showRT = showTree . rtToTree

printRT :: (Show a) => RT a -> IO ()
printRT = putStr . showRT

printContext :: (Show a) => Context a -> IO ()
printContext c = printForest $ insertForest (fmap (PrintInContext . Just) c) (Forest [Lf $ PrintInContext Nothing])

newtype PrintInContext a = PrintInContext (Maybe a)

instance (Show a) => Show (PrintInContext a) where
    show (PrintInContext Nothing)  = "☐"
    show (PrintInContext (Just x)) = show x

