
module Pretty where

import qualified Data.Tree as DT
import Data.Tree.Pretty
import RoseTree
import Forest

rtToTree :: RT a -> DT.Tree a
rtToTree = foldRT (flip DT.Node []) DT.Node

printTree :: (Show a) => DT.Tree a -> IO ()
printTree t = putStr $ drawVerticalTree (fmap show t)

printDTForest :: (Show a) => DT.Forest a -> IO ()
printDTForest ts = putStr $ drawVerticalForest $ fmap (fmap show) ts

printForest :: (Show a) => Forest a -> IO ()
printForest = printDTForest . fmap rtToTree . trees

printRT :: (Show a) => RT a -> IO ()
printRT = printTree . rtToTree

printContext :: (Show a) => Context a -> IO ()
printContext = printRT . fmap PrintInContext

data PrintInContext a = PrintInContext (Maybe a)

instance (Show a) => Show (PrintInContext a) where
    show (PrintInContext Nothing)  = "☐"
    show (PrintInContext (Just x)) = show x

