
module Pretty(printTree) where

import Data.Tree
import Data.Tree.Pretty


printTree :: (Show a) => Tree a -> IO ()
printTree t = putStr $ drawVerticalTree (fmap show t)

