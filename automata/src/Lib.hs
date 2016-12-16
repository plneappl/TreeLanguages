module Lib (powerset, chooseAll) where

import Prelude hiding (map)
import qualified Prelude as P
import Data.Set
import Data.List (nub)

powerset :: Ord a => Set a -> Set (Set a)
powerset s
    | s == empty = singleton empty
    | otherwise = map (insert x) pxs `union` pxs
        where (x, xs) = deleteFindMin s
              pxs = powerset xs

chooseAll :: Eq a => a -> [Set a] -> [[a]]
chooseAll a xss = nub $ chooseAll' (P.map toList xss) where
  chooseAll' [] = []
  chooseAll' [xs] = [[x] | x <- a:xs]
  chooseAll' (ys:yss) = [y:z | y <- a:ys, z <- chooseAll' yss]

