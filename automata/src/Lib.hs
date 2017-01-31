module Lib (powerset, chooseAll, pairs, pairsWith) where

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

-- http://stackoverflow.com/questions/7141287/haskell-cartesian-product-of-infinite-lists
diagonal :: [[a]] -> [a]
diagonal = concat . stripe
  where
  stripe [] = []
  stripe ([]:xss) = stripe xss
  stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)

  zipCons [] ys = ys
  zipCons xs [] = P.map (:[]) xs
  zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys

pairsWith :: (a -> b -> c) -> [a] -> [b] -> [c]
pairsWith f as bs = diagonal [[f a b | a <- as] | b <- bs]

-- works with infinite lists
pairs :: [a] -> [b] -> [(a, b)]
pairs = pairsWith (,)