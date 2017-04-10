module Lib (powerset, chooseAll, pairs, pairsWith, pairs', pairsWith', expect, fixIOWin, (⇒), (...)) where

import Prelude hiding (map)
import qualified Prelude as P
import Data.Set
import Data.List (nub)
import GHC.IO.Encoding

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

pairsWith' :: (Ord c) => (a -> b -> c) -> Set a -> Set b -> Set c
pairsWith' f as bs = fromList $ pairsWith f (toList as) (toList bs)

-- works with infinite lists
pairs :: [a] -> [b] -> [(a, b)]
pairs = pairsWith (,)

pairs' :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
pairs' = pairsWith' (,)

expectWidth :: Int
expectWidth = 20
expect :: Show a => a -> a -> String
expect x y = fixWidth expectWidth (show x) ++ " ⇔ " ++ show y where
  fixWidth :: Int -> String -> String
  fixWidth t s = replicate (t - length s) ' ' ++ s

fixIOWin :: IO ()
fixIOWin = setLocaleEncoding utf8

(⇒) :: Bool -> Bool -> Bool
False ⇒ _ = True
_ ⇒ True  = True
_ ⇒ _     = False

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)