module TreeEnumeration where

import Alphabet
import Data.List
import RoseTree
import Data.Tagged
  
treesOfSizeAtMost :: (Alphabet a, Enum a, Bounded a) => Int -> [RT a]
treesOfSizeAtMost 0 = []
treesOfSizeAtMost k = flatTrees k ++ concatMap (\l -> map (Br l) $ subsequences $ treesOfSizeAtMost (k - 1)) [minBound..]

-- http://stackoverflow.com/questions/7141287/haskell-cartesian-product-of-infinite-lists
diagonal :: [[a]] -> [a]
diagonal = concat . stripe
  where
  stripe [] = []
  stripe ([]:xss) = stripe xss
  stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)

  zipCons [] ys = ys
  zipCons xs [] = map (:[]) xs
  zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys

-- works with infinite lists
pairs :: [a] -> [b] -> [(a, b)]
pairs as bs = diagonal [[(a, b) | a <- as] | b <- bs]

allTrees :: (Alphabet a) => Tagged a [RT a]
allTrees = do
  fti <- flatTreesInf
  let subTrees = subsequences fti
  let symbolsAndSubtrees = pairs allLetters subTrees
  let bigTrees = map (uncurry Br) symbolsAndSubtrees
  return $ concatMap (\(a, b) -> [a,b]) $ zip bigTrees fti 

-- will produce all trees of depth 1
flatTreesInf :: (Alphabet a) => Tagged a [RT a]
flatTreesInf = let
  leavesList = takeAnyInf (map Lf allLetters)
  branchLetters = allLetters
  symbolAndLeaves = pairs branchLetters leavesList in
  Tagged $ map (uncurry Br) symbolAndLeaves

flatTrees :: Alphabet a => Int -> [RT a]
flatTrees k = concatMap (\l -> map (Br l) $ takeAnyAndLess (map Lf allLetters) k) allLetters

-- takeAnyAndLess xs ∞
takeAnyInf :: [a] -> [[a]]
takeAnyInf xs = concatMap (takeAny xs) [1..]

takeAnyAndLess :: [a] -> Int -> [[a]]
takeAnyAndLess xs k = concatMap (takeAny xs) [1..k]

takeAny :: [a] -> Int -> [[a]]
takeAny _ 0 = [[]]
takeAny xs 1 = map (: []) xs
takeAny xs k = concatMap (\ys -> map (: ys) xs) $ takeAny xs (k - 1)

data Alph = A | B | F deriving (Show, Eq, Ord)
instance Alphabet Alph where
  allLetters = [A, B, F]

main :: IO ()
main = do
  --putStrLn $ intercalate "\n" $ take 1000 $ map show (treesOfSizeAtMost 2 :: [RT Alph])
  --putStrLn $ intercalate "\n" $ take 1000 $ map show (untag flatTreesInf :: [RT Alph])
  --putStrLn $ intercalate "\n" $ take 1000 $ map show (takeAnyInf (map Lf [minBound..]) :: [[RT Alph]])
  putStrLn $ intercalate "\n" $ take 1000 $ map show (untag allTrees :: [RT Alph])
  --print $ (flatTrees 3 :: [RT Alph])
