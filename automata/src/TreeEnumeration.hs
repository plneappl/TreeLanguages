module TreeEnumeration where

import Alphabet
import Data.List
import RoseTree
  
treesOfSizeAtMost :: (Alphabet a, Enum a, Bounded a) => Int -> [RT a]
treesOfSizeAtMost 0 = []
treesOfSizeAtMost k = flatTrees k ++ concatMap (\l -> map (Br l) $ subsequences $ treesOfSizeAtMost (k - 1)) [minBound..]

flatTrees :: (Alphabet a, Enum a, Bounded a) => Int -> [RT a]
flatTrees k = concatMap (\l -> map (Br l) $ takeAnyAndLess (map Lf [minBound..]) k) [minBound..]


takeAnyAndLess :: [a] -> Int -> [[a]]
takeAnyAndLess xs k = concatMap (takeAny xs) [1..k]

takeAny :: [a] -> Int -> [[a]]
takeAny _ 0 = [[]]
takeAny xs 1 = map (: []) xs
takeAny xs k = concatMap (\ys -> map (: ys) xs) $ takeAny xs (k - 1)

data Alph = A | B | F deriving (Show, Eq, Ord, Enum, Bounded)
instance Alphabet Alph

main :: IO ()
main = do
  putStrLn $ intercalate "\n" $ take 1000 $ map show (treesOfSizeAtMost 2 :: [RT Alph])
  --print $ (flatTrees 3 :: [RT Alph])
