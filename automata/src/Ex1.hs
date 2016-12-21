module Ex1 where

import Prelude hiding (map)
import Alphabet
import States
import qualified DeterministicAutomaton as DA
import qualified NonDeterministicAutomaton as NA
import Automaton
import Data.Set
import Pretty
import Data.Tree

data Alph = A | B | F deriving (Show, Eq, Ord)
instance Alphabet Alph


data Sts = Y | N | E deriving (Show, Eq, Ord)

instance States Sts where
  allStates = fromList [Y, N, E]
instance HasEmptyState Sts where
  emptyState = E

d1 :: NA.DeltaProto Alph Sts
d1 A [] = singleton Y
d1 B [] = singleton N
d1 F xs | Y `elem` xs = singleton Y
d1 _ _ = singleton N

d2 :: DA.DeltaProto Alph Sts
d2 A _ = Y
d2 B _ = N
d2 F xs | Y `elem` xs = Y
        | otherwise = N

na :: NA.NonDeterministicAutomaton Sts Alph
na = NA.NA d1 (singleton Y)

da :: DA.DeterministicAutomaton Sts Alph 
da = DA.DA d2 (singleton Y)

ex1_1, ex1_2 :: Tree Alph
ex1_1 = Node F [Node F [Node A [], Node A []], Node F [Node B [], Node B []]]

ex1_2 = Node F [Node F [Node F [Node B [], Node B [], Node F [Node B [], Node B []]], Node F [Node B [], Node B [], Node F [Node B [], Node B []]]], Node F [Node B [], Node B [], Node F [Node B [], Node B []]]]

--  toTree :: RT a -> Tree a
--  toTree (Lf x) = Node x []
--  toTree (Br x children) = Node x $ fmap toTree children

--  printTree :: (Show a) => RT a -> IO ()
--  printTree t = putStr $ drawVerticalTree (fmap show (toTree t))

main :: IO()
main = do
  print "Tree 1:"
  printTree ex1_1
  print "NTA looking for an A:"
  print $ NA.runNonDeterministicAutomaton na ex1_1
  automatonAcceptsIO na ex1_1
  print "Accepting stated of determinized NTA:"
  print $ DA.acc $ DA.determinize na
  print "determinized NTA looking for an A:"
  print $ "det. NTA " ++ (if automatonAccepts (DA.determinize na) ex1_1 then "accepted" else
    "didn't accept")
  print $ DA.runDeterministicAutomaton (DA.determinize na) ex1_1
  print "manual DTA looking for an A:"
  automatonAcceptsIO da ex1_1
  putStrLn ""
  print "Tree 2:"
  printTree ex1_2
  automatonAcceptsIO da ex1_2
  automatonAcceptsIO na ex1_2
  
