module Ex1 where

import Prelude hiding (map)
import RoseTree
import Alphabet
import States
import qualified DeterministicAutomaton as DA
import qualified NonDeterministicAutomaton as NA
import Automaton
import Data.Set

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

ex1_1, ex1_2 :: RT Alph
ex1_1 = Br F [Br F [Lf A, Lf A], Br F [Lf B, Lf B]]

ex1_2 = Br F [Br F [Br F [Lf B, Lf B, Br F [Lf B, Lf B]], Br F [Lf B, Lf B, Br F [Lf B, Lf B]]], Br F [Lf B, Lf B, Br F [Lf B, Lf B]]]

main :: IO()
main = do
  print "Tree 1:"
  print ex1_1
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
  print ex1_2
  automatonAcceptsIO da ex1_2
  automatonAcceptsIO na ex1_2
  