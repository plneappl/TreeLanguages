{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Ex3 where

import Alphabet
import States
import qualified DeterministicAutomaton as DA
import qualified NonDeterministicAutomaton as NA
import Automaton
import qualified Data.Set as DS
import Pretty
import Data.Tree

data Alph = A | B | E deriving (Show, Eq, Ord)

instance Alphabet Alph

type Sts = (Alph, Alph)
instance States Sts where
  allStates = DS.fromList [
    (A, A), (A, B), (A, E), 
    (B, A), (B, B), (B, E),
    (E, A), (E, B), (E, E)]

instance HasEmptyState Sts where
  emptyState = (E, E)

d :: NA.DeltaProto Alph Sts
d A [] = DS.singleton (A, A)
d B [] = DS.singleton (B, B)
d A xs = DS.fromList $ map ((\ (_, x) -> (A, x))) xs
d _ _ = DS.singleton (E, E)


na :: NA.NonDeterministicAutomaton Sts Alph
na = NA.NA d (DS.fromList [(A, A), (B, B)])

ex1, ex2 :: Tree Alph
ex1 = Node A [Node B [Node B [Node B []]], Node A [Node A [Node A []]]]
ex2 = Node A [Node B [Node B [Node B []]], Node A [Node A [Node B []]]]

main :: IO()
main = do
  print "Tree 1:"
  printTree ex1
  automatonAcceptsIO (DA.determinize na) ex1
  --print (DA.acc $ DA.determinize na)
  print "Tree 2:"
  printTree ex2
  automatonAcceptsIO (DA.determinize na) ex2
  






