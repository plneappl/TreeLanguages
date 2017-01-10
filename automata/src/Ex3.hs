{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Ex3 where

import RoseTree
import Alphabet
import States
import qualified DeterministicAutomaton as DA
import qualified NonDeterministicAutomaton as NA
import Automaton
import qualified Data.Set as DS
import Pretty

data Alph = A | B | E deriving (Show, Eq, Ord)

instance Alphabet Alph where
  allLetters = [A, B, E]

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
d A xs = DS.fromList $ map (\ (_, x) -> (A, x)) xs
d _ _ = DS.singleton (E, E)


na :: NA.NonDeterministicAutomaton Sts Alph
na = NA.NA d (DS.fromList [(A, A), (B, B)])

ex1, ex2 :: RT Alph
ex1 = Br A [Br B [Br B [Lf B]], Br A [Br A [Lf A]]]
ex2 = Br A [Br B [Br B [Lf B]], Br A [Br A [Lf B]]]

main :: IO()
main = do
  print "Tree 1:"
  printRT ex1
  automatonAcceptsIO (DA.determinize na) ex1
  --print (DA.acc $ DA.determinize na)
  print "Tree 2:"
  printRT ex2
  automatonAcceptsIO (DA.determinize na) ex2







