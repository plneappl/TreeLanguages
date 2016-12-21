module Ex2 where

import Alphabet
import States
import DeterministicAutomaton
import Automaton
import qualified Data.Set as DS
import Pretty
import Data.Tree

data Alph = A | B | F | Eps deriving (Show, Eq)
instance Alphabet Alph

data Sts = SA | SB | Y | N deriving (Show, Eq, Ord)
instance States Sts where
  allStates = DS.fromList [SA, SB, Y, N]

ex2_1, ex2_2, ex2_3 :: Tree Alph
ex2_1 = Node F [Node A [], Node F [Node A [], Node F [Node A [], Node F [Node A [], Node Eps [], Node B []], Node B []], Node B []], Node B []]
ex2_2 = Node F [Node A [], Node F [Node A [], Node F [Node A [], Node F [Node B [], Node Eps [], Node B []], Node B []], Node B []], Node B []]
ex2_3 = Node F [Node A [], Node A [], Node A [], Node A [], Node B [], Node B [], Node B [], Node B []]


d :: DeltaProto Alph Sts
d _ xs | N `elem` xs = N
d F [SA, _, SB] = Y
d Eps [] = Y
d A [] = SA
d B [] = SB
d _ _ = N

da :: DeterministicAutomaton Sts Alph
da = DA d (DS.singleton Y)

main :: IO()
main = do
  print "Tree 1:"
  printTree ex2_1
  automatonAcceptsIO da ex2_1
  print "Tree 2:"
  printTree ex2_2
  automatonAcceptsIO da ex2_2
  print "Tree 3:"
  printTree ex2_3
  automatonAcceptsIO da ex2_3
