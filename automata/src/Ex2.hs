module Ex2 where

import RoseTree
import Alphabet
import States
import DeterministicAutomaton
import Automaton
import qualified Data.Set as DS

data Alph = A | B | F | Eps deriving (Show, Eq)
instance Alphabet Alph

data Sts = SA | SB | Y | N deriving (Show, Eq, Ord)
instance States Sts where
  allStates = DS.fromList [SA, SB, Y, N]

ex2_1, ex2_2, ex2_3 :: RT Alph
ex2_1 = Br F [Lf A, Br F [Lf A, Br F [Lf A, Br F [Lf A, Lf Eps, Lf B], Lf B], Lf B], Lf B]
ex2_2 = Br F [Lf A, Br F [Lf A, Br F [Lf A, Br F [Lf B, Lf Eps, Lf B], Lf B], Lf B], Lf B]
ex2_3 = Br F [Lf A, Lf A, Lf A, Lf A, Lf B, Lf B, Lf B, Lf B]


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
  print ex2_1
  automatonAcceptsIO da ex2_1
  print "Tree 2:"
  print ex2_2
  automatonAcceptsIO da ex2_2
  print "Tree 3:"
  print ex2_3
  automatonAcceptsIO da ex2_3
