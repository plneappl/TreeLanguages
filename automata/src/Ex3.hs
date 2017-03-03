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
import GHC.IO.Encoding
import EQClass
import Lib

data Alph = A | B deriving (Show, Eq, Ord)

instance Alphabet Alph where
  allLetters = [A, B]
 
data Sts = GA | GB | G | U | F deriving (Eq, Show, Ord)
instance Monoid Sts where
  mempty = U
  mappend U x = x
  mappend x U = x
  mappend GA GB = G
  mappend GB GA = G
  mappend G _ = G
  mappend _ G = G
  mappend GA _ = GA
  mappend _ GA = GA
  mappend GB _ = GB
  mappend _ GB = GB
  mappend F x = x

_states_sts = States $ DS.fromList [G, GA, GB, F, U]

instance HasEmptyState Sts where
  emptyState = G

d :: NA.DeltaProto Alph Sts
d A U  = DS.singleton GA
d B U  = DS.singleton GB
d A G  = DS.fromList [GA, GB]
d B G  = DS.fromList [GB, GB]
d A GA = DS.singleton GA
d B GB = DS.singleton GB
d _ _  = DS.singleton F


na :: NA.NonDeterministicAutomaton Sts Alph
na = NA.NA d (DS.fromList [GA, GB, G]) _states_sts

ex1, ex2 :: RT Alph
ex1 = Br A [Br B [Br B [Lf B]], Br A [Br A [Lf A]]]
ex2 = Br A [Br B [Br B [Lf B]], Br A [Br A [Lf B]]]

d1 = DA.minimize $ DA.determinize na

rel = relation $ DS.elemAt 0 $ allStates $ DA.states d1
as  = DS.map fromWitness $ DA.reachable $ DA.determinize na
 

main :: IO()
main = do
  setLocaleEncoding utf8
  print (DA.determinize na)
  print $ assertReflexive as rel
  print $ assertTransitive as rel
  print $ assertSymetric as rel
  print (DA.minimize $ DA.determinize na)
  print "Tree 1:"
  printRT ex1
  automatonAcceptsIO d1 ex1
  --print (DA.acc $ DA.determinize na)
  print "Tree 2:"
  printRT ex2
  automatonAcceptsIO d1 ex2







