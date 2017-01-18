{-# LANGUAGE GADTs, MultiParamTypeClasses, InstanceSigs #-}

module DeterministicAutomaton where

import Prelude hiding (map, filter)
import qualified Prelude as P
import RoseTree
import Alphabet
import States
import NonDeterministicAutomaton (NonDeterministicAutomaton(NA))
import Automaton
import Lib
import Data.Set

data DeterministicAutomaton s a where
  DA :: (Alphabet a, States s) => {
    delta :: DeltaProto a s,
    acc :: Set s
  } -> DeterministicAutomaton s a

type DeltaProto a s = a -> [s] -> s

runDeterministicAutomaton :: DeterministicAutomaton s a -> RT a -> s
runDeterministicAutomaton da (Br a rs) = delta da a (P.map (runDeterministicAutomaton da) rs)
runDeterministicAutomaton da (Lf a) = delta da a []

instance (States s, Eq s) => Automaton (DeterministicAutomaton s) where
  automatonAccepts da rt = runDeterministicAutomaton da rt `elem` acc da
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "DTA accepted" else "DTA didn't accept"

determinize :: (Eq s, Ord s, States s) => NonDeterministicAutomaton s a -> DeterministicAutomaton (Set s) a
determinize (NA delta acc) = DA delta' acc' where
  acc' = filter (\x -> any (`elem` x) acc) (allStates :: (States s, Ord s) => Set (Set s))
  delta' a [] = delta a []
  delta' a ss = foldMap (delta a) $ chooseAll emptyState ss



--main :: IO ()
--main = print ""
