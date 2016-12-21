{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module NonDeterministicAutomaton where
import Alphabet
import States
import Lib
import Automaton
import qualified Data.Set as DS
import Data.Tree

data NonDeterministicAutomaton s a where
  NA :: (Alphabet a, States s, HasEmptyState s) => {
    delta :: DeltaProto a s,
    acc :: DS.Set s
  } -> NonDeterministicAutomaton s a

type DeltaProto a s = a -> [s] -> DS.Set s

instance (States s, Ord s, HasEmptyState s) => Automaton (NonDeterministicAutomaton s) where
  automatonAccepts na rt = (runNonDeterministicAutomaton na rt `DS.intersection` acc na) /= DS.empty
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "NTA accepted" else "NTA didn't accept"

runNonDeterministicAutomaton :: (Ord s, States s, HasEmptyState s) => NonDeterministicAutomaton s a -> Tree a -> DS.Set s
--  runNonDeterministicAutomaton na (Lf a) = (delta na) a []
runNonDeterministicAutomaton na (Node a rs) = let
  substatesSets = map (runNonDeterministicAutomaton na) rs
  substatesLists = chooseAll emptyState substatesSets in
  foldMap ((delta na) a) substatesLists





