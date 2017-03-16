{-| 
Module      : NonDeterministicAutomaton
Description : Contains the definition of Non-Deterministic Tree Automatons.
-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module NonDeterministicAutomaton where
import Alphabet
import States
import RoseTree
import Lib
import Automaton
import qualified Data.Set as DS
import qualified Data.Foldable as DF

-- |A NTA (Non-Deterministic Tree Automaton) with States @s@ and Alphabet @a@. @s@ has to be a Monoid.
data NonDeterministicAutomaton s a where
  NA :: (Alphabet a, HasEmptyState s, Monoid s) => {
    delta :: DeltaProto a s,
    acc :: DS.Set s,
    states :: States s
  } -> NonDeterministicAutomaton s a

type DeltaProto a s = a -> s -> DS.Set s

instance (Ord s, HasEmptyState s, Monoid s) => Automaton (NonDeterministicAutomaton s) where
  automatonAccepts na rt = (runNonDeterministicAutomaton na rt `DS.intersection` acc na) /= DS.empty
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "NTA accepted" else "NTA didn't accept"

-- |Run a DTA on a Tree.
runNonDeterministicAutomaton :: (Ord s, HasEmptyState s, Monoid s) => NonDeterministicAutomaton s a -> RT a -> DS.Set s
runNonDeterministicAutomaton na (Lf a) = delta na a mempty
runNonDeterministicAutomaton na (Br a rs) = let
  substatesSets = map (NonDetSimulation . runNonDeterministicAutomaton na) rs
  startingStates = ndStates $ DF.fold substatesSets
  in foldMap (delta na a) startingStates
  






