{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module States where

import qualified Data.Set as DS
import Lib

data States s = States {
  allStates :: DS.Set s
} deriving (Ord, Eq, Show)

class StatesC s where
  allStatesC :: DS.Set s

instance (Ord s, StatesC s) => StatesC (DS.Set s) where
  allStatesC = powerset allStatesC

class HasEmptyState s where
  emptyState :: s

statesPowerset :: Ord s => States s -> States (DS.Set s)
statesPowerset s = States $ powerset $ allStates s

instance HasEmptyState (DS.Set s) where
  emptyState = DS.empty
  
newtype NonDetSimulation s = NonDetSimulation { ndStates :: DS.Set s }
  deriving (Eq,Ord,Foldable)

instance (Show s) => Show (NonDetSimulation s) where
  show = show . ndStates

statesNonDetSimulation :: Ord s => States s -> States (NonDetSimulation s)
statesNonDetSimulation s = States $ DS.map NonDetSimulation $ powerset $ allStates s

instance Ord s => HasEmptyState (NonDetSimulation s) where
  emptyState = NonDetSimulation DS.empty

instance (Monoid m, Ord m) => Monoid (NonDetSimulation m) where
  mempty = NonDetSimulation $ DS.singleton mempty
  ls `mappend` rs = NonDetSimulation $ foldMap (\l -> DS.map (l `mappend`) $ ndStates rs) $ ndStates ls




