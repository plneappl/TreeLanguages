module States where

import qualified Data.Set as DS
import Lib

class States s where
  allStates :: DS.Set s

class (States s) => HasEmptyState s where
  emptyState :: s

instance (States s, Ord s) => States (DS.Set s) where
  allStates = powerset allStates

instance (States s, Ord s) => HasEmptyState (DS.Set s) where
  emptyState = DS.empty
