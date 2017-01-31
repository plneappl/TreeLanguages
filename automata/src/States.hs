{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

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

newtype NonDetSimulation s = NonDetSimulation { ndStates :: DS.Set s }
    deriving (Eq,Ord,Foldable)

instance (Show s) => Show (NonDetSimulation s) where
    show = show . ndStates

instance (States s, Ord s) => States (NonDetSimulation s) where
  allStates = DS.map NonDetSimulation $ powerset allStates

instance (States s, Ord s) => HasEmptyState (NonDetSimulation s) where
  emptyState = NonDetSimulation DS.empty

instance (Monoid m, Ord m) => Monoid (NonDetSimulation m) where
    mempty = NonDetSimulation $ DS.singleton mempty
    ls `mappend` rs = NonDetSimulation $
                        foldMap (\l -> DS.map (l `mappend`) $ ndStates rs) $ ndStates ls



--  data Foo = A | B | C | D | Z | O
    --  deriving (Show,Eq,Ord)

--  instance Monoid Foo where
    --  mempty = O
    --  Z `mappend` _ = Z
    --  x `mappend` O = x
    --  _ `mappend` x = x
