{-# LANGUAGE GADTs, ViewPatterns, DuplicateRecordFields, OverloadedLabels #-}

module VisiblyPushdownAutomaton where
import WordAutomaton
import qualified Prelude as P
import Prelude hiding (map, filter, Word, null)
import Alphabet
import States
import Data.Set

class Alphabet a => StackAlph a where
  emptyStack :: a

data DVPA s g a where
  DVPA :: (Ord a, Ord s, Alphabet a, StackAlph g) => {
    statesD      :: States s,
    startStateD  :: s,
    accD         :: Set s,
    callD        :: Set a,
    retD         :: Set a,
    internD      :: Set a,
    deltaCallD   :: a -> s -> (s, g),
    deltaRetD    :: a -> s -> g -> s,
    deltaInternD :: a -> s -> s
  } -> DVPA s g a

data NVPA s g a where
  NVPA :: (Ord a, Ord s, Alphabet a, StackAlph g) => {
    statesN      :: States s,
    startStatesN :: Set s,
    accN         :: Set s,
    callN        :: Set a,
    retN         :: Set a,
    internN      :: Set a,
    deltaCallN   :: a -> s -> Set (s, g),
    deltaRetN    :: a -> s -> g -> Set s,
    deltaInternN :: a -> s -> Set s
  } -> NVPA s g a


runDVPA :: DVPA s g a -> Word a -> s
runDVPA aut@(DVPA {}) w = runWithStack [] w (startStateD aut) where
  runWithStack stack (a:as) state  
    | a `member` callD aut   = let (state', sl) = deltaCallD aut a state in runWithStack (sl:stack) as state'
    | a `member` retD aut    = case stack of 
      []     -> runWithStack [] as (deltaRetD aut a state emptyStack) 
      (s:ss) -> runWithStack ss as (deltaRetD aut a state s         ) 
    | a `member` internD aut = runWithStack stack as (deltaInternD aut a state) 
  runWithStack _ [] s = s

runNVPA :: NVPA s g a -> Word a -> Set s
runNVPA aut@(NVPA {}) w = unions' $ map (runWithStack [] w) $ startStatesN aut where
  unions' = unions . toList
  runWithStack stack (a:as) state
    | a `member` callN aut   = let pushes = deltaCallN aut a state in unions' $ map (\(state', sl) -> runWithStack (sl:stack) as state') pushes
    | a `member` retN aut    = let 
      (stack', sl) = case stack of 
        []     -> ([], emptyStack)
        (s:ss) -> (ss, s) 
      pops = deltaRetN aut a state sl in
      unions' $ map (runWithStack stack' as) pops
    | a `member` internN aut = unions' $ map (runWithStack stack as) (deltaInternN aut a state) 
  runWithStack _ [] s = singleton s

instance (Ord s) => WordAutomaton (DVPA s g) where
  automatonAccepts vpa word = runDVPA vpa word `member` accD vpa
instance (Ord s) => WordAutomaton (NVPA s g) where
  automatonAccepts vpa word = not $ null $ runNVPA vpa word `intersection` accN vpa
