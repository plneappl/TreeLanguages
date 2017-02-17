{-# LANGUAGE GADTs, ViewPatterns #-}

module VisiblyPushdownAutomaton where
import WordAutomaton
import qualified Prelude as P
import Prelude hiding (map, filter, Word, null)
import Alphabet
import States
import Data.Set

class Alphabet a => StackAlph a where
  emptyStack :: a

data VPA s g a where
  NVPA :: (Ord a, Ord s, Alphabet a, StackAlph g) => {
    states       :: States s,
    startStates  :: Set s,
    acc          :: Set s,
    call         :: Set a,
    ret          :: Set a,
    intern       :: Set a,
    deltaCallN   :: a -> s -> Set (s, g),
    deltaRetN    :: a -> s -> g -> Set s,
    deltaInternN :: a -> s -> Set s
  } -> VPA s g a
  DVPA :: (Ord a, Alphabet a, StackAlph g) => {
    states       :: States s,
    startState   :: s,
    acc          :: Set s,
    call         :: Set a,
    ret          :: Set a,
    intern       :: Set a,
    deltaCallD   :: a -> s -> (s, g),
    deltaRetD    :: a -> s -> g -> s,
    deltaInternD :: a -> s -> s
  } -> VPA s g a


runVPA :: VPA s g a -> Word a -> Set s
runVPA aut@(DVPA {}) w = singleton $ runWithStack [] w (startState aut) where
  runWithStack stack (a:as) state  
    | a `member` call aut   = let (state', sl) = deltaCallD aut a state in runWithStack (sl:stack) as state'
    | a `member` ret aut    = case stack of 
      []     -> runWithStack [] as (deltaRetD aut a state emptyStack) 
      (s:ss) -> runWithStack ss as (deltaRetD aut a state s         ) 
    | a `member` intern aut = runWithStack stack as (deltaInternD aut a state) 
  runWithStack _ [] s = s
runVPA aut@(NVPA {}) w = unions' $ map (runWithStack [] w) $ startStates aut where
  unions' = unions . toList
  runWithStack stack (a:as) state
    | a `member` call aut   = let pushes = deltaCallN aut a state in unions' $ map (\(state', sl) -> runWithStack (sl:stack) as state') pushes
    | a `member` ret aut    = let 
      (stack', sl) = case stack of 
        []     -> ([], emptyStack)
        (s:ss) -> (ss, s) 
      pops = deltaRetN aut a state sl in
      unions' $ map (runWithStack stack' as) pops
    | a `member` intern aut = unions' $ map (runWithStack stack as) (deltaInternN aut a state) 
  runWithStack _ [] s = singleton s

instance (Ord s) => WordAutomaton (VPA s g) where
  automatonAccepts vpa word = not $ null $ runVPA vpa word `intersection` acc vpa

