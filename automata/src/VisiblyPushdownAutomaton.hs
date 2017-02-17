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
  NVPA :: (Ord a, Alphabet a, StackAlph g) => {
    states :: States s,
    startStates :: Set s,
    acc :: Set s,
    call :: Set a,
    ret :: Set a,
    intern :: Set a,
    deltaCallN :: a -> s -> Set (s, g),
    deltaRetN :: a -> s -> g -> Set s,
    deltaInternN :: a -> s -> Set s
  } -> VPA s g a
  DVPA :: (Ord a, Alphabet a, StackAlph g) => {
    states :: States s,
    startState :: s,
    acc :: Set s,
    call :: Set a,
    ret :: Set a,
    intern :: Set a,
    deltaCallD :: a -> s -> (s, g),
    deltaRetD :: a -> s -> g -> s,
    deltaInternD :: a -> s -> s
  } -> VPA s g a


runVPA :: VPA s g a -> Word a -> Set s
runVPA aut@(DVPA {}) w = singleton $ runWithStack [] (startState aut) w where
  runWithStack stack state (a:as) 
    | a `member` call aut   = let (state', sl) = deltaCallD aut a state in runWithStack (sl:stack) state' as
    | a `member` ret aut    = case stack of 
      []     -> runWithStack [] (deltaRetD aut a state emptyStack) as
      (s:ss) -> runWithStack ss (deltaRetD aut a state s         ) as
    | a `member` intern aut = runWithStack stack (deltaInternD aut a state) as
  runWithStack _ s _ = s

instance (Ord s) => WordAutomaton (VPA s g) where
  automatonAccepts vpa word = not $ null $ runVPA vpa word `intersection` acc vpa

