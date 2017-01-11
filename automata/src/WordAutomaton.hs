module WordAutomaton where

import Prelude hiding (Word)
import Alphabet

type Word a = [a]

class WordAutomaton at where
  automatonAccepts :: Alphabet al => at al -> Word al -> Bool
  automatonAcceptsIO :: Alphabet al => at al -> Word al -> IO ()
  automatonAcceptsIO at al = print $ if automatonAccepts at al then "Automaton accepted" else "Automaton didn't accept"
