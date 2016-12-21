module Automaton where

import Alphabet
import Data.Tree

class Automaton at where
  automatonAccepts :: Alphabet al => at al -> Tree al -> Bool
  automatonAcceptsIO :: Alphabet al => at al -> Tree al -> IO ()
  automatonAcceptsIO at al = print $ if automatonAccepts at al then "TA accepted" else "TA didn't accept"
