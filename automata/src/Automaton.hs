module Automaton where

import Alphabet
import RoseTree

class Automaton at where
  automatonAccepts :: Alphabet al => at al -> RT al -> Bool
  automatonAcceptsIO :: Alphabet al => at al -> RT al -> IO ()
  automatonAcceptsIO at al = print $ if automatonAccepts at al then "TA accepted" else "TA didn't accept"
