module Automaton where

import Alphabet
import RoseTree
import TreeEnumeration
import Data.Tagged

class Automaton at where
  automatonAccepts :: Alphabet al => at al -> RT al -> Bool
  allAcceptedTrees :: Alphabet al => at al -> [RT al]
  allAcceptedTrees at = filter (automatonAccepts at) $ untag allTrees
  automatonAcceptsIO :: Alphabet al => at al -> RT al -> IO ()
  automatonAcceptsIO at al = print $ if automatonAccepts at al then "TA accepted" else "TA didn't accept"
