{-| 
Module      : Automaton
Description : Contains the definition of the class 'Automaton'.

The Module "Automaton" contains the definition for the class 'Automaton', which is implemented by all models of different Tree Automatons.
-}
module Automaton where

import Alphabet
import RoseTree
import TreeEnumeration
import Data.Tagged

-- |All Tree Automatons implement this class. 
class Automaton at where
  -- A (short) name for this Automaton. Is used in @automatonAcceptsIO@.
  -- ??doesn't work??
  --automatonName :: String
  --automatonName = "TA"
  -- |Ask an Automaton whether it accepts some Tree.
  automatonAccepts :: (Alphabet al) => at al -> RT al -> Bool
  -- |Get the (often infinite) list of all Trees accepted by this Automaton. Every accepted Tree will be produced at some point.
  --The default implementation is to just try every Tree for acceptance.
  allAcceptedTrees :: (Alphabet al) => at al -> [RT al]
  allAcceptedTrees at = filter (automatonAccepts at) $ untag allTrees
  -- |Ask an automaton whether it accepts some Tree and print a message.
  automatonAcceptsIO :: (Alphabet al) => at al -> RT al -> IO ()
  automatonAcceptsIO at al = let name = "TA" in print $ if automatonAccepts at al then name ++ " accepted" else name ++ " didn't accept"
