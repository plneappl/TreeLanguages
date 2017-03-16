{-# LANGUAGE FlexibleInstances #-}
module Alphabet where

import Lib
import Prelude hiding (map)
import Data.Set (Set, map, union, singleton, unions, toList)

class Alphabet a where
  allLetters :: Set a

instance (Alphabet a, Ord a) => Alphabet (Maybe a) where
  allLetters = singleton Nothing `union` map Just allLetters

instance (Alphabet a, Alphabet s, Ord a, Ord s) => Alphabet (a, s) where
  allLetters = unions $ toList $ map (\a -> map (\s -> (a, s)) allLetters) allLetters
