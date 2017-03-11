{-# LANGUAGE FlexibleInstances #-}
module Alphabet where

import Lib

class Alphabet a where
  allLetters :: [a]

instance Alphabet a => Alphabet (Maybe a) where
  allLetters = Nothing : map Just allLetters

instance (Alphabet a, Alphabet s) => Alphabet (a, s) where
  allLetters = pairs allLetters allLetters
