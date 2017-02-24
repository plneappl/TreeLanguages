{-# LANGUAGE FlexibleInstances #-}
module Alphabet where

import Lib

class Alphabet a where
  allLetters :: [a]


instance Alphabet a => Alphabet (a, a) where
  allLetters = pairs allLetters allLetters
