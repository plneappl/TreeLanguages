{-# LANGUAGE FlexibleInstances #-}
module Alphabet where

class Alphabet a where
  allLetters :: [a]

