module RoseTree where

data RT a = Lf a | Br a [RT a] deriving (Show)



