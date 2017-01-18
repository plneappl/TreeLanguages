module Test where

import RoseTree
import ForestAlgebra

data Z2 = Zero | One deriving Show
instance Monoid Z2 where
  mempty = Zero
  mappend Zero a = a
  mappend a Zero = a
  mappend _ _ = Zero

data Alph = A | B

fa :: ForestAlgebra Z2 Z2
fa = FA {
  act = mappend,
  inₗ = mappend One,
  inᵣ = mappend One
}


m = morphFromFun fa (const One)

zero, one, two, e1, e2 :: Forest Alph
zero = Forest []
one = Forest [Lf A]
two = Forest [Lf A, Lf A]
e1 = Forest $ (:[]) $ Br A [Br B [Lf A, Lf B]]
e2 = Forest $ (:[]) $ Br A [Br B [Lf A, Lf B], Lf B]


main :: IO ()
main = do
  print (α m zero)
  print (α m one)
  print (α m two)
  print (α m e1)
  print (α m e2)