module Test where

import RoseTree
import ForestAlgebra
import States
import Alphabet
import Automaton
import DeterministicAutomaton
import qualified Data.Set as DS

data Z2 = Zero | One deriving (Show, Ord, Eq, Enum, Bounded)
instance Monoid Z2 where
  mempty = Zero
  mappend Zero a = a
  mappend a Zero = a
  mappend _ _ = Zero

data Alph = A | B deriving (Show, Eq, Ord)
instance Alphabet Alph where
  allLetters = DS.fromList [A, B]

_States_Z2 :: States Z2 
_States_Z2 = States $ DS.fromList [Zero, One]

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

da0 = toDTA _States_Z2 m fa (DS.fromList [Zero])
da1 = toDTA _States_Z2 m fa (DS.fromList [One])

main :: IO ()
main = do
  print (α m zero)
  print (α m one)
  print (α m two)
  print (α m e1)
  print (α m e2)

  automatonAcceptsIO da0 $ Br A [Br B [Lf A, Lf B], Lf B]
  automatonAcceptsIO da1 $ Br A [Br B [Lf A, Lf B], Lf B]
  print ""