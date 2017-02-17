{-# LANGUAGE TypeSynonymInstances #-}
module ExVPA where

import Alphabet
import VisiblyPushdownAutomaton
import Data.Set
import qualified Prelude as P
import States
import Prelude hiding (map, filter, Word)

type Alph = Char
instance Alphabet Alph where
  allLetters = ['(', ')']
instance StackAlph Alph where
  emptyStack = '#'

data Sts = Good | Fail deriving (Eq, Ord, Show)
sts = States $ fromList [Good, Fail]

-- accepts ⅅ₁
vpa :: VPA Sts Alph Alph
vpa = DVPA {
  states = sts,
  startState = Good,
  acc = singleton Good,
  call = singleton '(',
  ret = singleton ')',
  intern = empty,
  deltaCallD   = deltaC,
  deltaRetD    = deltaR,
  deltaInternD = undefined
} 

deltaC :: Alph -> Sts -> (Sts, Alph)
deltaC a x = (x, a)

deltaR :: Alph -> Sts -> Alph -> Sts
deltaR _ Good '(' = Good
deltaR _ _ _ = Fail



main :: IO ()
main = do
  print $ runVPA vpa "()()()"
  print $ runVPA vpa "(())"
  print $ runVPA vpa "()()())"