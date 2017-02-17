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
  allLetters = "()[]"
instance StackAlph Alph where
  emptyStack = '#'

data Sts = First | Second | Fail deriving (Eq, Show, Ord)

-- accepts   (ⅅ₁∖{ϵ}) . ⅅ₂  
vpa :: VPA Sts Alph Alph
vpa = NVPA {
  states       = States $ fromList [First, Second, Fail],
  startStates  = singleton First,
  acc          = singleton Second,
  call         = fromList "([",
  ret          = fromList ")]",
  intern       = undefined,
  deltaCallN   = deltaC,
  deltaRetN    = deltaR,
  deltaInternN = undefined
}

deltaC :: Alph -> Sts -> Set (Sts, Alph)
deltaC '(' First = singleton (First,  ')')
deltaC '[' First = singleton (Fail, ']')
deltaC '(' Second = singleton (Second,  ')')
deltaC '[' Second = singleton (Second,  ']')
deltaC x Fail = singleton (Fail, x)
deltaR :: Alph -> Sts -> Alph -> Set Sts
deltaR ')' First ')' = fromList [First, Second]
deltaR ')' Second ')' = singleton Second
deltaR ']' First ']' = fromList [First, Second]
deltaR ']' Second ']' = singleton Second
deltaR _ _ _ = singleton Fail

main :: IO ()
main = do
  print $ runVPA vpa "()()"
  print $ runVPA vpa "(())([])"
  print $ runVPA vpa "([])()())"
  print $ runVPA vpa "()()()"
  print $ runVPA vpa "()"