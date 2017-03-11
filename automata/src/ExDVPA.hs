{-# LANGUAGE TypeSynonymInstances #-}
module ExVPA where

import Alphabet
import VisiblyPushdownAutomaton
import Data.Set
import qualified Prelude as P
import States
import Prelude hiding (map, filter, Word)
import DeterministicAutomaton
import Control.Applicative ((<$>))
import RoseTree
import qualified Automaton as A
import qualified WordAutomaton as W

type Alph = Char
instance Alphabet Alph where
  allLetters = "[]()ab"
instance StackAlph Alph where
  emptyStack = '#'

data Sts = Good | Fail deriving (Eq, Ord, Show)
sts = States $ fromList [Good, Fail]

-- accepts ⅅ₂ with some a's, but no b's
vpa :: DVPA Sts Alph Alph
vpa = DVPA {
  statesD      = sts,
  startStateD  = Good,
  accD         = singleton Good,
  callD        = fromList "([",
  retD         = fromList "])",
  internD      = fromList "ab",
  deltaCallD   = deltaC,
  deltaRetD    = deltaR,
  deltaInternD = deltaI
} 

deltaC :: Alph -> Sts -> (Sts, Alph)
deltaC a x = (x, a)

deltaR :: Alph -> Sts -> Alph -> Sts
deltaR ')' Good '(' = Good
deltaR ']' Good '[' = Good
deltaR _ _ _ = Fail

deltaI :: Alph -> Sts -> Sts
deltaI 'a' x = x
deltaI 'b' _ = Fail

fromListTuple :: [a] -> (a, a)
fromListTuple [a, b] = (a, b)
fromListTuple [a] = (a, a)

(dta, trans) = fromDVPA vpa

(vpa', trans') = toDVPA dta

id' = P.map trans' . trans 

main :: IO ()
main = do
  print $ runDVPA vpa "()()()"
  print $ runDVPA vpa "(())"
  print $ runDVPA vpa "()()())"
  print $ runDVPA vpa "()[]()"
  print   dta
  print (trans "[(ab)([a])]")
  print (trans "()()[)") 
  print $ all (A.automatonAccepts dta) (trans "()()[]")
  print $ all (A.automatonAccepts dta) (trans "()()[)")
  print $ all (A.automatonAccepts dta) (trans "[(ab)([a])]")
  print $ all (A.automatonAccepts dta) (trans "[(aa)([a])]")
  print $ all (A.automatonAccepts dta) (trans "[(aa)([a]]]")
  print "--------"
  print (accD vpa')
  print (id' "()()()")
  print $ P.map (W.automatonAccepts vpa') (id' "()()()")
  print (id' "(())")  
  print $ P.map (W.automatonAccepts vpa') (id' "(())")
  -- can't do this, since the word isn't well matched and "transform" only works for well matched words
  -- TODO: think of a way to handle this
  --print (id' "()()())")  
  --print $ P.map (W.automatonAccepts vpa') (id' "()()())")
  print (id' "()[]()")  
  print $ P.map (W.automatonAccepts vpa') (id' "()[]()")

