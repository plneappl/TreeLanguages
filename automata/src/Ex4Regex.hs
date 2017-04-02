module Ex4Regex where

import RegExp
import Alphabet
import RoseTree
import DeterministicAutomaton
import Pretty
import Automaton
import qualified Data.Set as DS

type Alph = Char
instance Alphabet Char where
  allLetters = DS.fromList ['a' .. 'z']

r :: RegExp Alph
r = Concat [Star (Singleton 'a'), Star (Singleton 'b')]
t0, t1, t2, t3, t4, t5 :: RT Alph
t1 = Br 'a' [Lf 'a', Lf 'b']
t2 = Br 'a' [Br 'a' [Br 'a' [Lf 'a']]]
t3 = Br 'a' [Br 'a' [Br 'a' [Lf 'a']], Br 'a' [Br 'a' [Br 'a' [Lf 'a']]]]
t4 = Br 'a' [Br 'a' [Br 'a' [Lf 'a']], Br 'b' [Br 'b' [Br 'b' [Lf 'b']]]]
t5 = Br 'b' [Br 'a' [Br 'a' [Lf 'a']], Br 'b' [Br 'b' [Br 'b' [Lf 'b']]]]

t0 = Lf 'a'

autAnd = fromPathRegexAnd r
autOr  = fromPathRegexOr  r

main :: IO ()
main = do
  printRT t1
  automatonAcceptsIO autAnd t1
  automatonAcceptsIO autOr  t1
  printRT t2
  automatonAcceptsIO autAnd t2
  automatonAcceptsIO autOr  t2
  printRT t3
  automatonAcceptsIO autAnd t3
  automatonAcceptsIO autOr  t3
  printRT t4
  automatonAcceptsIO autAnd t4
  automatonAcceptsIO autOr  t4
  printRT t5
  automatonAcceptsIO autAnd t5
  automatonAcceptsIO autOr  t5  
  
