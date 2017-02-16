module Ex2 where

import RoseTree
import Alphabet
import States
import DeterministicAutomaton
import Automaton
import qualified Data.Set as DS
import Pretty
import ForestAlgebra
import GHC.IO.Encoding

data Alph = A | B | F | Eps deriving (Show, Eq, Ord)
instance Alphabet Alph where
  allLetters = [A, B, F, Eps]

t1, ex2_1, ex2_2, ex2_3 :: RT Alph
t1 = Br F [Lf A, Lf Eps, Lf B]
ex2_1 = Br F [Lf A, Br F [Lf A, Br F [Lf A, Br F [Lf A, Lf Eps, Lf B], Lf B], Lf B], Lf B]
ex2_2 = Br F [Lf A, Br F [Lf A, Br F [Lf A, Br F [Lf B, Lf Eps, Lf B], Lf B], Lf B], Lf B]
ex2_3 = Br F [Lf A, Lf A, Lf A, Lf A, Lf B, Lf B, Lf B, Lf B]

--  data Sts = SA | SB | Y | N deriving (Show, Eq, Ord)
--  instance States Sts where
  --  allStates = DS.fromList [SA, SB, Y, N]

data Sts = N | Y | SA | SB | SY | Ntr
    deriving (Show,Eq,Ord,Enum)

_States_Sts :: States Sts
_States_Sts = States $ DS.fromList [N .. Ntr]

instance Monoid Sts where
    mempty = Ntr
    Ntr `mappend` x = x
    x `mappend` Ntr = x
    N `mappend`_    = N
    _ `mappend`N    = N
    SA `mappend` SB = SY
    SA `mappend` SY = SY
    SY `mappend` SB = SY
    SA `mappend` _  = SA
    _ `mappend` SB  = SB
    _ `mappend` _   = N

d :: DeltaProto Alph Sts
--  d _ xs | N `elem` xs = N
--  d F [SA, _, SB] = Y
--  d Eps [] = Y
--  d A [] = SA
--  d B [] = SB
--  d _ _ = N
d A Ntr = SA
d B Ntr = SB
d Eps Ntr = Y
d F SY = Y
d _ _ = N

da :: DeterministicAutomaton Sts Alph
da = DA d (DS.singleton Y) _States_Sts

da_fa = fromDTA da
da_back = toDTA _States_Sts (snd da_fa) (fst da_fa) (DS.singleton Y)

da_min = minimize da

main :: IO()
main = do
  setLocaleEncoding utf8
  print "Tree 1:"
  printRT ex2_1
  automatonAcceptsIO da ex2_1
  print "Tree 2:"
  printRT ex2_2
  automatonAcceptsIO da ex2_2
  print "Tree 3:"
  printRT ex2_3
  automatonAcceptsIO da ex2_3

  automatonAcceptsIO da_back ex2_1
  automatonAcceptsIO da_back ex2_2
  automatonAcceptsIO da_back ex2_3

  print $ reachable da
  print da_min
  automatonAcceptsIO da_min ex2_1
  automatonAcceptsIO da_min ex2_2
  automatonAcceptsIO da_min ex2_3
