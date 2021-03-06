module Ex1 where

import Prelude hiding (map)
import RoseTree
import Pretty
import Alphabet
import States
import qualified DeterministicAutomaton as DA
import qualified NonDeterministicAutomaton as NA
import Automaton
import Data.Set hiding (take)
import qualified Data.List as DL
import ForestAlgebra
import GHC.IO.Encoding
import EQClass

{-|
  This example demonstrates basic functionality and many conversions.
  The automatons should accept a tree @t@ ⇔ @A ∈ t@.
  Conversions demonstrated:
  - Determinization
  - DTA → Forest Algebra
  - Forest Algebra → DTA
  - Minimization
|-}

data Alph = A | B | F deriving (Show, Eq, Ord)
instance Alphabet Alph where
  allLetters = fromList [A, B, F]


data Sts = Y | N | E deriving (Show, Eq, Ord, Enum, Bounded)

_States_Sts :: States Sts
_States_Sts = States $ fromList [Y, N, E]

instance HasEmptyState Sts where
  emptyState = E
instance Monoid Sts where
    mempty = E
    Y `mappend` _ = Y
    _ `mappend` Y = Y  
    E `mappend` x = x
    x `mappend` E = x
    N `mappend` _ = N
    _ `mappend` N = N

d1 :: NA.DeltaProto Alph Sts
d1 A mempty = singleton Y
d1 B mempty = singleton N
d1 F Y      = singleton Y
d1 _ _      = singleton N

d2 :: DA.DeltaProto Alph Sts
d2 A _ = Y
d2 B _ = N
d2 F Y = Y
d2 _ _ = N

na :: NA.NonDeterministicAutomaton Sts Alph
na = NA.NA d1 (singleton Y) _States_Sts

da :: DA.DeterministicAutomaton Sts Alph
da = DA.DA d2 (singleton Y) _States_Sts


ex1_1, ex1_2 :: RT Alph
ex1_1 = Br F [Br F [Lf A, Lf A], Br F [Lf B, Lf B]]

ex1_2 = Br F [Br F [Br F [Lf B, Lf B, Br F [Lf B, Lf B]], Br F [Lf B, Lf B, Br F [Lf B, Lf B]]], Br F [Lf B, Lf B, Br F [Lf B, Lf B]]]

da_fa_m :: (ForestAlgebra Sts (Sts -> Sts), Morph (Forest Alph) (Context Alph) Sts (Sts -> Sts))
da_fa_m = fromDTA da

da_fa :: ForestAlgebra Sts (Sts -> Sts)
da_fa = fst da_fa_m

da_m :: Morph (Forest Alph) (Context Alph) Sts (Sts -> Sts)
da_m = snd da_fa_m

da_toAndFrom :: DA.DeterministicAutomaton Sts Alph
da_toAndFrom = toDTA (DA.states da) da_m da_fa (singleton Y)

da_det = DA.determinize na
da_min1 = DA.minimize da
da_min2 = DA.minimize da_det

main :: IO()
main = do
  setLocaleEncoding utf8
  print "Tree 1:"
  printRT ex1_1
  print "NTA looking for an A:"
  print $ NA.runNonDeterministicAutomaton na ex1_1
  automatonAcceptsIO na ex1_1
  --  print "Accepting stated of determinized NTA:"
  --  print $ DA.acc $ DA.determinize na
  --  print "determinized NTA looking for an A:"
  --  print $ "det. NTA " ++ (if automatonAccepts (DA.determinize na) ex1_1 then "accepted" else
    --  "didn't accept")
  --  print $ DA.runDeterministicAutomaton (DA.determinize na) ex1_1
  print "manual DTA looking for an A:"
  automatonAcceptsIO da ex1_1
  putStrLn ""
  print "Tree 2:"
  printRT ex1_2
  automatonAcceptsIO da ex1_2
  print "determinized NTA looking for an A:"
  --  print $ "det. NTA " ++ (if automatonAccepts (DA.determinize na) ex1_2 then "accepted" else
    --  "didn't accept")
  automatonAcceptsIO na ex1_2
  putStrLn $ DL.intercalate "\n" $ DL.map show $ take 100 $ allAcceptedTrees da
  print ""
  print "Forest Algebra:"
  print (α da_m $ Forest $ (:[]) ex1_1)
  print "Forest Algebra:"
  print (α da_m $ Forest $ (:[]) ex1_2)
  print "and back again:"
  automatonAcceptsIO da_toAndFrom ex1_1
  automatonAcceptsIO da_toAndFrom ex1_2
  print ""
  print da
  print $ da_min1
  automatonAcceptsIO da_min1 ex1_1
  automatonAcceptsIO da_min1 ex1_2
  print da_det
  print $ da_min2
  automatonAcceptsIO da_min2 ex1_1
  automatonAcceptsIO da_min2 ex1_2

  

