{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module TransMonoid where

import Alphabet
import States
import WordDFA

import qualified Data.Set as DS

data TransMonoid s a =
    TM {aut :: WordDFA s a, dom :: [s], trans :: s -> s }
img :: TransMonoid s a -> [s]
img tm = map (trans tm) (dom tm)

instance (Show s, Ord s, Alphabet a, StatesC s) => Show (TransMonoid s a) where
   show m = show' m allTs 0
      where
         t = transMonoid $ aut m
         allTs = DS.toList $ elems $ fst t
         show' m [] _ = error "illegal transformation monoid element"
         show' m (x:xs) n
            | m == x = show n
            | otherwise = show' m xs (n+1)

instance (Alphabet a, StatesC s) => Monoid (TransMonoid s a) where
    mempty = TM { aut = simpleAut $ fromStates $ States allStatesC, dom = DS.toList allStatesC, trans = id }
    mappend l r
        | DS.null (acc (aut l)) = r
        | DS.null (acc (aut r)) = l
        | otherwise =
            TM { aut = aut l, dom = fmap (trans r) (dom l), trans = trans r . trans l }

instance (Eq s, Alphabet a) => Eq (TransMonoid s a) where
   l == r
      | DS.null (acc (aut l)) = DS.null (acc (aut r))
      | DS.null (acc (aut r)) = False
      | otherwise = dom l == dom r

data FullTransMonoid s a = FullTM { zero :: TransMonoid s a, elems :: DS.Set (TransMonoid s a) }
   deriving (Eq)

instance (Show s, Ord s, Alphabet a, StatesC s) => Show (FullTransMonoid s a) where
   show = show . elems

instance (Ord s, Alphabet a) => Ord (TransMonoid s a) where
   l < r       = dom l < dom r
   compare l r = compare (dom l) (dom r)

transMonoid :: (Ord s, Alphabet a, StatesC s) => WordDFA s a -> (FullTransMonoid s a, a -> TransMonoid s a)
transMonoid da = (FullTM { zero = z, elems = e }, ftFromLetter)
   where
      z = TM { aut = da, dom = DS.toList (states da), trans = id }
      ftFromLetter a = TM { aut = da, dom = fmap (delta da a) (dom z), trans = delta da a }
      allSingleTrans = DS.map ftFromLetter allLetters
      e = oneStepClosure $ DS.singleton z
      oneStepClosure fs = let doLeftStep fa  = DS.map (fa `mappend`) fs
                              doRightStep fa = DS.map (`mappend` fa) fs
                              leftNew        = foldMap doLeftStep allSingleTrans
                              rightNew       = foldMap doLeftStep allSingleTrans
                              newFs          = DS.union leftNew rightNew
                           in if newFs `DS.isSubsetOf` fs
                              then fs
                              else oneStepClosure $ newFs `DS.union` fs

transWDFA :: forall s0 s a. (s ~ (TransMonoid s0 a), Ord s0, StatesC s0) => WordDFA s0 a -> WordDFA s a
transWDFA dfa@WordDFA {} = WordDFA { start = start', delta = delta', acc = acc', states=states' } where
  start' = zero tmonoid
  tmonoid :: FullTransMonoid s0 a
  morph :: a -> s
  (tmonoid, morph) = transMonoid dfa
  delta' a s = s `mappend` morph a
  acc' = DS.filter (\tm -> trans tm (start dfa) `elem` acc dfa) states'
  states' = elems tmonoid



data Sts = SZ | SO | Tw | Thr | SF
    deriving (Show,Eq,Ord,Enum)
_States_Sts :: States Sts
_States_Sts = States $ DS.fromList [SZ .. SF]

data Alph = AZ | AO
    deriving (Show,Eq,Ord,Enum)
instance Alphabet Alph where
    allLetters = DS.fromList [AZ, AO]

tmz :: TransMonoid Sts Alph
tmz = TM { aut = da, dom = img, trans = d AZ }
   where
      da = WordDFA { delta = d, start = SZ, acc = DS.singleton SF, states = allStates _States_Sts }
      d AO SZ  = SO
      d AO SO  = Tw
      d AO Tw  = Thr
      d AO Thr = SF
      d AO SF  = SF
      d AZ SZ  = SZ
      d AZ SO  = SO
      d AZ Tw  = Tw
      d AZ Thr = Thr
      d AZ SF  = SZ
      img = fmap (d AZ) (DS.toList $ allStates _States_Sts)

tmo :: TransMonoid Sts Alph
tmo = TM { aut = da, dom = img, trans = d AO }
   where
      da = WordDFA { delta = d, start = SZ, acc = DS.singleton SF, states = allStates _States_Sts }
      d AO SZ  = SO
      d AO SO  = Tw
      d AO Tw  = Thr
      d AO Thr = SF
      d AO SF  = SF
      d AZ SZ  = SZ
      d AZ SO  = SO
      d AZ Tw  = Tw
      d AZ Thr = Thr
      d AZ SF  = SZ
      img = fmap (d AO) (DS.toList $ allStates _States_Sts)

