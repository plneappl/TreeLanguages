
module TransMonoid where

import Alphabet
import States
import WordDFA

import qualified Data.Set as DS

data TransMonoid s a =
    TM {aut :: WordDFA s a, dom :: [s], trans :: s -> s }

instance (Show s, Ord s, Alphabet a, States s) => Show (TransMonoid s a) where
   show m = show' m allTs 0
      where
         t = transMonoid $ aut m
         allTs = DS.toList $ elems t
         show' m [] _ = error "illegal transformation monoid element"
         show' m (x:xs) n
            | m == x = show n
            | otherwise = show' m xs (n+1)

instance (States s, Alphabet a) => Monoid (TransMonoid s a) where
    mempty = TM { aut = simpleAut, dom = DS.toList allStates, trans = id }
    mappend l r
        | DS.null (acc (aut l)) = r
        | DS.null (acc (aut r)) = l
        | otherwise =
            TM { aut = aut l, dom = fmap (trans r) (dom l), trans = trans r . trans l }

instance (States s, Eq s, Alphabet a) => Eq (TransMonoid s a) where
   l == r
      | DS.null (acc (aut l)) = DS.null (acc (aut r))
      | DS.null (acc (aut r)) = False
      | otherwise = dom l == dom r

data FullTransMonoid s a = FullTM { zero :: TransMonoid s a, elems :: DS.Set (TransMonoid s a) }
   deriving (Eq)

instance (Show s, Ord s, Alphabet a, States s) => Show (FullTransMonoid s a) where
   show = show . elems

instance (States s, Ord s, Alphabet a) => Ord (TransMonoid s a) where
   l < r       = dom l < dom r
   compare l r = compare (dom l) (dom r)

transMonoid :: (States s, Ord s, Alphabet a) => WordDFA s a -> FullTransMonoid s a
transMonoid da = FullTM { zero = z, elems = e }
   where
      z = TM { aut = da, dom = DS.toList (states da), trans = id }
      ftFromLetter a = TM { aut = da, dom = fmap (delta da a) (dom z), trans = delta da a }
      allSingleTrans = fmap ftFromLetter allLetters
      e = oneStepClosure $ DS.singleton z
      oneStepClosure fs = let doLeftStep fa  = DS.map (fa `mappend`) fs
                              doRightStep fa = DS.map (`mappend` fa) fs
                              leftNew        = DS.unions $ fmap doLeftStep allSingleTrans
                              rightNew       = DS.unions $ fmap doLeftStep allSingleTrans
                              newFs          = DS.union leftNew rightNew
                           in if newFs `DS.isSubsetOf` fs
                              then fs
                              else oneStepClosure $ newFs `DS.union` fs

data Sts = SZ | SO | Tw | Thr | SF
    deriving (Show,Eq,Ord,Enum)
instance States Sts where
    allStates = DS.fromList [SZ .. SF]

data Alph = AZ | AO
    deriving (Show,Eq,Ord,Enum)
instance Alphabet Alph where
    allLetters = [AZ, AO]

tmz :: TransMonoid Sts Alph
tmz = TM { aut = da, dom = img, trans = d AZ }
   where
      da = WordDFA { delta = d, start = SZ, acc = DS.singleton SF, states = allStates }
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
      img = fmap (d AZ) (DS.toList allStates)

tmo :: TransMonoid Sts Alph
tmo = TM { aut = da, dom = img, trans = d AO }
   where
      da = WordDFA { delta = d, start = SZ, acc = DS.singleton SF, states = allStates }
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
      img = fmap (d AO) (DS.toList allStates)

