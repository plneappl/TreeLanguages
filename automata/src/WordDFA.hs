{-# LANGUAGE GADTs #-}

module WordDFA where

import Prelude hiding (Word)
import Alphabet
import States
import qualified Data.Set as DS
import WordAutomaton
import qualified EpsWordNFA as Eps

import Data.Equivalence.Monad

data WordDFA s a where
  WordDFA :: (Alphabet a, States s) => {
    delta :: DeltaProto a s,
    start :: s,
    acc :: DS.Set s
  } -> WordDFA s a

type DeltaProto a s = a -> s -> s

data MinWordDFA s a where
    MinWordDFA :: (Alphabet a, States s) => {
        automaton :: WordDFA s a,
        usedStates :: DS.Set s,
        unusedStates :: DS.Set s
    } -> MinWordDFA s a

instance (States s, Ord s) => WordAutomaton (WordDFA s) where
  automatonAccepts da word = (runWordDFA da word `DS.member` acc da)
  automatonAcceptsIO da word = print $ if automatonAccepts da word then "DFA accepted" else "DFA didn't accept"

--  runEpsWordNFA :: (Ord s, States s, HasEmptyState s) => EpsWordNFA s a -> Word a -> DS.Set s
runWordDFA :: (States s, Alphabet a) => WordDFA s a -> Word a -> s
runWordDFA da = foldl (flip $ delta da) (start da)


determinize :: (States s, Ord s, Alphabet a) => Eps.EpsWordNFA s a -> WordDFA (DS.Set s) a
determinize ena = WordDFA { start = start', delta = delta', acc = acc' }
    where
        start' = doEpsTrans $ Eps.start ena
        acc'   = DS.filter (\ss -> not $ DS.null (Eps.acc ena `DS.intersection` ss)) allStates
        delta' a = foldMap (doEpsTrans . Eps.delta ena a)
        doEpsTrans states = let reachable = foldMap (Eps.epsDelta ena) states
                    in if DS.null (reachable DS.\\ states)
                        then states
                        else doEpsTrans (DS.union states reachable)

-----------------
-- test cases ---
-----------------

data Sts = Z | O | Tw | Thr | F
    deriving (Show,Eq,Ord,Enum)
instance States Sts where
    allStates = DS.fromList [Z .. F]

data Alph = AZ | AO
    deriving (Show,Eq,Ord,Enum)
instance Alphabet Alph where
    allLetters = [AZ, AO]

da :: WordDFA Sts Alph
da = WordDFA { acc = DS.singleton Tw, delta = d, start = Z}
    where
        d AZ Z   = Z
        d AO Z   = O
        d AZ O   = Tw
        d AO O   = Thr
        d AZ Tw  = F
        d AO Tw  = Z
        d AZ Thr = O
        d AO Thr = Tw
        d AZ F   = Thr
        d AO F   = F

parseAlphString :: String -> Word Alph
parseAlphString = fmap toAlph
    where toAlph '0' = AZ
          toAlph '1' = AO
          toAlph _   = error "Input must consist of 0 and 1"

--      _0_
--      |  |
--       \/
-- o--1--o--0--o
-- \__ε_/

ena :: Eps.EpsWordNFA Sts Alph
ena = Eps.EpsWNFA { Eps.delta = d, Eps.epsDelta = ed, Eps.start = DS.singleton Z, Eps.acc = DS.singleton Tw }
    where
        d AO Z = DS.singleton O
        d AZ O = DS.fromList [O,Tw]
        d _  _ = DS.empty
        --  ed Z = DS.singleton O
        --  ed Z = DS.fromList [O,Tw]
        ed Z = DS.singleton O
        ed O = DS.fromList [Z, O, Tw]
        ed _ = DS.empty


foo = runEquivM DS.singleton DS.union $ do
    equate Z O
    equate Tw Thr
    equate F Z
    fs <- getClass Z
    removeClass Z
    --  equateAll $ filter (/= F) $ DS.toList fs
    --  let bar = DS.toList fs
    cs <- mapM classDesc (DS.toList allStates)
    return $ DS.fromList cs
    desc fs
    
