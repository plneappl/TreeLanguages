{-# LANGUAGE GADTs #-}

module WordDFA where

import Prelude hiding (Word)
import Alphabet
import States
import qualified Data.Set as DS
import WordAutomaton
import qualified EpsWordNFA as Eps

import Lib

import Data.Equivalence.Monad

data WordDFA s a where
  WordDFA :: (Alphabet a, States s) => {
    delta :: DeltaProto a s,
    start :: s,
    acc :: DS.Set s,
    states :: DS.Set s
  } -> WordDFA s a

type DeltaProto a s = a -> s -> s

--  data MinWordDFA s a where
    --  MinWordDFA :: (Alphabet a, States s) => {
        --  automaton :: WordDFA s a,
        --  usedStates :: DS.Set s,
        --  unusedStates :: DS.Set s
    --  } -> MinWordDFA s a

instance (States s, Ord s) => WordAutomaton (WordDFA s) where
  automatonAccepts da word = runWordDFA da word `DS.member` acc da
  automatonAcceptsIO da word = print $ if automatonAccepts da word then "DFA accepted" else "DFA didn't accept"

--  runEpsWordNFA :: (Ord s, States s, HasEmptyState s) => EpsWordNFA s a -> Word a -> DS.Set s
runWordDFA :: (States s, Alphabet a) => WordDFA s a -> Word a -> s
runWordDFA da = foldl (flip $ delta da) (start da)


determinize :: (States s, Ord s, Alphabet a) => Eps.EpsWordNFA s a -> WordDFA (DS.Set s) a
determinize ena = WordDFA { start = start', delta = delta', acc = acc', states=states' }
    where
        states' = powerset $ Eps.states ena
        start' = doEpsTrans $ Eps.start ena
        acc'   = DS.filter (\ss -> not $ DS.null (Eps.acc ena `DS.intersection` ss)) states'
        delta' a = foldMap (doEpsTrans . Eps.delta ena a)
        doEpsTrans states = let reachable = foldMap (Eps.epsDelta ena) states
                    in if DS.null (reachable DS.\\ states)
                        then states
                        else doEpsTrans (DS.union states reachable)


minimize :: (States s, Ord s, Alphabet a) => WordDFA s a -> WordDFA (DS.Set s) a
minimize da = WordDFA { delta=delta', start=start', acc=acc', states=states' }
    where
        reachable = stepClosure (DS.singleton $ start da)
        stepClosure states = let reachable' = foldMap doOneStep states
                in if DS.null (reachable' DS.\\ states)
                    then states
                    else stepClosure (DS.union states reachable')
        doOneStep s = DS.fromList $ fmap (flip (delta da) s) allLetters
        refine p w = if DS.null w
                     then p
                     else let (a, w')   = DS.deleteFindMin w
                              (p', w'') = foldl (refine' a) (p, w') allLetters
                        in refine p' w''
        refine' a (p, w) l = let x = DS.filter ((`DS.member` a) . delta da l) reachable
                              in DS.foldl (checkAndRefine x) (p, w) p
        checkAndRefine x (p, w) y = let   xInterY = x `DS.intersection` y
                                          yLessX  = y DS.\\ x
                                    in if any DS.null [xInterY, yLessX]
                                       then
                                          (p, w)
                                       else
                                          let p' = DS.delete y p `DS.union` DS.fromList [xInterY, yLessX]
                                          in if y `DS.member` w
                                          then
                                             (p', DS.delete y w `DS.union` DS.fromList [xInterY, yLessX])
                                          else if DS.size xInterY <= DS.size yLessX
                                          then
                                             (p', DS.insert xInterY w)
                                          else
                                             (p', DS.insert yLessX w)
        delta' a ss = let  ds = DS.map (delta da a) ss
                           ss' = DS.filter (\ts -> not $ DS.null (ds `DS.intersection` ts)) states'
                        in if DS.size ss' == 1
                           then
                              DS.findMin ss'
                           else if DS.null ss'
                           then
                              error "No new state found in minimized DFA"
                           else
                              error "Too many new states found in minimized DFA"
        start'  = let   s = DS.filter (\ss -> start da `DS.member` ss) states'
                  in if DS.size s == 1
                     then
                        DS.findMin s
                     else if DS.null s
                     then
                        error "No starting states found after determinizing"
                     else
                        error "Too many starting states found after determinizing"
        acc'    = DS.filter (\ss -> not $ DS.null (acc da `DS.intersection` ss)) states'
        states' = let   (w, n) = DS.partition (`DS.member` acc da) reachable
                  in refine (DS.fromList [w,n]) (DS.singleton w)

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

-- w \in L <=> w \equiv 2 mod 5
da :: WordDFA Sts Alph
da = WordDFA { acc = DS.singleton Tw, delta = d, start = Z, states = allStates}
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
ena = Eps.EpsWNFA { Eps.delta = d, Eps.epsDelta = ed, Eps.start = DS.singleton Z, Eps.acc = DS.singleton Tw, Eps.states = allStates }
    where
        d AO Z = DS.fromList [O]
        d AZ O = DS.fromList [O,Tw]
        d _  _ = DS.empty
        --  ed Z = DS.singleton O
        --  ed Z = DS.fromList [O,Tw]
        ed Z = DS.fromList [O]
        --  ed O = DS.fromList [Z, O, Tw]
        ed _ = DS.empty

da' :: WordDFA (DS.Set Sts) Alph
da' = determinize ena

ma' :: WordDFA (DS.Set (DS.Set Sts)) Alph
ma' = minimize da'

-- o----1--->o----->0--->o
--  \___0___>o------0--->o
da'' :: WordDFA Sts Alph
da'' = WordDFA { delta=d, start=s, acc=a, states=allStates }
   where
      s = Z
      a = DS.fromList [ Tw, F ]
      d AZ Z   = Thr
      d AO Z   = O
      d AZ O   = Tw
      d AO O   = Tw
      d AZ Tw  = Tw
      d AO Tw  = O
      d AZ Thr = F
      d AO Thr = F
      d AZ F   = F
      d AO F   = Thr

ma'' :: WordDFA (DS.Set Sts) Alph
ma'' = minimize da''

allWords :: [String]
allWords = "" : concatMap (\s -> ['0':s, '1':s]) allWords

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
    
