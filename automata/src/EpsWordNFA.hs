{-# LANGUAGE GADTs #-}
module EpsWordNFA where

import Prelude hiding (Word)
import Alphabet
import States
import qualified Data.Set as DS
import WordAutomaton
import ParseLanguages

data EpsWordNFA s a where
  --  EpsWNFA :: (Alphabet a, States s, HasEmptyState s) => {
  EpsWNFA :: (Alphabet a, States s) => {
    delta :: EpsDeltaProto a s,
    epsDelta :: s -> DS.Set s,
    start :: DS.Set s,
    acc :: DS.Set s
  } -> EpsWordNFA s a

type EpsDeltaProto a s = a -> s -> DS.Set s

--  instance (States s, Ord s, HasEmptyState s) => WordAutomaton (EpsWordNFA s) where
instance (States s, Ord s) => WordAutomaton (EpsWordNFA s) where
  automatonAccepts na rt = (runEpsWordNFA na rt `DS.intersection` acc na) /= DS.empty
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "NFA accepted" else "NFA didn't accept"

--  runEpsWordNFA :: (Ord s, States s, HasEmptyState s) => EpsWordNFA s a -> Word a -> DS.Set s
runEpsWordNFA :: (Ord s, States s, Alphabet a) => EpsWordNFA s a -> Word a -> DS.Set s
runEpsWordNFA na = foldl applyTransition (start na)
    where
        applyTransition states letter =
            foldMap (delta na letter) (doEpsTrans states)
        doEpsTrans states = let reachable = foldMap (epsDelta na) states
                    in if DS.null (reachable DS.\\ states)
                        then states
                        else doEpsTrans (DS.union states reachable)


data TestStates = SA | SB | SC | SD deriving (Eq,Ord,Show,Enum)

instance States TestStates where
    allStates = DS.fromList [SA .. SD]

type TestAlph =  Char

instance Alphabet Char where
    allLetters = ['a' .. 'b']

testAut :: EpsWordNFA TestStates TestAlph
testAut = EpsWNFA delta epsDelta (DS.fromList [SA,SB]) (DS.singleton SD)
    where
        delta 'a' SA = DS.singleton SB
        delta 'a' SB = DS.empty
        delta 'b' SA = DS.singleton SC
        delta 'b' SB = DS.singleton SC
        delta 'a' SC = DS.fromList [SC,SD]
        delta _   _  = DS.empty
        epsDelta SB  = DS.singleton SA
        epsDelta _   = DS.empty

data CountableState = CState Int deriving (Eq,Show,Ord)

instance States CountableState where
    allStates = DS.fromList $ fmap CState [1..]

fromRegExp :: (Alphabet a) => RegExp a -> EpsWordNFA CountableState a
fromRegExp Empty = EpsWNFA (const $ const DS.empty) (const DS.empty) DS.empty DS.empty
fromRegExp regex = fst $ fromRegExp' 0 regex

fromRegExp' :: (Alphabet a) => Int -> RegExp a -> (EpsWordNFA CountableState a, Int)
fromRegExp' n Empty = undefined
fromRegExp' n Unit  = (EpsWNFA delta epsDelta (DS.singleton $ CState n) (DS.singleton $ CState $ n+1), totNum)
    where
        totNum              = n+1
        delta               = const $ const DS.empty
        epsDelta (CState s) = if s == n then DS.singleton (CState $ s+1) else DS.empty
fromRegExp' n (Singleton a) = (EpsWNFA delta epsDelta (DS.singleton $ CState n) (DS.singleton $ CState $ n+1), totNum)
    where
        totNum             = n+1
        delta a (CState s) = if s == n then DS.singleton (CState $ s+1) else DS.empty
        epsDelta           = const DS.empty
fromRegExp' n (Star regex) = (EpsWNFA delta' epsDelta' startState endState, totNum)
    where
        endState           = DS.singleton $ CState $ n+1
        startState         = DS.singleton $ CState n
        (starAut, totNum)  = fromRegExp' (n+2) regex
        delta' a (CState s)
            | s > n+1              = delta starAut a (CState s)
            | n <= s && s <= (n+1) = DS.empty
        epsDelta' (CState s)
            | s == n     = start starAut `DS.union` (DS.singleton $ CState (s+1))
            | s == (n+1) = DS.empty
            | s > (n+1)  = if (CState s) `DS.member` (acc starAut)
                           then (epsDelta starAut (CState s)) `DS.union` (start starAut) `DS.union` endState
                           else epsDelta starAut (CState s)
fromRegExp' n (Union rs) = (EpsWNFA delta' epsDelta' startState endState, totNum)
    where
        startState = DS.singleton $ CState n
        endState   = DS.singleton $ CState $ n+1
        auts       = tail $ foldl (\ps r -> case ps of
                                    [] -> [fromRegExp' (n+2) r]
                                    xs@((a,m):as) -> (fromRegExp' (m+1) r) : xs)
                            [] rs
        totNum     = case auts of
                        [] -> error "empty union makes no sense"
                        _  -> snd $ head auts
        delta'     = undefined
        epsDelta'  = undefined
fromRegExp' n (Concat rs) = (EpsWNFA delta' epsDelta' startState endState, totNum)
    where
        auts       = tail $ foldr (\r ps -> case ps of
                                    [] -> [fromRegExp' (n+2) r]
                                    xs@((a,m):as) -> (fromRegExp' (m+1) r) : xs)
                            [] $ rs
        totNum     = case auts of
                        [] -> error "empty concat makes no sense"
                        _  -> snd $ head auts
        startState = undefined
        endState   = undefined
        delta'     = undefined
        epsDelta'  = undefined