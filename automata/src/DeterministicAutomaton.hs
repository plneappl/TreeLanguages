{-| 
Module      : DeterministicAutomaton
Description : Contains the definition of DTAs, operations on it and most conversions from and to it.

The Module DeterministicAutomaton contains the data definition of DTAs 'DA', 
methods for determinizing NTAs and minimizing DTAs, 
as well as converters to and from VDPAs.
-}
{-# LANGUAGE GADTs, FlexibleInstances, ScopedTypeVariables #-}

module DeterministicAutomaton (
      DeterministicAutomaton(..)
    , DeltaProto
    , runDeterministicAutomaton
    , determinize
    , minimize
    , fromDVPA
    , toDVPA
    , PushdownAlphabet(..)
    , reachable
    , fromPathRegexOr
    , fromPathRegexAnd
  ) where

import Prelude hiding (map, filter, null)
import qualified Prelude as P
import RoseTree
import Alphabet
import States
import qualified NonDeterministicAutomaton as NA
import Automaton
import Data.Set
import qualified Data.Foldable as DF
import Lib
import EQClass
import VisiblyPushdownAutomaton
import FiniteFunctions
import Control.Monad (guard)
import Control.Exception (assert)
import qualified RegExp as RE
import qualified TransMonoid as TM
import qualified EpsWordNFA as NFA
import qualified WordDFA as DFA
import Debug.Trace (trace)

-- |A DTA (Deterministic Tree Automaton) with 'States' @s@ and 'Alphabet' @a@. @s@ has to be a 'Monoid'.
data DeterministicAutomaton s a where
  DA :: (Alphabet a, Monoid s) => {
    delta :: DeltaProto a s,
    acc :: Set s,
    states :: States s
  } -> DeterministicAutomaton s a 


instance (Show s, Show a) => Show (DeterministicAutomaton s a) where
  show (DA _ acc states) = "DA {\n\t" ++
    "fun: <binary>\n" ++ 
    "\tacc: " ++ (show acc) ++ "\n" ++
    "\tsts: " ++ (show states) ++ "\n}" 


type DeltaProto a s = a -> s -> s

-- |Run a DTA on a Tree.
runDeterministicAutomaton :: DeterministicAutomaton s a -> RT a -> s
runDeterministicAutomaton da@(DA delt _ _) (Br a rs) = delt a (DF.foldMap (runDeterministicAutomaton da) rs)
runDeterministicAutomaton (DA delt _ _) (Lf a) = delt a mempty


instance (Eq s, Monoid s) => Automaton (DeterministicAutomaton s) where
  automatonAccepts da rt = runDeterministicAutomaton da rt `elem` acc da
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "DTA accepted" else "DTA didn't accept"

-- |Determinization of NTAs (Non-Deterministic Tree Automaton).
determinize :: (Eq s, Ord s) => NA.NonDeterministicAutomaton s a -> DeterministicAutomaton (NonDetSimulation s) a
determinize (na@(NA.NA {})) = DA delta' acc' (statesNonDetSimulation $ NA.states na) where
  acc' = map NonDetSimulation $ filter (\x -> any (`elem` x) $ NA.acc na) (powerset $ allStates $ NA.states na)
  delta' a s = NonDetSimulation $ foldMap (NA.delta na a) s


-- http://antoine.delignat-lavaud.fr/doc/report-M1.pdf
-- |Minimization of DTAs.
minimize :: (Ord a, Ord s) => DeterministicAutomaton s a -> DeterministicAutomaton (EQClass a s) a
minimize da@DA { delta = delta0, acc = acc0 } = DA { 
  delta = delta', acc = acc', states = states' } where
  reachWitnesses = reachable da
  reachSts = States $ map fromWitness reachWitnesses 
  reachDTA = da { states = reachSts, acc = acc0 `intersection` allStates reachSts }
  eqClass' = eqClass $ EQRel (reachWitnesses) (computeEquivSlow reachDTA)
  acc' = map eqClass' $ acc reachDTA
  states' = States $ map eqClass' $ allStates reachSts
  delta' a s = eqClass' $ delta0 a $ repr s


reachable :: (Ord a, Ord s) => DeterministicAutomaton s a -> Set (Witness a s)
reachable (DA delta acc _) = let
  s_init = singleton $ Witness (mempty, []) in
  reach s_init where
  reach ss = let nss = union ss $ next ss in
    if nss == ss then ss else reach nss
  next ss = union (map (\(a, Witness (s, t)) -> Witness (delta a s, [Br a t])) $ pairs' allLetters ss)
                  (map (\(Witness (s1, t1), Witness (s2, t2)) -> Witness (mappend s1 s2, t1 ++ t2)) $ pairs' ss ss)



computeEquivSlow :: (Ord a, Ord s) => DeterministicAutomaton s a -> Set (s, s)
computeEquivSlow (DA delta acc (States sts)) = 
  result $ runSteps markInit where
  delta' = flip delta mempty
  firstIter = (map delta' allLetters)
  allStatesPairs = pairs' sts sts
  inv sts = allStatesPairs \\ sts
  markInitS = pairs' acc (sts \\ acc)
  markInitQ = pairs' (firstIter \\ acc) (intersection firstIter acc)
  markInit = (markInitS, markInitQ)
  oneStepA a (s1, s2) = (delta a s1, delta a s2)
  oneStepS1 s (s1, s2) = (s1 `mappend` s, s2 `mappend` s)
  oneStepS2 s (s1, s2) = (s `mappend` s1, s `mappend` s2)

  alphaStep'  (ms, mq) = ms `union` foldMap (\a -> filter (\s12 -> oneStepA  a s12 `member` (union ms mq)) (inv ms)) allLetters
  stateStep1' (ms, mq) = ms `union` foldMap (\s -> filter (\s12 -> oneStepS1 s s12 `member` (union ms mq)) (inv ms)) sts
  stateStep2' (ms, mq) = mq `union` foldMap (\s -> filter (\s12 -> oneStepS2 s s12 `member` (union ms mq)) (inv mq)) sts
  alphaStep  msq@(ms, mq) = (alphaStep'  msq, mq)
  stateStep1 msq@(ms, mq) = (stateStep1' msq, mq)
  stateStep2 msq@(ms, mq) = (ms, stateStep2' msq)
  symetrizise' ss = ss `union` (map (\(s1, s2) -> (s2, s1)) ss)
  symetrizise (ms, mq) = (symetrizise' ms, symetrizise' mq)
  step = symetrizise . stateStep2 . stateStep1 . alphaStep
  runSteps msts = let nsts = step msts in
    if nsts == msts then nsts else runSteps nsts

  result (ms, mq) = union (inv ms) (inv mq)

-- |'DVPA' (Deterministic Visibly Pushdown Automaton) → DTA. Also returns a converter of words to trees.
fromDVPA :: (Show a) => DVPA s g a -> (DeterministicAutomaton (EndoFunL s) (a, a), [a] -> [RT (a, a)])
fromDVPA vpa@DVPA {} = (DA {
    delta   = delta',   
    acc     = acc', 
    states  = states'  
  }, transform) where
  accPairs = map (\e -> (startStateD vpa, e)) $ accD vpa
  vpaStates = allStates $ statesD vpa
  allFuns = allFunctions vpaStates vpaStates
  acc' = filter (\f -> not $ null $ accPairs `intersection` fromList (unFunL f)) $ allFuns
  -- a -> s -> s
  -- first case: c and r are really call and return
  delta' (c, r) f | c `member` callD vpa = let 
    f1 = [ (s0, s1, g) | s0 <- toList vpaStates, let (s1, g) = deltaCallD vpa c s0 ] 
    f2 = P.map (\(s0, s1, g) -> (s0, appl f s1, g)) f1
    f3 = P.map (\(s0, s2, g) -> (s0, deltaRetD vpa r s2 g)) f2 in 
    FunL f3
  -- second case: (c, r) is a leaf, actually (a, a) with a ∈ Σ_internal
                  | otherwise = assert (c == r) $ FunL [(s0, deltaInternD vpa c s0) | s0 <- toList vpaStates]
  states' = States $ allFuns

  -- transform a word into a forest
  transform [] = []
  transform (a:as) | a `member` internD vpa = Lf (a, a) : transform as
                   | a `member` callD   vpa = let
                      asRev = reverse as
                      (subWord, a', rest) = findClosing as in
                      Br (a, a') (transform subWord) : transform rest
                   | a `member` retD    vpa = error $ "word isn't well matched: " ++ (show (a:as))
  -- find the next closing letter. divide word into before and after, example:
  -- [][[]]]][] -> [][[]], ], ][]
  -- case 1: a is a closing letter, so we are done.
  findClosing (a:as) | a `member` retD    vpa = ([], a, as)
  -- case 2: a is internal. append it to the first part, return recursive.
                     | a `member` internD vpa = let
                       (sub', a', rest) = findClosing as in
                       (a:sub', a', rest)
  -- case 3: a is an opening letter. first close it, then search in the part after the first closing for our real result. finally append everything.
  -- example: 
  -- a  as    (sub' a'  rest')  (a   sub'  a', a'', rest'') (sum  a''  rest'')
  -- [  ]][] -> ϵ,   ],  ][]  -> [ +  ϵ  + ],  ],   []     = [],   ],   []
                     | a `member` callD   vpa = let
                      (sub', a', rest') = findClosing as
                      (sub'', a'', rest'') = findClosing rest' 
                      prefix = a : sub' ++ [a'] ++ sub'' in
                      -- check we didn't change the order and didn't lose anything
                      assert ((prefix ++ [a''] ++ rest'') == a:as)
                      (prefix, a'', rest'')

data PushdownAlphabet a = Call a | Intern a | Return a deriving (Eq, Ord)
instance Show a => Show (PushdownAlphabet a) where
  show (Call a) = show a
  show (Intern a) = show a ++ show a ++ "'"
  show (Return a) = show a ++ "'"
instance (Alphabet a, Ord a) => Alphabet (PushdownAlphabet a) where
  allLetters = map Call allLetters `union` map Intern allLetters `union` map Return allLetters

-- need failure state, bottom letter --> States ~ Maybe s, StackAlph ~ Maybe (a, s) 
-- |DTA → DVPA. Also returns a converter of trees to words.
toDVPA :: (Ord a, Ord s) => DeterministicAutomaton s a -> (DVPA (Maybe s) (Maybe (a, s)) (PushdownAlphabet a), RT a -> [(PushdownAlphabet a)])
toDVPA da@DA {} = (DVPA {
    statesD      = States $ map Just $ allStates $ states da,
    startStateD  = Just mempty,
    accD         = map Just $ acc da,
    callD        = map Call   allLetters,
    retD         = map Return allLetters,
    internD      = map Intern allLetters,
    -- Call saves current state on stack for return call, initializes lower state
    --           ┌──┴──┐                  ┌──┴──┐           
    --           s'    a           ⇒      s'    a          
    --         ┌─┈┈┈┈┬─┴─┬─┈┈┈┈┐                │        
    --         s1    si  sj    sn      0 + s1 + s2 + … + sn       
    deltaCallD   = \ (Call a) s -> (Just mempty, s >>= \s' -> Just (a, s')),
    -- Situation on return:    
    --  ┌──┴──┐    need to calculate:    s'' + δ(a', s')
    -- s''    a'    
    --        │         
    --        s'
    deltaRetD    = \ (Return a) s sa -> do
      s' <- s
      (a', s'') <- sa
      guard $ a == a'
      return $ s'' `mappend` delta da a s',
    -- Intern letters are leaves, therefore just do as the DTA would do
    deltaInternD = \ (Intern a) s -> do
      s' <- s
      return $ s' `mappend` (delta da a mempty) 

  }, transform) where
  transform (Lf a)    = [Intern a]
  transform (Br l ts) = Call l : (concatMap transform ts) ++ [Return l]


-- reduce to acceptance by DFA via 
--       ∀ p: ϕ(p)  ⇔  ¬∃ p: ¬ϕ(p)
--    invert ACC(DTA)--^     ^-- invert ACC(DFA(NFA(RegEx)))

-- |Define an automaton via Regex. Accept ⇔ all paths are accepted by Regex.
fromPathRegexAnd :: forall a s nds. (nds ~ Set NFA.CountableState, s ~ TM.TransMonoid nds a, Eq a, Alphabet a, Ord a) => RE.RegExp a -> DeterministicAutomaton (Set s) a
fromPathRegexAnd r = dta { acc = stsDTA \\ acc dta } where
  nfa :: NFA.EpsWordNFA NFA.CountableState a
  nfa = NFA.fromRegExp r
  dfa :: DFA.WordDFA nds a
  dfa = DFA.determinize nfa  
  stsDFA = DFA.states dfa
  dfa' = dfa { DFA.acc = stsDFA \\ DFA.acc dfa }
  dta = fromDFA dfa'
  stsDTA = allStates $ states dta


-- |Define an automaton via Regex. Accept ⇔ any path is accepted by Regex.
fromPathRegexOr :: forall a s nds. 
  (nds ~ Set NFA.CountableState, s ~ TM.TransMonoid nds a,
    Eq a, Alphabet a, Ord a) => 
  RE.RegExp a -> DeterministicAutomaton (Set s) a
fromPathRegexOr r = fromDFA dfa where
  nfa :: NFA.EpsWordNFA NFA.CountableState a
  nfa = NFA.fromRegExp r
  dfa :: DFA.WordDFA nds a
  dfa = DFA.determinize nfa 

fromDFA :: forall a s nds. (nds ~ Set NFA.CountableState, s ~ TM.TransMonoid nds a, Eq a, Alphabet a, Ord a) => DFA.WordDFA nds a -> DeterministicAutomaton (Set s) a
fromDFA dfa = DA { delta = delta', acc = acc', states = states' } where
  tfa :: DFA.WordDFA s a
  tfa = TM.transWDFA dfa
  morph :: a -> s
  morph = snd $ TM.transMonoid dfa
  delta' a s = if null s then singleton $ morph a else map (morph a `mappend`) s
  acc' = filter (\s -> not $ null $ s `intersection` DFA.acc tfa) $ allStates states'
  states' = States $ powerset $ DFA.states tfa