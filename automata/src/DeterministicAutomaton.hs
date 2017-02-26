{-# LANGUAGE GADTs, FlexibleInstances #-}

module DeterministicAutomaton where

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
import Control.Exception (assert)

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

runDeterministicAutomaton :: DeterministicAutomaton s a -> RT a -> s
runDeterministicAutomaton da@(DA delt _ _) (Br a rs) = delt a (DF.foldMap (runDeterministicAutomaton da) rs)
runDeterministicAutomaton (DA delt _ _) (Lf a) = delt a mempty


instance (Eq s, Monoid s) => Automaton (DeterministicAutomaton s) where
  automatonAccepts da rt = runDeterministicAutomaton da rt `elem` acc da
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "DTA accepted" else "DTA didn't accept"

determinize :: (Eq s, Ord s) => NA.NonDeterministicAutomaton s a -> DeterministicAutomaton (NonDetSimulation s) a
determinize (na@(NA.NA {})) = DA delta' acc' (statesNonDetSimulation $ NA.states na) where
  acc' = map NonDetSimulation $ filter (\x -> any (`elem` x) $ NA.acc na) (powerset $ allStates $ NA.states na)
  delta' a s = NonDetSimulation $ foldMap (NA.delta na a) s


-- http://antoine.delignat-lavaud.fr/doc/report-M1.pdf
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
  next ss = union (map (\(a, Witness (s, t)) -> Witness (delta a s, [Br a t])) $ fromList $ pairs allLetters $ toList ss)
                  (map (\(Witness (s1, t1), Witness (s2, t2)) -> Witness (mappend s1 s2, t1 ++ t2)) $ fromList $ pairs (toList ss) (toList ss))



computeEquivSlow :: (Ord a, Ord s) => DeterministicAutomaton s a -> Set (s, s)
computeEquivSlow (DA delta acc (States sts)) = 
  result $ runSteps markInit where
  delta' = flip delta mempty
  firstIter = (map delta' $ fromList allLetters)
  allStatesPairs = fromList $ pairs (toList sts) (toList sts)
  inv sts = allStatesPairs \\ sts
  markInitS = fromList $ pairs (toList acc) (toList $ sts \\ acc)
  markInitQ = fromList $ pairs (toList $ firstIter \\ acc) (toList $ intersection firstIter acc)
  markInit = (markInitS, markInitQ)
  oneStepA a (s1, s2) = (delta a s1, delta a s2)
  oneStepS1 s (s1, s2) = (s1 `mappend` s, s2 `mappend` s)
  oneStepS2 s (s1, s2) = (s `mappend` s1, s `mappend` s2)

  alphaStep'  (ms, mq) = unions $ ms :  P.map (\a -> filter (\s12 -> oneStepA  a s12 `member` (union ms mq)) (inv ms)) allLetters
  stateStep1' (ms, mq) = unions $ ms : (P.map (\s -> filter (\s12 -> oneStepS1 s s12 `member` (union ms mq)) (inv ms)) $ toList sts)
  stateStep2' (ms, mq) = unions $ mq : (P.map (\s -> filter (\s12 -> oneStepS2 s s12 `member` (union ms mq)) (inv mq)) $ toList sts)
  alphaStep  msq@(ms, mq) = (alphaStep'  msq, mq)
  stateStep1 msq@(ms, mq) = (stateStep1' msq, mq)
  stateStep2 msq@(ms, mq) = (ms, stateStep2' msq)
  step = stateStep2 . stateStep1 . alphaStep
  runSteps msts = let nsts = step msts in
    if nsts == msts then nsts else runSteps nsts

  result (ms, mq) = union (inv ms) (inv mq)


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
                  | otherwise = FunL [(s0, deltaInternD vpa c s0) | s0 <- toList vpaStates]
  states' = States $ allFuns

  transform [] = []
  transform (a:as) | a `member` internD vpa = Lf (a, a) : transform as
                   | a `member` callD   vpa = let
                      asRev = reverse as
                      (subWord, a', rest) = findClosing as in
                      Br (a, a') (transform subWord) : transform rest
                   | a `member` retD    vpa = error $ "word isn't well matched: " ++ (show (a:as))

  findClosing (a:as) | a `member` retD    vpa = ([], a, as)
                     | a `member` internD vpa = let
                       (sub', a', rest) = findClosing as in
                       (a:sub', a', rest)
                     | a `member` callD   vpa = let
                      (sub', a', rest') = findClosing as
                      (sub'', a'', rest'') = findClosing rest' in
                      assert ((a : sub' ++ [a'] ++ sub'' ++ [a''] ++ rest'') == a:as)
                      (a : sub' ++ [a'] ++ sub'', a'', rest'')
