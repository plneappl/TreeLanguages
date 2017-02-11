{-# LANGUAGE GADTs, MultiParamTypeClasses, InstanceSigs,  FlexibleInstances #-}

module DeterministicAutomaton where

import Prelude hiding (map, filter)
import qualified Prelude as P
import RoseTree
import Alphabet
import States
import NonDeterministicAutomaton (NonDeterministicAutomaton(NA))
import Automaton
import Data.Set
import qualified Data.Foldable as DF
import Lib

data DeterministicAutomaton s a where
  DA :: (Alphabet a, States s, Monoid s) => {
    delta :: DeltaProto a s,
    acc :: Set s,
    states :: Set s
  } -> DeterministicAutomaton s a 

data ExplicitDTA s a where
  ExplicitDA :: (Alphabet a, States s) => {
    deltaH :: DeltaProto a s,
    explAcc :: Set s,
    explStates :: Set s,
    deltaV :: s -> s -> s,
    s0 :: s
  } -> ExplicitDTA s a

instance (Show s, Show a) => Show (DeterministicAutomaton s a) where
  show (DA _ acc states) = "DA {\n\t" ++
    "fun: <binary>\n" ++ 
    "\tacc: " ++ (show acc) ++ "\n" ++
    "\tsts: " ++ (show states) ++ "\n}" 
instance (Show s, Show a) => Show (ExplicitDTA s a) where
  show (ExplicitDA {explAcc = acc', s0 = s0', explStates = sts}) = "ExplicitDA {\n\t" ++
    "fun: <binary>\n" ++ 
    "\tacc: " ++ (show acc') ++ "\n" ++
    "\ts0:  " ++ (show s0') ++ "\n" ++
    "\tsts: " ++ (show sts) ++ "\n}" 


type DeltaProto a s = a -> s -> s

runDeterministicAutomaton :: DeterministicAutomaton s a -> RT a -> s
runDeterministicAutomaton da@(DA delt _ _) (Br a rs) = delt a (DF.foldMap (runDeterministicAutomaton da) rs)
runDeterministicAutomaton (DA delt _ _) (Lf a) = delt a mempty

runExplicitDTA :: ExplicitDTA s a -> RT a -> s
runExplicitDTA da@(ExplicitDA deltH _ _ deltV s0) (Br a rs) = deltH a lastState where
  lastState = P.foldl deltV s0 (P.map (runExplicitDTA da) rs)
runExplicitDTA (ExplicitDA { deltaH = delt, s0 = s' }) (Lf a) = delt a s'

instance (States s, Eq s, Monoid s) => Automaton (DeterministicAutomaton s) where
  automatonAccepts da rt = runDeterministicAutomaton da rt `elem` acc da
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "DTA accepted" else "DTA didn't accept"

instance (States s, Eq s, Monoid s) => Automaton (ExplicitDTA s) where
  automatonAccepts da rt = runExplicitDTA da rt `elem` explAcc da
  automatonAcceptsIO da rt = print $ if automatonAccepts da rt then "DTA accepted" else "DTA didn't accept"

determinize :: (Eq s, Ord s, States s) => NonDeterministicAutomaton s a -> DeterministicAutomaton (NonDetSimulation s) a
determinize (NA { delta = delta, acc = acc, states = naStates }) = DA delta' acc' (map NonDetSimulation $ powerset naStates) where
  acc' = map NonDetSimulation $ filter (\x -> any (`elem` x) acc) (powerset naStates)
  delta' a s = NonDetSimulation $ foldMap (delta a) s

data EQRel s = EQRel {
    allSubelements :: Set s
  , classRelation :: Set (s, s)
} deriving (Eq, Ord)

data EQClass s = EQNeutral | EQClass {
  elements :: Set s,
  relation :: EQRel s
} deriving (Eq, Ord)


instance (States s) => States (EQClass s) 


instance (Ord s, Monoid s) => Monoid (EQClass s) where
  mempty = EQNeutral
  mappend EQNeutral x = x
  mappend x EQNeutral = x
  mappend c1 c2 = let
    e1 = elements c1
    e2 = elements c2
    rel = relation c1 in
    eqClass rel $ mappend (elemAt 0 e1) (elemAt 0 e2)

-- http://antoine.delignat-lavaud.fr/doc/report-M1.pdf
minimize :: (Ord a, Ord s) => DeterministicAutomaton s a -> DeterministicAutomaton (EQClass s) a
minimize da@DA { delta = delta0, acc = acc0 } = DA { 
  delta = delta', acc = acc', states = states' } where
  reachSts = reachable da
  reachDTA = da { states = reachSts }
  eqClass' = eqClass $ EQRel (states reachDTA) (computeEquivSlow reachDTA)
  acc' = map eqClass' acc0
  states' = map eqClass' reachSts
  delta' a s = eqClass' $ delta0 a $ elemAt 0 $ elements s

reachable :: (Ord a, Ord s) => DeterministicAutomaton s a -> (Set s)
reachable (DA delta acc _) = let
  s_init = singleton mempty in
  reach s_init where
  reach ss = let nss = union ss $ next ss in
    if nss == ss then ss else reach nss
  next ss = union (map (uncurry delta) $ fromList $ pairs allLetters $ toList ss)
                  (map (uncurry mappend) $ fromList $ pairs (toList ss) (toList ss))

computeEquivSlow :: (Ord a, Ord s) => DeterministicAutomaton s a -> Set (s, s)
computeEquivSlow (DA delta acc sts) = 
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

eqClass :: (Ord s) => EQRel s -> s -> EQClass s
eqClass r@(EQRel ss eqr) s = EQClass {
    elements = filter (\s' -> s ~~ s' $ eqr) ss
  , relation = r }

(~~) :: (Ord s) => s -> s -> Set (s, s) -> Bool
(~~) s s' eqr = (s, s') `member` eqr || (s', s) `member` eqr



--main :: IO ()
--main = print ""
