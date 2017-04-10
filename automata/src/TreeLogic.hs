{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, FlexibleInstances, DeriveFunctor, StandaloneDeriving #-}
module TreeLogic where

import Alphabet
import Control.Monad
import Data.Maybe
import RoseTree
import qualified Data.Map.Strict as Map
import qualified Data.Set as DS
import Data.Set ((\\))
import Lib
import Prelude hiding (negate)
import DeterministicAutomaton
import States
import Control.Exception (assert)
import Debug.Trace (trace)

data Path = Eps | PLeft Path | PRight Path deriving (Eq, Ord, Show)

type Ident = String

data Formula a = 
    Label Ident a 
  | ExistsFO Ident (Formula a) 
  | ExistsSO Ident (Formula a) 
  | Elem Ident Ident 
  | LChild Ident Ident 
  | RChild Ident Ident 
  | Or (Formula a) (Formula a)
  | And (Formula a) (Formula a)
  | Not (Formula a) 
  deriving (Eq)

instance (Show a) => Show (Formula a) where
  show (Label v a)    = "Lab(" ++ v ++ ", " ++ show a ++ ")"
  show (ExistsFO v f) = "∃" ++ v ++ " (" ++ show f ++ ")"
  show (ExistsSO v f) = "∃" ++ v ++ " (" ++ show f ++ ")"
  show (Elem x y)     = x ++ " ∈ " ++ y
  show (LChild x y)   = y ++ " ↩ " ++ x
  show (RChild x y)   = x ++ " ↪ " ++ y
  show (Or f1 f2)     = show f1 ++ " ∨ " ++ show f2
  show (And f1 f2)    = show f1 ++ " ∧ " ++ show f2
  show (Not f)        = "¬" ++ show f

data BoolState = Good | Fail deriving (Eq, Ord, Show)
data BT a = L a | T (BT a) (BT a) deriving (Eq, Ord, Show)
instance Monoid a => Monoid (BT a) where
  mempty = L mempty
  mappend (L a1)    (L a2)    = L (mappend a1 a2)
  mappend (T a1 a2) (T b1 b2) = T (mappend a1 b1) (mappend a2 b2)
  mappend _ _ = undefined

instance Monoid BoolState where
  Good `mappend` _ = Good
  _ `mappend` Good = Good
  _ `mappend` _    = Fail
  mempty = Fail

instance Alphabet (DS.Set Ident) where
  allLetters = DS.singleton DS.empty

newtype MList a = MList { unMList :: [a] } deriving (Eq, Ord, Show, Functor)
instance Monoid a => Monoid (MList a) where
  mempty = MList $ repeat mempty
  mappend (MList x) (MList y) = MList $ zipWith mappend x y

instance Applicative MList where
  pure = MList . pure
  (<*>) x y = (...) MList (<*>) (unMList x) (unMList y)

data LastVars s = LV { last :: [DS.Set Ident], this :: s } deriving (Show)
instance Eq s => Eq (LastVars s) where
  (LV _ s1) == (LV _ s2) = s1 == s2
deriving instance Ord s => Ord (LastVars s)

instance (Monoid s) => Monoid (LastVars s) where
  mempty = LV [] mempty
  mappend (LV a1 s1) (LV a2 s2) = LV (mappend a1 a2) (mappend s1 s2)

formulaToDTA :: forall a a' s. 
  (Alphabet a, Eq a, Ord a, Show s, Show a,
    a' ~ (a, DS.Set Ident), 
    s ~ LastVars (BT BoolState)) => 
  Formula a -> DeterministicAutomaton s a'
formulaToDTA (Label v a)    = DA {delta = d, acc = ac, states = sts} where
  d (_,  set) (LV _ (L Good)) = LV [set] (L Good)
  d (a', set) _ = LV [set] (if (v `DS.member` set) && (a == a') then L Good else L Fail)
  ac            = DS.singleton (LV [] (L Good)) 
  sts           = States $ DS.map (LV []) $ DS.fromList [L Good, L Fail]
formulaToDTA (ExistsFO v f) = 
  da { delta = d' } where
  da@DA { delta = d } = formulaToDTA f     
  d' (a', set) s = d (a', set \\ DS.singleton v) s `mappend` d (a', DS.insert v set) s

formulaToDTA (ExistsSO v f) =  
  da { delta = d' } where
  da@DA { delta = d } = formulaToDTA f     
  d' (a', set) s = d (a', set \\ DS.singleton v) s `mappend` d (a', DS.insert v set) s
formulaToDTA (Elem x y)     = DA {delta = d, acc = ac, states = sts} where
  d (_,  set) (LV _ (L Good)) = LV [set] (L Good)
  d (a', set) _ = LV [set] (if (x `DS.member` set) && (y `DS.member` set) then L Good else L Fail)
  ac            = DS.singleton (LV [] (L Good)) 
  sts           = States $ DS.map (LV []) $ DS.fromList [L Good, L Fail]
formulaToDTA (LChild x y)   = DA {delta = d, acc = ac, states = sts} where
  d (_, set) (LV (sLeft:_) _) = LV [set] (if (x `DS.member` set) && (y `DS.member` sLeft) then L Good else L Fail)
  d (_, set) (LV _ x) = LV [set] x
  ac            = DS.singleton (LV [] (L Good)) 
  sts           = States $ DS.map (LV []) $ DS.fromList [L Good, L Fail]
formulaToDTA (RChild x y)   = DA {delta = d, acc = ac, states = sts} where
  d (_, set) (LV (_:sRight:_) _) = LV [set] (if (x `DS.member` set) && (y `DS.member` sRight) then L Good else L Fail)
  d (_, set) (LV _ x) = LV [set] x
  ac            = DS.singleton (LV [] (L Good)) 
  sts           = States $ DS.map (LV []) $ DS.fromList [L Good, L Fail]
formulaToDTA (Or f1 f2)     = da { acc = ac } where
  da1, da2, da :: DeterministicAutomaton s a'
  da1@DA { states = States sts1, acc = acc1 } = formulaToDTA f1
  da2@DA { states = States sts2, acc = acc2 } = formulaToDTA f2
  da@DA { states = States sts } = crossAutomaton da1 da2 
  ac = DS.union (pairsWith' pairAnnotatedState sts1 acc2) (pairsWith' pairAnnotatedState sts2 acc1)
formulaToDTA (And f1 f2)    = da { acc = ac } where
  da1@DA { acc = acc1 } = formulaToDTA f1
  da2@DA { acc = acc2 } = formulaToDTA f2
  da@DA { states = States sts } = crossAutomaton da1 da2 
  ac = pairsWith' pairAnnotatedState acc1 acc2
formulaToDTA (Not f)        = let 
  da@DA {acc = ac, states = States sts} = formulaToDTA f in
  da {acc = sts \\ ac}

pairAnnotatedState (LV a1 s1) (LV a2 s2) = LV (mappend a1 a2) (T s1 s2)
crossAutomaton :: forall a a' s s'. 
  (Eq a, Ord a, Ord s, s' ~ LastVars (BT s), a' ~ (a, DS.Set Ident), Show s, Show a) => 
  DeterministicAutomaton s' a' -> DeterministicAutomaton s' a' -> DeterministicAutomaton s' a'
crossAutomaton
  da1@DA { delta = d1, states = States sts1 } 
  da2@DA { delta = d2, states = States sts2 } = 
    DA { delta = d, acc = error "can't use this automaton", states = States prs } where
    prs = pairsWith' pairAnnotatedState sts1 sts2
    d a@(_, set) (LV la (T s1 s2)) = let
      (LV _ s1') = d1 a $ LV la s1
      (LV _ s2') = d1 a $ LV la s2 in
      LV [set] (T s1' s2')
    d a@(_, set) (LV la s) = let
      (LV _ s1') = d1 a $ LV la s
      (LV _ s2') = d1 a $ LV la s in
      LV [set] (T s1' s2')


forallFO :: Ident -> Formula a -> Formula a
forallFO x f = Not $ ExistsFO x (negate f)
forallSO :: Ident -> Formula a -> Formula a
forallSO x f = Not $ ExistsSO x (negate f)

negate :: Formula a -> Formula a
negate (ExistsFO x f) = forallFO x (negate f)
negate (ExistsSO x f) = forallSO x (negate f)
negate (Or f1 f2) = And (negate f1) (negate f2)
negate (And f1 f2) = Or (negate f1) (negate f2)
negate (Not f) = f
negate f = f

treeAccepted :: (Eq a) => Formula a -> RT a -> Bool
treeAccepted = treeAccepted' Map.empty Map.empty where
  treeAccepted' :: (Eq a) => Map.Map Ident Path -> Map.Map Ident (DS.Set Path) -> Formula a -> RT a -> Bool 
  treeAccepted' mFO mSO (Label var letter) t = isJust $ do
    p <- Map.lookup var mFO
    letter' <- letterAtPath p t 
    guard $ letter' == letter
  treeAccepted' mFO mSO (ExistsFO var f) t = any (\p -> treeAccepted' (Map.insert var p mFO) mSO f t) (allPaths $ depth t)
  treeAccepted' mFO mSO (ExistsSO var f) t = any (\p -> treeAccepted' mFO (Map.insert var p mSO) f t) (powerset $ DS.fromList $ allPaths $ depth t)
  treeAccepted' mFO mSO (Elem varFO varSO) t = isJust $ do
    p <- Map.lookup varFO mFO 
    ps <- Map.lookup varSO mSO 
    guard $ p `DS.member` ps

  treeAccepted' mFO mSO (LChild x y) t = isJust $ do
    px <- Map.lookup x mFO
    py <- Map.lookup y mFO
    guard $ PLeft px == py 

  treeAccepted' mFO mSO (RChild x y) t = isJust $ do
    px <- Map.lookup x mFO
    py <- Map.lookup y mFO
    guard $ PRight px == py 

  treeAccepted' mFO mSO (Or f1 f2) t = treeAccepted' mFO mSO f1 t || treeAccepted' mFO mSO f2 t
  treeAccepted' mFO mSO (And f1 f2) t = treeAccepted' mFO mSO f1 t && treeAccepted' mFO mSO f2 t
  treeAccepted' mFO mSO (Not f) t = not $ treeAccepted' mFO mSO f t 

depth :: RT a -> Int
depth (Lf _) = 0
depth (Br _ lfs) = 1 + maximum (map depth lfs)

allPaths :: Int -> [Path]
allPaths 0 = [Eps]
allPaths n = concatMap (\p -> [PLeft p, PRight p, p]) (allPaths $ n - 1)

letterAtPath :: Path -> RT a -> Maybe a
letterAtPath Eps (Lf a) = Just a
letterAtPath Eps (Br a _) = Just a
letterAtPath p (Br _ chlds) | length chlds > 1 = case p of
  PLeft p' -> letterAtPath p' (head chlds)
  PRight p' -> letterAtPath p' (head $ tail chlds) 
  _ -> Nothing
letterAtPath _ _ = Nothing

freeVars :: Formula a -> DS.Set Ident
freeVars (Label v _)    = DS.singleton v
freeVars (ExistsFO v f) = DS.delete v $ freeVars f 
freeVars (ExistsSO v f) = DS.delete v $ freeVars f 
freeVars (Elem x y)     = DS.fromList [x, y] 
freeVars (LChild x y)   = DS.fromList [x, y] 
freeVars (RChild x y)   = DS.fromList [x, y] 
freeVars (Or f1 f2)     = DS.union (freeVars f1) (freeVars f2)
freeVars (And f1 f2)    = DS.union (freeVars f1) (freeVars f2)
freeVars (Not f)        = freeVars f

bindAllVars :: Formula a -> Formula a
bindAllVars f = let fv = freeVars f in
  foldr forallFO f fv

