{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module TreeLogic where

import Control.Monad
import Data.Maybe
import RoseTree
import qualified Data.Map.Strict as Map
import qualified Data.Set as DS
import Lib
import Prelude hiding (negate)

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
    return undefined
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

