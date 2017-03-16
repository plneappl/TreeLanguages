module FiniteFunctions where
import Lib

import Prelude hiding (map, null, splitAt, drop)
import qualified Prelude as P
import Control.Arrow
import Data.Set
import Data.List (intercalate)

-- |A finite endofunction @a → a@.
data EndoFunL a = FunL { unFunL :: [(a, a)] } | Id deriving (Show, Eq, Ord)

instance (Eq a) => Monoid (EndoFunL a) where
  mempty = Id
  mappend = chain

combine :: Eq a => [EndoFunL a] -> [EndoFunL a] -> [EndoFunL a]
combine = pairsWith chain

-- |@chain == (.)@ 
chain :: Eq a => EndoFunL a -> EndoFunL a -> EndoFunL a
chain f1 Id = f1
chain Id f2 = f2
chain f1 f2 = FunL $ P.map (second (appl f2)) $ unFunL f1

applSafe :: Eq a => EndoFunL a -> a -> Maybe a
applSafe Id a = Just a
applSafe (FunL ((a, b):fs)) a' | a == a' = Just b
                        | otherwise = applSafe (FunL fs) a'
applSafe _ _ = Nothing

domain :: (Ord a) => EndoFunL a -> Set a
domain Id = empty
domain f = fromList $ P.map fst $ unFunL f

image :: (Ord a) => EndoFunL a -> Set a
image Id = empty
image f = fromList $ P.map snd $ unFunL f

-- |@appl == ($)@
appl :: Eq a => EndoFunL a -> a -> a
appl f a = case applSafe f a of 
  Just x -> x

-- |Construct an @EndoFunL@ from a function @f@ and its domain @as@.
finiteFunToTuples :: [a] -> (a -> a) -> EndoFunL a
finiteFunToTuples as f = FunL $ P.map (\a -> (a, f a)) as

-- |Construct all possible functions from a Domain @as@ to an image @bs@.
allFunctions :: (Ord a) => Set a -> Set a -> Set (EndoFunL a)
allFunctions as bs | null as || null bs = empty
                   | size as == 1 = fromList [FunL [(a, b)] | let a = elemAt 0 as, b <- toList bs]
                   | otherwise = let
                      a = elemAt 0 as
                      as' = drop 1 as in
                      map FunL $ fromList [fa : fr | fa <- [(a, b) | b <- toList bs], fr <- toList $ map unFunL $ allFunctions as' bs]


