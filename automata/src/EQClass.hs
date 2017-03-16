{-| 
Module      : EQClass
Description : Contains the definition of equivalence relations and classes.

The Module EQClass contains definitions of equivalence relations 'EQRel' and classes 'EQClass'. 
Elements of equivalence relations/equivalence classes know which tree produces them, so called 'Witness'es.
-}
module EQClass where

import Prelude hiding (map, filter)
import qualified Prelude as P
import Data.Set
import Control.Exception (assert)
import RoseTree
import Pretty
import Lib
import Control.Monad (join)
import Data.Maybe (catMaybes, listToMaybe)

-- |An equivalence relation of states. Also contains minimum Forest over Alphabet @a@ that produces a particular state.
data EQRel a s = EQRel {
    allSubelements :: Set (Witness a s)
  , classRelation :: Set (s, s)
} deriving (Eq, Ord, Show)

-- |A witness of how a particular state is reached.
newtype Witness a s = Witness { unWitness :: (s, [RT a]) } deriving (Show)

fromWitness :: Witness a s -> s
fromWitness (Witness (s, _)) = s

actualWitness :: Witness a s -> [RT a]
actualWitness (Witness (_, t)) = t

-- |Only compare states inside, important for f.e. Set
instance Eq s => Eq (Witness a s) where
  (Witness (s1, _)) == (Witness (s2, _)) = s1 == s2

instance Ord s => Ord (Witness a s) where
  compare (Witness (s1, _)) (Witness (s2, _)) = compare s1 s2

-- |Represents a singular equivalence class of a certain equivalence relation.
data EQClass a s = EQNeutral | EQClass {
  elements :: Set (Witness a s),
  relation :: EQRel a s
} deriving (Eq, Ord)

(→) :: Bool -> Bool -> Bool
b1 → b2 = (not b1) || b2

-- -- |Show an @EQClass@ via it's witness
--instance (Show a) => Show (EQClass a s) where
--  show EQNeutral = "⟦ϵ⟧"
--  show EQClass {elements = e} = assert (size e > 0) $ "⟦" ++ P.foldr ((++) . init . showRT) "" (actualWitness $ elemAt 0 e) ++ "⟧"

-- |Show an @EQClass@ via a state
instance (Show s) => Show (EQClass a s) where
  show EQNeutral = "⟦ϵ⟧"
  show EQClass {elements = e, relation = r} = assert (size e > 0) $ "⟦" ++ show (fromWitness $ elemAt 0 e) ++ "⟧"


repr :: (Monoid s) => EQClass a s -> s
repr = repr' mempty

repr' :: s -> EQClass a s -> s
repr' s EQNeutral = s
repr' _ EQClass {elements = e} = assert (size e > 0) $ fromWitness $ elemAt 0 e

-- |Get an element's @EQClass@ from a @EQRel@.
eqClass :: (Ord s) => EQRel a s -> s -> EQClass a s
eqClass r@(EQRel ss eqr) s = EQClass {
    elements = filter (\(Witness (s', _)) -> s ~~ s' $ r) ss
  , relation = r }

(~~) :: (Ord s) => s -> s -> EQRel a s -> Bool
(~~) s s' eqr = (s, s') `member` classRelation eqr 
-- EQClasses are reflexive, therefore we need not test this
-- || (s', s) `member` classRelation eqr

instance (Ord s, Monoid s) => Monoid (EQClass a s) where
  mempty = EQNeutral
  mappend EQNeutral x = x
  mappend x EQNeutral = x
  mappend EQClass {elements = e1, relation = rel} EQClass {elements = e2, relation = rel'} =
    assert (rel == rel') $ eqClass rel $ mappend (fromWitness $ elemAt 0 e1) (fromWitness $ elemAt 0 e2)

-- |Some assertions for debugging. @EQRel@ should always satisfy all of these.
assertTransitive :: (Eq s) => Set s -> EQRel a s -> [((s, s), (s, s))]
assertTransitive ss EQRel { classRelation = rel } = let
  ss' = toList ss
  ssPairs = pairs ss' ss'
  ssPairs' = pairs ssPairs ssPairs in
  catMaybes $ P.map (\(p1@(s1, s2), p2@(s3, s4)) -> if ((s2 == s3) && (p1 `elem` rel) && (p2 `elem` rel)) → ((fst p1, snd p2) `elem` rel) then Nothing else Just (p1, p2)) ssPairs'

assertReflexive :: (Eq s) => Set s -> EQRel a s -> [(s, s)]
assertReflexive ss EQRel { classRelation = rel } = let
  ss' = toList ss in
  catMaybes $ P.map (\s -> if (s, s) `elem` rel then Nothing else Just (s, s)) ss'

assertSymetric :: (Eq s) => Set s -> EQRel a s -> [(s, s)]
assertSymetric ss EQRel { classRelation = rel } = let
  ss' = toList ss
  ssPairs = pairs ss' ss' in
  catMaybes $ P.map (\p@(s1, s2) -> if (p `elem` rel) → ((s2, s1) `elem` rel) then Nothing else Just (s1, s2)) ssPairs