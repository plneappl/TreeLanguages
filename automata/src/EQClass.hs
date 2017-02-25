module EQClass where

import Prelude hiding (map, filter)
import Data.Set
import Control.Exception (assert)

data EQRel s = EQRel {
    allSubelements :: Set s
  , classRelation :: Set (s, s)
} deriving (Eq, Ord, Show)

data EQClass s = EQNeutral | EQClass {
  elements :: Set s,
  relation :: EQRel s
} deriving (Eq, Ord)

instance (Show s) => Show (EQClass s) where
  show EQNeutral = "⟦ϵ⟧"
  show EQClass {elements = e} = assert (size e > 0) $ "⟦" ++ show (elemAt 0 e) ++ "⟧"

repr :: (Monoid s) => EQClass s -> s
repr = repr' mempty

repr' :: s -> EQClass s -> s
repr' s EQNeutral = s
repr' _ EQClass {elements = e} = assert (size e > 0) $ elemAt 0 e


eqClass :: (Ord s) => EQRel s -> s -> EQClass s
eqClass r@(EQRel ss eqr) s = EQClass {
    elements = filter (\s' -> s ~~ s' $ r) ss
  , relation = r }

(~~) :: (Ord s) => s -> s -> EQRel s -> Bool
(~~) s s' eqr = (s, s') `member` classRelation eqr || (s', s) `member` classRelation eqr

instance (Ord s, Monoid s) => Monoid (EQClass s) where
  mempty = EQNeutral
  mappend EQNeutral x = x
  mappend x EQNeutral = x
  mappend EQClass {elements = e1, relation = rel} EQClass{elements = e2} =
    eqClass rel $ mappend (elemAt 0 e1) (elemAt 0 e2)