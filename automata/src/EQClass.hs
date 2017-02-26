module EQClass where

import Prelude hiding (map, filter)
import qualified Prelude as P
import Data.Set
import Control.Exception (assert)
import RoseTree
import Pretty

data EQRel a s = EQRel {
    allSubelements :: Set (Witness a s)
  , classRelation :: Set (s, s)
} deriving (Eq, Ord, Show)

newtype Witness a s = Witness { unWitness :: (s, [RT a]) } deriving (Show)

fromWitness :: Witness a s -> s
fromWitness (Witness (s, _)) = s

actualWitness :: Witness a s -> [RT a]
actualWitness (Witness (_, t)) = t

instance Eq s => Eq (Witness a s) where
  (Witness (s1, _)) == (Witness (s2, _)) = s1 == s2


instance Ord s => Ord (Witness a s) where
  compare (Witness (s1, _)) (Witness (s2, _)) = compare s1 s2

data EQClass a s = EQNeutral | EQClass {
  elements :: Set (Witness a s),
  relation :: EQRel a s
} deriving (Eq, Ord)


instance (Show a) => Show (EQClass a s) where
  show EQNeutral = "⟦ϵ⟧"
  show EQClass {elements = e} = assert (size e > 0) $ "⟦" ++ P.foldr ((++) . init . showRT) "" (actualWitness $ elemAt 0 e) ++ "⟧"

--instance (Show s) => Show (EQClass a s) where
--  show EQNeutral = "⟦ϵ⟧"
--  show EQClass {elements = e} = assert (size e > 0) $ "⟦" ++ show (fromWitness $ elemAt 0 e) ++ "⟧"


repr :: (Monoid s) => EQClass a s -> s
repr = repr' mempty

repr' :: s -> EQClass a s -> s
repr' s EQNeutral = s
repr' _ EQClass {elements = e} = assert (size e > 0) $ fromWitness $ elemAt 0 e


eqClass :: (Ord s) => EQRel a s -> s -> EQClass a s
eqClass r@(EQRel ss eqr) s = EQClass {
    elements = filter (\(Witness (s', _)) -> s ~~ s' $ r) ss
  , relation = r }

(~~) :: (Ord s) => s -> s -> EQRel a s -> Bool
(~~) s s' eqr = (s, s') `member` classRelation eqr || (s', s) `member` classRelation eqr

instance (Ord s, Monoid s) => Monoid (EQClass a s) where
  mempty = EQNeutral
  mappend EQNeutral x = x
  mappend x EQNeutral = x
  mappend EQClass {elements = e1, relation = rel} EQClass{elements = e2, relation = rel'} =
    assert (rel == rel') $ eqClass rel $ mappend (fromWitness $ elemAt 0 e1) (fromWitness $ elemAt 0 e2)