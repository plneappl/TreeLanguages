{-# LANGUAGE GADTs, TypeApplications #-}
module ForestAlgebra where
import RoseTree
import DeterministicAutomaton
import qualified Data.Set as DS
import States
import Alphabet
import Lib
import Control.Arrow (second)

-- https://www.mimuw.edu.pl/~bojan/upload/confbirthdayBojanczykW08.pdf
-- section 3:
data ForestAlgebra h v where 
  FA :: (Monoid h, Monoid v) => { 
    act :: h -> v -> h, 
    inₗ :: h -> v, 
    inᵣ :: h -> v 
  } -> ForestAlgebra h v

data Morph h v g w where
  MFA :: (Monoid h, Monoid v, Monoid g, Monoid w) => {
    α :: h -> g,
    β :: v -> w
  } -> Morph h v g w
 
type MorphFFA a h v = Morph (Forest a) (Context a) h v

morphFromBeta :: (Monoid h, Monoid v, Monoid g, Monoid w) => ForestAlgebra h v -> ForestAlgebra g w -> (v -> w) -> Morph h v g w
morphFromBeta f1 f2 beta = MFA { α = alpha, β = beta } where
  alpha h = act f2 mempty (beta (inₗ f1 h)) 

morphFromFun :: (Monoid h, Monoid v) => ForestAlgebra h v -> (a -> v) -> MorphFFA a h v
morphFromFun fa f = MFA { α = alpha, β = beta } where
  alpha (Forest ts) = foldMap alpha' ts

  alpha' (Lf a) = act fa (alpha (Forest [])) (f a)
  alpha' (Br a t) = act fa (alpha $ Forest t) (f a)

  beta (Context (Forest []) c (Forest [])) = beta' c
  beta (Context t1 c t2) = inₗ fa (alpha t1) `mappend` inᵣ fa (alpha t2) `mappend` beta' c

  beta' (CTree []) = mempty
  beta' (CTree ((f1, a, f2):cs)) = f a `mappend` beta (Context f1 (CTree cs) f2)

type FreeForestAlgebra a = ForestAlgebra (Forest a) (Context a)
freeForestAlgebra :: FreeForestAlgebra a
freeForestAlgebra = FA {
    act = flip insertForest
  , inₗ = \f -> Context f mempty mempty
  , inᵣ = \f -> Context mempty mempty f
}

-- http://www.labri.fr/perso/igw/Papers/igw-bordeaux07.pdf

fromDTA :: (Ord s) => DeterministicAutomaton s a -> (ForestAlgebra s (s -> s), MorphFFA a s (s -> s))
fromDTA (DA delta acc _) = (fa, morphFromFun fa delta) where
  fa = FA { act = actF, inₗ = in_l, inᵣ = in_r }
  actF = flip ($)
  in_l = mappend 
  in_r = flip mappend

-- The presentation says 
-- δ a h = act h (β a), but β :: Context a -> v.
-- So... construct a context out of a? and then believe that act produces a single tree?

toDTA :: (Alphabet a) => States h -> MorphFFA a h v -> ForestAlgebra h v -> DS.Set h -> DeterministicAutomaton h a
toDTA s (MFA alpha beta) fa acc' = DA {
  delta = dt,
  states = s,
  acc = acc'
} where
  ffa = freeForestAlgebra
  dt a h = act fa h $ beta $ Context mempty (CTree [(mempty, a, mempty)]) mempty






