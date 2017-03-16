{-| 
Module      : ForestAlgebra
Description : Contains the definition of 'ForestAlgebra's and things that are related to them.

The Module "ForestAlgebra" contains:
1. Definitions:
  - of the data type 'ForestAlgebra'
  - of the data type 'Morph', which is a Morphism between Forest Algebras
  - type aliases for Free Forest Algebras
2. Morphism constructors:
  - 'morphFromBeta', which infers α from β
  - 'morphFromFun', which infers a Morphism from the Free Forest Algebra according to a function mapping letters to a vertical monoid
3. Forest Algebra transformations to and from "DeterministicAutomaton"s
-}
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
-- |A Forest Algebra is defined over two Monoids @H@ and @V@. It has 3 functions @act, inₗ, inᵣ@.
data ForestAlgebra h v where 
  FA :: (Monoid h, Monoid v) => { 
    act :: h -> v -> h, 
    inₗ :: h -> v, 
    inᵣ :: h -> v 
  } -> ForestAlgebra h v

-- |A Morphism @(α, β)@ between ForestAlgebras.
data Morph h v g w where
  MFA :: (Monoid h, Monoid v, Monoid g, Monoid w) => {
    α :: h -> g,
    β :: v -> w
  } -> Morph h v g w

-- |Type synonym for Morphisms from Free Forest Algebras.
type MorphFFA a h v = Morph (Forest a) (Context a) h v

-- |Construct a Morphism only from @β@.
morphFromBeta :: (Monoid h, Monoid v, Monoid g, Monoid w) => ForestAlgebra h v -> ForestAlgebra g w -> (v -> w) -> Morph h v g w
morphFromBeta f1 f2 beta = MFA { α = alpha, β = beta } where
  alpha h = act f2 mempty (beta (inₗ f1 h)) 

-- |Construct a @MorphFFA@ from a function mapping letters to the vertical Monoid.
morphFromFun :: (Monoid h, Monoid v) => ForestAlgebra h v -> (a -> v) -> MorphFFA a h v
morphFromFun fa f = MFA { α = alpha, β = beta } where
  alpha (Forest ts) = foldMap alpha' ts

  alpha' (Lf a) = act fa (alpha (Forest [])) (f a)
  alpha' (Br a t) = act fa (alpha $ Forest t) (f a)

  beta (Context (Forest []) c (Forest [])) = beta' c
  beta (Context t1 c t2) = inₗ fa (alpha t1) `mappend` inᵣ fa (alpha t2) `mappend` beta' c

  beta' (CTree []) = mempty
  beta' (CTree ((f1, a, f2):cs)) = f a `mappend` beta (Context f1 (CTree cs) f2)

-- |Type synonym for Free Forest Algebras.
type FreeForestAlgebra a = ForestAlgebra (Forest a) (Context a)
-- |Given an alphabet @a@ we can construct the Free Forest Algebra on it.
freeForestAlgebra :: FreeForestAlgebra a
freeForestAlgebra = FA {
    act = flip insertForest
  , inₗ = \f -> Context f mempty mempty
  , inᵣ = \f -> Context mempty mempty f
}

-- http://www.labri.fr/perso/igw/Papers/igw-bordeaux07.pdf
-- |DTA (Deterministic Tree Automaton) → Forest Algebra. Also returns a @MorphFFA@ to the Forest Algebra.
fromDTA :: (Ord s) => DeterministicAutomaton s a -> (ForestAlgebra s (s -> s), MorphFFA a s (s -> s))
fromDTA (DA delta acc _) = (fa, morphFromFun fa delta) where
  fa = FA { act = actF, inₗ = in_l, inᵣ = in_r }
  actF = flip ($)
  in_l = mappend 
  in_r = flip mappend

-- The presentation says 
-- δ a h = act h (β a), but β :: Context a -> v.
-- So... construct a context out of a? and then believe that act produces a single tree?
-- |Forest Algebra → DTA. Needs an accepting Set of elements in @H@.
toDTA :: (Alphabet a) => States h -> MorphFFA a h v -> ForestAlgebra h v -> DS.Set h -> DeterministicAutomaton h a
toDTA s (MFA alpha beta) fa acc' = DA {
  delta = dt,
  states = s,
  acc = acc'
} where
  ffa = freeForestAlgebra
  dt a h = act fa h $ beta $ Context mempty (CTree [(mempty, a, mempty)]) mempty






