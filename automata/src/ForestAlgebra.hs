{-# LANGUAGE GADTs, TypeApplications #-}
module ForestAlgebra where
import RoseTree
import Alphabet
import Data.Functor.Identity

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

type FreeForestAlgebra a = ForestAlgebra (Forest a) (Context a)
freeForestAlgebra :: (Alphabet a) => FreeForestAlgebra a
freeForestAlgebra = FA {
    act = \ forest contxt -> insertForest contxt forest
  , inₗ = \f -> Context f mempty mempty
  , inᵣ = \f -> Context mempty mempty f
}
