{-# LANGUAGE GADTs, TypeApplications #-}
module ForestAlgebra where
import RoseTree

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
 
morphFromBeta :: (Monoid h, Monoid v, Monoid g, Monoid w) => ForestAlgebra h v -> ForestAlgebra g w -> (v -> w) -> Morph h v g w
morphFromBeta f1 f2 beta = MFA { α = alpha, β = beta } where
  alpha h = act f2 mempty (beta (inₗ f1 h)) 

morphFromFun :: (Monoid h, Monoid v) => ForestAlgebra h v -> (a -> v) -> Morph (Forest a) (Context a) h v
morphFromFun fa f = MFA { α = alpha, β = beta } where
  alpha (Forest ts) = foldMap alpha' ts

  alpha' (Lf a) = act fa (alpha (Forest [])) (f a)
  alpha' (Br a t) = act fa (alpha $ Forest t) (f a)

  beta (Context (Forest []) c (Forest [])) = beta' c
  beta (Context t1 c t2) = (inₗ fa (alpha t1)) `mappend` (inᵣ fa (alpha t2)) `mappend` (beta' c)

  beta' (CTree []) = mempty
  beta' (CTree ((f1, a, f2):cs)) = (f a) `mappend` beta (Context f1 (CTree cs) f2)

type FreeForestAlgebra a = ForestAlgebra (Forest a) (Context a)
freeForestAlgebra :: FreeForestAlgebra a
freeForestAlgebra = FA {
    act = \ forest contxt -> insertForest contxt forest
  , inₗ = \f -> Context f mempty mempty
  , inᵣ = \f -> Context mempty mempty f
}
