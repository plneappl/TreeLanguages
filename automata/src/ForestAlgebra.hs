{-# LANGUAGE GADTs #-}
module ForestAlgebra where
import RoseTree
import Forest
import Alphabet

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

insertHoleToRootRight :: Context a -> Context a
insertHoleToRootRight (Lf a) = Br a [Lf Nothing]
insertHoleToRootRight (Br a bs) = Br a (bs ++ [Lf Nothing])

insertHoleToRootLeft :: Context a -> Context a
insertHoleToRootLeft (Lf a) = Br a [Lf Nothing]
insertHoleToRootLeft (Br a bs) = Br a (Lf Nothing:bs)

type FreeForestAlgebra a = ForestAlgebra (Forest a) (Context a)
freeForestAlgebra :: (Alphabet a) => FreeForestAlgebra a
freeForestAlgebra = FA {
  act = \ (Forest trees) context -> Forest $ map (insertTree context) trees,
  inₗ = \(Forest trees) -> mconcat $ map (insertHoleToRootRight . rtToContext) trees,
  inᵣ = \(Forest trees) -> mconcat $ map (insertHoleToRootLeft . rtToContext) trees
}