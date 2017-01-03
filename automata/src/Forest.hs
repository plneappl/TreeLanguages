module Forest where
import RoseTree

-- https://www.mimuw.edu.pl/~bojan/upload/confbirthdayBojanczykW08.pdf
-- page 2:
newtype Forest a = Forest { trees:: [RT a]}
instance Monoid (Forest a) where
  mempty = Forest []
  mappend (Forest f1) (Forest f2) = Forest (mappend f1 f2)

