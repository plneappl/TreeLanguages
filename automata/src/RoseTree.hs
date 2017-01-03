{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module RoseTree where

data RT a = Lf a | Br a [RT a] deriving (Show, Eq, Functor, Ord)

-- https://www.mimuw.edu.pl/~bojan/upload/confbirthdayBojanczykW08.pdf
-- page 2:

type Context a = RT (Maybe a)

rtToContext :: RT a -> Context a
rtToContext = fmap Just

insertTree :: Context a -> RT a -> RT a
insertTree c t = case c of
  Lf Nothing -> t
  Lf (Just l) -> Lf l
  Br (Just l) cs -> Br l (map (`insertTree` t) cs)
  Br Nothing _ -> undefined


instance Monoid (Context a) where
  mempty = Lf Nothing
  mappend c1 c2 = case c1 of
    Lf Nothing -> c2
    l@(Lf (Just _)) -> l
    (Br a c1') -> Br a (map (mappend c2) c1')

foldRT :: (a -> b) -> (a -> [b] -> b) -> RT a -> b
foldRT fl _ (Lf l)     = fl l
foldRT fl fb (Br l rs) = fb l $ fmap (foldRT fl fb) rs
