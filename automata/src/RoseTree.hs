{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module RoseTree where


data RT a = Lf a | Br a [RT a] deriving (Show, Eq, Functor, Ord)

newtype Forest a = Forest { trees :: [RT a] }
instance Monoid (Forest a) where
  mempty = Forest []
  mappend (Forest f1) (Forest f2) = Forest (mappend f1 f2)


-- https://www.mimuw.edu.pl/~bojan/upload/confbirthdayBojanczykW08.pdf
-- page 2:

newtype Context a = Context [RT (Maybe a)]

rtToContext :: RT a -> Context a
rtToContext = Context . (:[]) . fmap Just

insertTrees :: Monad m => Context a -> [RT (m a)] -> Forest (m a)
insertTrees (Context cs) t = Forest $ concatMap insTree cs where
  insTree (Lf (Just l)) = [Lf $ return l]
  insTree (Br (Just l) cs') = [Br (return l) $ insertIntoContext cs']
  insTree (Lf Nothing) = t
  insTree (Br Nothing _) = undefined

  insertIntoContext (Lf (Just l):cs1) = Lf (return l) : insertIntoContext cs1
  insertIntoContext (Lf Nothing:cs1) = t ++ insertIntoContext cs1
  insertIntoContext [] = []
  insertIntoContext (Br (Just l) cs2:cs1) = Br (return l) (insertIntoContext cs2) : insertIntoContext cs1
  insertIntoContext (Br Nothing _:_) = undefined



instance Monoid (Context a) where
  mempty = Context [Lf Nothing]
  mappend c1 (Context c2) = forestToContext $ insertTrees c1 c2

forestToContext :: Forest (Maybe a) -> Context a
forestToContext = Context . trees
  

foldRT :: (a -> b) -> (a -> [b] -> b) -> RT a -> b
foldRT fl _ (Lf l)     = fl l
foldRT fl fb (Br l rs) = fb l $ fmap (foldRT fl fb) rs
