{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
module RoseTree where

-- |Unranked Trees AKA RoseTrees.
data RT a = Lf a | Br a [RT a] deriving (Show, Eq, Functor, Ord)

newtype Forest a = Forest { trees :: [RT a] }
    deriving (Eq,Functor)
instance Monoid (Forest a) where
  mempty = Forest []
  mappend (Forest f1) (Forest f2) = Forest (mappend f1 f2)


-- https://www.mimuw.edu.pl/~bojan/upload/confbirthdayBojanczykW08.pdf
-- page 2:
--
-- encoding see http://strictlypositive.org/diff.pdf
data Context a = Context { lForest :: Forest a
                         , cTree   :: CTree a
                         , rForest :: Forest a }
            deriving (Eq,Functor)
-- corresponds to lForest + cTree + rForest

newtype CTree a = CTree { unCTree :: [(Forest a, a, Forest a)] }
    deriving (Eq,Functor)
-- (l, n, r) corresponds to 
--    n
--   /|\
--  / | \
--  l ☐ r

-- |Insert a Forest into a Context.
insertForest :: Context a -> Forest a -> Forest a
insertForest c f = lForest c `mappend` appended `mappend` rForest c
    where
        ctree    = unCTree $ cTree c
        appended = Forest $ foldr (\(l,n,r) cs -> [Br n $ trees l ++ cs ++ trees r]) (trees f) ctree


instance Monoid (CTree a) where
    mempty = CTree []
    mappend (CTree ls) (CTree rs) = CTree $ ls ++ rs

instance Monoid (Context a) where
  mempty = Context mempty mempty mempty
  mappend c1 c2 = c1 { cTree = CTree $ appendC2 (unCTree $ cTree c1) }
    where
        appendC2 []        = unC2
        appendC2 [(l,n,r)] = (l `mappend` lForest c2, n, rForest c2 `mappend` r) : unC2
        appendC2 (x:xs)    = x : appendC2 xs

        unC2 = unCTree $ cTree c2

--  forestToContext :: Forest (Maybe a) -> Context a
--  forestToContext = Context . trees
  

foldRT :: (a -> b) -> (a -> [b] -> b) -> RT a -> b
foldRT fl _ (Lf l)     = fl l
foldRT fl fb (Br l rs) = fb l $ fmap (foldRT fl fb) rs
