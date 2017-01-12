
module RegExp where

import Data.List

data RegExp a   = Empty
                | Unit
                | Singleton a
                | Star (RegExp a)
                | Union [RegExp a]
                | Concat [RegExp a]
                deriving (Eq)
                --  deriving (Eq,Show)

instance (Show a) => Show (RegExp a) where
    show Empty = "∅"
    show Unit  = "λ"
    show (Singleton s) = show s
    show (Star r) = "( " ++ show r ++ " )*"
    show (Union rs) = "[" ++ intercalate " | " (map show rs) ++ "]"
    show (Concat rs) = intercalate "." $ map show rs

