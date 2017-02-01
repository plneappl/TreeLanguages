module FiniteFunctions where


type FunL a b = [(a, b)]
type EndoFunL a = FunL a a

combine :: Eq b => [FunL a b] -> [FunL b c] -> [FunL a c]
combine = pairsWith chain

chain :: Eq b => FunL a b -> FunL b c -> FunL a c
chain f1 f2 = map (second (appl f2)) f1

plus :: (Eq s) => EndoFunL s -> [EndoFunL (EndoFunL s)] -> EndoFunL s -> [EndoFunL (EndoFunL s)]
plus h1 vs h2 = map (\v -> map (\(x, vx) -> (x, h1 `chain` vx `chain` h2)) v) vs 

appl :: Eq a => FunL a b -> a -> b
appl ((a, b):fs) a' | a == a' = b
                    | otherwise = appl fs a'

finiteFunToTuples :: [a] -> (a -> b) -> FunL a b
finiteFunToTuples as f = map (\a -> (a, f a)) as