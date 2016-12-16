module Test where

chooseAll :: [[a]] -> [[a]]
chooseAll [] = []
chooseAll [xs] = [[x] | x <- xs]
chooseAll (xs:xss) = [x:y | x <- xs, y <- chooseAll xss]

main = print $ chooseAll [[1, 2], [3, 4], [5, 6]]