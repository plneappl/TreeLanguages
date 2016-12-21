{-# LANGUAGE FlexibleContexts #-}

module ParseForest where

import Data.Tree
import Text.Parsec

import Data.Tree.Pretty

simpleTree :: Parsec String () (Tree ())
simpleTree = do
    char '('
    ts <- many simpleTree
    char ')'
    return $ Node () ts


parseTree :: Parsec String () (Tree String)
parseTree = do
    l <- letter
    f <- parseForest
    return $ Node [l] f

parseForest :: Parsec String () (Forest String)
parseForest = do
    let isLabel c = c `elem` ['a'..'z'] ++ ['A'..'Z']
    l <- letter <|> char '('
    if isLabel l
    then do
        f <- parseForest
        return $ [Node [l] f]
    else do
        ts <- many parseTree
        char ')'
        return ts

main :: IO ()
main = do
    case (parse simpleTree "" "(()())") of Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "-----\n"
    case (parse simpleTree "" "((()(()))())") of Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "Trees:\n"
    case (parse parseTree "" "ab(d()ab()c())") of Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "-----\n"
    case (parse parseTree "" "a(b()c()foobar())") of Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "Forest:\n"
    case (parse parseForest "" "(a()b())") of Right ts -> mapM_ putStr $ map (\t -> drawVerticalTree (fmap show t)) ts
    putStr "-----\n"
    case (parse parseForest "" "()") of Right ts -> mapM_ putStr $ map (\t -> drawVerticalTree (fmap show t)) ts
    putStr "-----\n"
    case (parse parseForest "" "(a()x()v())") of Right ts -> mapM_ putStr $ map (\t -> drawVerticalTree (fmap show t)) ts
