{-# LANGUAGE FlexibleContexts #-}

module ParseForest where

import Data.Tree
import Text.Parsec
import Data.Set as DS

import Control.Monad

import Data.Tree.Pretty

simpleTree :: Parsec String () (Tree ())
simpleTree = do
    char '('
    ts <- many simpleTree
    char ')'
    return $ Node () ts

type Parser a = Parsec String () a
data Translator a = Translator { node :: Parser a, startMark :: Parser (), endMark :: Parser ()}

tree :: (Eq a,Show a) => Translator a -> Parsec String () (Tree a)
tree trans = do
    l <- node trans
    f <- forest trans
    return $ Node l f

forest :: (Eq a,Show a) => Translator a -> Parsec String () (Forest a)
forest trans = do
    check <- fmap Right (node trans) <|> fmap Left (startMark trans)
    case check of
        Right l -> do
            f <- forest trans
            return [Node l f]
        Left _ -> do
            ts <- many $ tree trans
            endMark trans
            return ts

letters :: Translator Char
letters = Translator letter (void (char '(')) (void (char ')'))

forest' = forest letters
tree' = tree letters

data SomeLetters = A | B | C | D | E | F | G | H | I | J | K
                 | L | M | N | O | P | Q | R | S | T | U | V
                 | W | X | Y | Z deriving (Eq,Show,Enum,Ord)
someLetterChars :: Parsec String () SomeLetters
someLetterChars = do
    c <- choice $ fmap char ['a'..'z']
    let pairs = zip ['a'..'z'] [A .. Z]
    return $ snd $ head $ Prelude.filter ((c ==) . fst) pairs
someLetterStart = void $ char '('
someLetterEnd = void $ char ')'
someLetters :: Translator SomeLetters
someLetters = Translator someLetterChars  someLetterStart someLetterEnd

tree'' = tree someLetters
forest'' = forest someLetters

main :: IO ()
main = do
    case parse simpleTree "" "(()())" of
        Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "-----\n"
    case parse simpleTree "" "((()(()))())" of
        Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "Trees:\n"
    case parse tree' "" "ab(d()ab()c())" of
        Right t -> putStr $ drawVerticalTree (fmap show t) 
    case parse tree'' "" "ab(d()ab()c())" of
        Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "-----\n"
    case parse tree' "" "a(b()c()foobar())" of
        Right t -> putStr $ drawVerticalTree (fmap show t) 
    case parse tree'' "" "a(b()c()foobar())" of
        Right t -> putStr $ drawVerticalTree (fmap show t) 
    putStr "Forests:\n"
    case parse forest' "" "(a()b())" of
        Right ts -> mapM_ putStr $ fmap (drawVerticalTree . fmap show) ts
    putStr "-----\n"
    case parse forest' "" "()" of
        Right ts -> mapM_ putStr $ fmap (drawVerticalTree . fmap show) ts
    putStr "-----\n"
    case parse forest' "" "(a()x()v())" of
        Right ts -> mapM_ putStr $ fmap (drawVerticalTree . fmap show) ts
