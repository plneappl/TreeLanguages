{-# LANGUAGE FlexibleContexts #-}

module ParseForest where

import Text.Parsec
import Data.Set as DS

import Control.Monad

import Pretty
import RoseTree
import Forest

simpleTree :: Parsec String () (RT ())
simpleTree = do
    char '('
    ts <- many simpleTree
    char ')'
    return $ Br () ts

type Parser a = Parsec String () a
data Translator a = Translator { node :: Parser a, startMark :: Parser (), endMark :: Parser ()}

tree :: (Eq a,Show a) => Translator a -> Parsec String () (RT a)
tree trans = do
    l <- node trans
    f <- forest trans
    return $ case trees f of
                  [] -> Lf l
                  ts -> Br l ts

forest :: (Eq a,Show a) => Translator a -> Parsec String () (Forest a)
forest trans = do
    check <- fmap Right (node trans) <|> fmap Left (startMark trans)
    case check of
        Right l -> do
            f <- forest trans
            return $ case trees f of
                          [] -> Forest [Lf l]
                          ts -> Forest [Br l ts]
        Left _ -> do
            ts <- many $ tree trans
            endMark trans
            return $ Forest ts

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

testSimple :: String -> IO ()
testSimple = test simpleTree
testTree' :: String -> IO ()
testTree' = test tree'
testTree'' :: String -> IO ()
testTree'' = test tree''
test :: (Show a) => Parser (RT a) -> String -> IO ()
test p s = case parse p "" s of
        Right t -> putStrLn ("----- pattern: " ++ s ++ " -----") >> printRT t
        Left e -> putStr $ "failed at pattern " ++ s ++ " with error:\n" ++ show e
testForests :: (Show a) => Parser (Forest a) -> String -> IO ()
testForests p s = case parse p "" s of
        Right ts -> putStrLn ("----- pattern: " ++ s ++ " -----") >> printForest ts
        Left e -> putStr $ "failed at pattern " ++ s ++ " with error:\n" ++ show e
testForest' :: String -> IO ()
testForest' = testForests forest'

main :: IO ()
main = do
    testSimple "(()())"
    testSimple "((()(()))())"
    putStr "============ Trees =============\n"
    testTree' "ab(d()ab()c())"
    testTree'' "ab(d()ab()c())"
    testTree' "a(b()c()foobar())"
    testTree'' "a(b()c()foobar())"
    putStr "============ Forests =============\n"
    testForest' "(a()b())"
    testForest' "()"
    testForest' "(a()x()va())"
