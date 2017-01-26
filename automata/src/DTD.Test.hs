
module DTD.Test where

import Parser
import DTD
import ParseRegExp
import Control.Monad
import GHC.IO.Encoding

data Chars = A | B | C | D | E | F | G | H | I | J | K | L | M
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
           deriving (Show,Eq,Enum)

char' :: Char -> Parser Chars
char' = (>>= (return . toChars)) . char
    where
        toChars = toEnum . (+ (-97)) . fromEnum

test :: String -> IO ()
test s = putStr $ case parse (regExp (void $ char 'e') (char 'a') >>= \r -> eof >> return r) "" s of
                      Left e -> "----------Failure in pattern " ++ s ++ ":\n" ++ show e ++ "\n"
                      Right r -> "++++++++++Pattern " ++ s ++ " yields:\n" ++ show r ++ "\n"

test' :: String -> IO ()
test' s = putStr $ case parse (regExp (void $ char' 'e') (foldl1 (<|>) $ map char' $ filter (/='e') ['a'..'z']) >>= \r -> eof >> return r) "" s of
                      Left e -> "----------Failure in pattern " ++ s ++ ":\n" ++ show e ++ "\n"
                      Right r -> "++++++++++Pattern " ++ s ++ " yields:\n" ++ show r ++ "\n"

cmpTest :: String -> RegExp Char -> Bool
cmpTest s ri = case parse (regExp (void $ char 'e') (char 'a')) "" s of
                Left e -> False
                Right rn -> rn == ri

cmpTest' :: String -> RegExp Chars -> Bool
cmpTest' s ri = case parse (regExp (void $ char' 'e') (foldl1 (<|>) $ map char' $ filter (/='e') ['a'..'z'])) "" s of
                Left e -> False
                Right rn -> rn == ri
main :: IO ()
main = do
    setLocaleEncoding utf8
    test' ""
    print $ cmpTest' "" Empty
    test' "e"
    print $ cmpTest' "e" Unit
    test' "aa"
    print $ cmpTest' "aa" $ Concat [Singleton A, Singleton A]
    test' "a*a"
    print $ cmpTest' "a*a" $ Concat [Star $ Singleton A, Singleton A]
    test' "(aa)*a"
    print $ cmpTest' "(aa)*a" $ Concat [Star $ Concat [Singleton A, Singleton A], Singleton A]
    test' "(a|aa)a"
    print $ cmpTest' "(a|aa)a" $ Concat [Union [Singleton A, Concat [Singleton A, Singleton A]], Singleton A]
    test' "((a|aa))*a"
    print $ cmpTest' "((a|aa))*a" $ Concat [Star $ Union [Singleton A, Concat [Singleton A, Singleton A]], Singleton A]
    test' "(a|a|ae|a)"
    print $ cmpTest' "(a|a|ae|a)" $ Union [Singleton A, Singleton A, Concat [Singleton A, Unit], Singleton A]
    test' "(a|a|a*e|a)"
    print $ cmpTest' "(a|a|a*e|a)" $ Union [Singleton A, Singleton A, Concat [Star $ Singleton A, Unit], Singleton A]
    test' "(a|a|(a)*e|a)"
    print $ cmpTest' "(a|a|(a)*e|a)" $ Union [Singleton A, Singleton A, Concat [Star $ Singleton A, Unit], Singleton A]
    test' "(a|a|(ae)*e|a)"
    print $ cmpTest' "(a|a|(ae)*e|a)" $ Union [Singleton A, Singleton A, Concat [Star $ Concat [Singleton A, Unit], Unit], Singleton A]
    test' "e*abcez"
    --  this fails, but is an extremely special case
    --  print $ cmpTest' "e*abcez" $ Concat [Star Unit, Singleton A, Singleton B, Singleton C, Unit, Singleton Z]
    --  moreover, it is not important, it produces this instead, which is
    --  fine:
    print $ cmpTest' "e*abcez" $ Concat [Star Unit, Concat [Singleton A, Singleton B, Singleton C, Unit, Singleton Z]]
    test' "(a|aa)"
    print $ cmpTest' "(a|aa)" $ Union [Singleton A, Concat [Singleton A, Singleton A]]
    test' "a(a|a)"
    print $ cmpTest' "a(a|a)" $ Concat [Singleton A, Union [Singleton A, Singleton A]]
    test' "(a|a)a"
    print $ cmpTest' "(a|a)a" $ Concat [Union [Singleton A, Singleton A], Singleton A]
    test' "a(a|a)a"
    print $ cmpTest' "a(a|a)a" $ Concat [Singleton A, Union [Singleton A, Singleton A], Singleton A]
    test' "(((b)|a)a)"
    print $ cmpTest' "(((b)|a)a)" $ Concat [Union [Singleton B, Singleton A], Singleton A]
    test' "(a|a|(a(a|a|a)|e)e|a)"
    print $ cmpTest' "(a|a|(a(a|a|a)|e)e|a)" $ Union [Singleton A, Singleton A, Concat [Union [Concat [Singleton A, Union [Singleton A, Singleton A, Singleton A]], Unit], Unit], Singleton A]
