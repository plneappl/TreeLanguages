
module ParseLanguages where

import Data.Tree
import Text.Parsec hiding (Empty)
import GHC.IO.Encoding

import Data.List
import Control.Monad

type Parser a = Parsec String () a

data RegExp a   = Empty
                | Unit
                | Singleton a
                | Star (RegExp a)
                | Union [RegExp a]
                | Concat [RegExp a]
                deriving (Eq)

instance (Show a) => Show (RegExp a) where
    show Empty = "∅"
    show Unit  = "λ"
    show (Singleton s) = show s
    show (Star r) = "( " ++ show r ++ " )*"
    show (Union rs) = intercalate " | " $ map show rs
    show (Concat rs) = concatMap show rs

data DTD a = DTD{ root :: a
                , rules :: a -> RegExp a
                }

regExp :: Parser () ->  Parser a -> Parser (RegExp a)
regExp unit singleton = do
    let empty = eof >> return Empty
        unit' = unit >> return Unit
        sngl  = fmap Singleton singleton
        re    = regExp unit singleton
        starp = do
            e <- between (char '(')
                    (string ")*")
                    re
            return $ Star e
        stars = do
            s <- sngl <|> unit'
            char '*'
            return $ Star s
        star = starp <|> stars
        union = do
            let unionnp = sepBy re (char '|' <|> char '+' <|> char '∪')
            es <- between (char '(') (char ')') unionnp
            case es of
                []  -> unexpected "empty union"
                [e] -> unexpected "unary union"
                _   -> return $ Union es
        conc  = do
            e  <- choice $ fmap try [union, star, unit', sngl]
            es <- many $ choice $ fmap try [union, star, unit', sngl]
            case es of
                [] -> return e
                _  -> return $ Concat (e:es)
    choice $ fmap try [union, conc, star, unit', sngl, empty]
        
test :: String -> IO ()
test s = print $ parse (regExp (void $ char 'e') (char 'a')) "" s
    
main :: IO ()
main = do
    setLocaleEncoding utf8
    test ""
    test "e"
    test "aa"
    test "a*a"
    test "(aa)*a"
    test "(a|aa)a"
    test "((a|aa))*a"
    test "(a|a|ae|a)"
    test "(a|a|a*e|a)"
    test "(a|a|(ae)*e|a)"
    test "(a|a|(a|e)e|a)"
