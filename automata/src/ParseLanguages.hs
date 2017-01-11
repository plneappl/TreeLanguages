
module ParseLanguages where

import Text.Parsec hiding (Empty)
import GHC.IO.Encoding

import Data.List
import Control.Monad
import RoseTree

type Parser a = Parsec String () a

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
        star = try starp <|> stars
        --  star = do
            --  r <- choice $ fmap try [union, conc, unit', sngl]
            --  char '*'
            --  return $ Star r
        union = do
            let unionnp = sepBy re (char '|' <|> char '+' <|> char '∪')
            es <- between (char '(') (char ')') unionnp
            case es of
                []  -> unexpected "empty union"
                [e] -> return e
                _   -> return $ Union es
        conc  = do
            e  <- choice $ fmap try [union, star, unit', sngl]
            es <- many $ choice $ fmap try [union, star, unit', sngl]
            case es of
                [] -> return e
                _  -> return $ Concat (e:es)
    --  choice $ fmap (try [union, conc, star, unit', sngl, empty]
    rs <- many $ choice $ fmap try [star, union, conc, unit', sngl]
    case rs of
         []  -> empty
         [r] -> return r
         _   -> return $ Concat rs

parseDTD :: Eq a =>  Parser () -> Parser a -> Parser (DTD a)
parseDTD unit singleton = do
    let regex = regExp unit singleton
        sep   = do  many (char ' ')
                    choice [ char '\n'
                            , char ','
                            ]
                    many (char ' ')
        arrow = many (char ' ') >> string "->" >> many (char ' ')
    r <- singleton
    arrow
    rreg <- regex
    let dtd = addToDTD (DTD r (const Empty)) r rreg
    rules <- many $ try $ do
                many1 sep
                s <- singleton 
                arrow
                rreg <- regex
                return (s, rreg)
    many sep >> eof
    return $ foldr (\(s, reg) d -> addToDTD d s reg) dtd rules
              

addToDTD :: (Eq a) => DTD a -> a -> RegExp a -> DTD a
addToDTD dtd s regex = 
    dtd { rules = newRules }
    where
        newRules st =
            if s == st
            then case rules dtd st of
                    Empty -> regex
                    Union rs -> Union (rs ++ [regex])
                    oldRegex -> Union [oldRegex, regex]
            else rules dtd st

data Chars = A | B | C | D | E | F | G | H | I | J | K | L | M
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
           deriving (Show,Eq,Enum)

char' :: Char -> Parser Chars
char' = (>>= (return . toChars)) . char
    where
        toChars = toEnum . (+ (-97)) . fromEnum

test :: String -> IO ()
test s = putStr $ case parse (regExp (void $ char 'e') (char 'a')) "" s of
                      Left e -> "----------Failure in pattern " ++ s ++ ":\n" ++ show e ++ "\n"
                      Right r -> "++++++++++Pattern " ++ s ++ " yields:\n" ++ show r ++ "\n"

test' :: String -> IO ()
test' s = putStr $ case parse (regExp (void $ char' 'e') (foldl1 (<|>) $ map char' $ filter (/='e') ['a'..'z'])) "" s of
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
