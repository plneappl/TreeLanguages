
import Prelude hiding (Word)
import States
import Alphabet
import WordAutomaton
import WordDFA
import qualified EpsWordNFA as Eps

import qualified TransMonoid as TR

import ParseRegExp
import Parser
import Control.Monad

import qualified Data.Set as DS

import Data.Foldable (foldMap)

import Lib

data Sts = SZ | SO | Tw | Thr | SF
    deriving (Show,Eq,Ord,Enum)
_States_Sts :: States Sts
_States_Sts = States $ DS.fromList [SZ .. SF]

data Alph = AZ | AO
    deriving (Show,Eq,Ord,Enum)
instance Alphabet Alph where
    allLetters = DS.fromList [AZ, AO]

data Chars = A | B | C | D | E | F | G | H | I | J | K | L | M
           | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
           deriving (Show,Ord,Eq,Enum)
instance Alphabet Chars where
    allLetters = DS.fromList [ A .. Z ]

-- w \in L <=> w \equiv 2 mod 5
da :: WordDFA Sts Alph
da = WordDFA { acc = DS.singleton Tw, delta = d, start = SZ, states = allStates _States_Sts }
    where
        d AZ SZ  = SZ
        d AO SZ  = SO
        d AZ SO  = Tw
        d AO SO  = Thr
        d AZ Tw  = SF
        d AO Tw  = SZ
        d AZ Thr = SO
        d AO Thr = Tw
        d AZ SF   = Thr
        d AO SF   = SF

parseAlphString :: String -> Word Alph
parseAlphString = fmap toAlph
    where toAlph '0' = AZ
          toAlph '1' = AO
          toAlph _   = error "Input must consist of 0 and 1"

toChars :: Char -> Chars
toChars = toEnum . (+ (-97)) . fromEnum

char' :: Char -> Parser Chars
char' = (>>= (return . toChars)) . char

parseCharsString :: String -> Word Chars
parseCharsString = fmap toChars

--      _0_
--      |  |
--       \/
-- o--1--o--0--o
-- \__ε_/

ena :: Eps.EpsWordNFA Sts Alph
ena = Eps.EpsWNFA { Eps.delta = d, Eps.epsDelta = ed, Eps.start = DS.singleton SZ, Eps.acc = DS.singleton Tw, Eps.states = allStates _States_Sts }
    where
        d AO SZ = DS.fromList [SO]
        d AZ SO = DS.fromList [SO,Tw]
        d _  _ = DS.empty
        --  ed Z = DS.singleton O
        --  ed Z = DS.fromList [O,Tw]
        ed SZ = DS.fromList [SO]
        --  ed O = DS.fromList [Z, O, Tw]
        ed _ = DS.empty

da' :: WordDFA (DS.Set Sts) Alph
da' = determinize ena

ma' :: WordDFA (DS.Set (DS.Set Sts)) Alph
ma' = minimize da'

--           v------1----\
-- o----1--->o------*--->o
--  \___0___>o------*--->o
--           ^------1----/
da'' :: WordDFA Sts Alph
da'' = WordDFA { delta=d, start=s, acc=a, states = allStates _States_Sts }
   where
      s = SZ
      a = DS.fromList [ Tw, SF ]
      d AZ SZ   = Thr
      d AO SZ   = SO
      d AZ SO   = Tw
      d AO SO   = Tw
      d AZ Tw  = Tw
      d AO Tw  = SO
      d AZ Thr = SF
      d AO Thr = SF
      d AZ SF   = SF
      d AO SF   = Thr

--           v------1----\
-- o----1--->o------*--->o
ma'' :: WordDFA (DS.Set Sts) Alph
ma'' = minimize da''

allWords :: [String]
allWords = "" : concatMap (\s -> ['0':s, '1':s]) allWords

allWords' :: [String]
allWords' = "" : concatMap (\s -> fmap (:s) ['a' .. 'z']) allWords'

------------------------
---- using regExp ------
------------------------

rParser :: Parser (RegExp Alph)
rParser = do
    r <- regExp (void $ char 'e') (choice [ char '0' >> return AZ, char '1' >> return AO ])
    eof
    return r

rParser' :: Parser (RegExp Chars)
rParser' = do
    r <- regExp (void $ char '0') (choice $ fmap char' ['a' .. 'z'])
    eof
    return r

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left  _) = error "this should be 'Right'"


r = fromRight $ parse rParser "" "(01|1*10|e)"
rna = Eps.fromRegExp r
rda = determinize rna
rma = minimize rda

--  trans :: FullTransMonoid 
trans = TR.transMonoid rma

morph :: [ Alph ] -> TR.TransMonoid (DS.Set (DS.Set Eps.CountableState)) Alph
morph [] = TR.zero trans
morph as = foldMap simpleFunc as
   where
      simpleFunc a = TR.TM { TR.aut = rma, TR.dom = fmap (delta rma a) (TR.dom $ TR.zero trans), TR.trans = delta rma a }

allFAs :: (Alphabet a, Eq a) => RegExp a -> (Eps.EpsWordNFA Eps.CountableState a, WordDFA (DS.Set Eps.CountableState) a, WordDFA (DS.Set (DS.Set Eps.CountableState)) a)
allFAs r = (na, da, ma)
    where
        na = Eps.fromRegExp r
        da = determinize na
        ma = minimize da

runAll ::  (Alphabet a, Eq a) => Word a -> (Eps.EpsWordNFA Eps.CountableState a, WordDFA (DS.Set Eps.CountableState) a, WordDFA (DS.Set (DS.Set Eps.CountableState)) a) -> (Bool, Bool, Bool)
runAll w (na,da,ma) = (automatonAccepts na w, automatonAccepts da w, automatonAccepts ma w)

checkAll :: [String] -> String -> IO ()
checkAll ss r = print re >> mapM_ checkSingle ss
    where
        re            = fromRight $ parse rParser "" r
        (na, da, ma) = allFAs re
        checkSingle s = let w               = parseAlphString s
                            (nab, dab, mab) = runAll w (na, da, ma)
                        in putStrLn $ s ++ ":\t" ++ "NFA: " ++ show nab ++ "\tDFA: " ++ show dab ++ "\tMDFA: " ++ show mab

checkAll' :: [String] -> String -> IO ()
checkAll' ss r = print re >> mapM_ checkSingle ss
    where
        re            = fromRight $ parse rParser' "" r
        (na, da, ma) = allFAs re
        checkSingle s = let w               = parseCharsString s
                            (nab, dab, mab) = runAll w (na, da, ma)
                        in putStrLn $ s ++ ":\t" ++ "NFA: " ++ show nab ++ "\tDFA: " ++ show dab ++ "\tMDFA: " ++ show mab

main :: IO ()
main = do
    mapM_ (\(s,d,m) -> putStrLn $ s ++ ":\t" ++ d ++ "\t" ++ m) $ take 40 $ fmap (\s -> let w = parseAlphString s in (s, "DFA: " ++ show (automatonAccepts da'' w), "MDFA: " ++ show (automatonAccepts ma'' w))) allWords
    mapM_ (\(s,n,d,m) -> putStrLn $ s ++ ":\t" ++ n ++ "\t" ++ d ++ "\t" ++ m) $ take 40 $ fmap (\s -> let w = parseAlphString s in (s, "DFA: " ++ show (automatonAccepts ena w), "DFA: " ++ show (automatonAccepts da' w), "MDFA: " ++ show (automatonAccepts ma' w))) allWords
