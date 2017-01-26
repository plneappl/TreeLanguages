
import Prelude hiding (Word)
import States
import Alphabet
import WordAutomaton
import WordDFA
import qualified EpsWordNFA as Eps

import ParseRegExp
import Parser
import Control.Monad

import qualified Data.Set as DS

import Lib

data Sts = Z | O | Tw | Thr | F
    deriving (Show,Eq,Ord,Enum)
instance States Sts where
    allStates = DS.fromList [Z .. F]

data Alph = AZ | AO
    deriving (Show,Eq,Ord,Enum)
instance Alphabet Alph where
    allLetters = [AZ, AO]

-- w \in L <=> w \equiv 2 mod 5
da :: WordDFA Sts Alph
da = WordDFA { acc = DS.singleton Tw, delta = d, start = Z, states = allStates}
    where
        d AZ Z   = Z
        d AO Z   = O
        d AZ O   = Tw
        d AO O   = Thr
        d AZ Tw  = F
        d AO Tw  = Z
        d AZ Thr = O
        d AO Thr = Tw
        d AZ F   = Thr
        d AO F   = F

parseAlphString :: String -> Word Alph
parseAlphString = fmap toAlph
    where toAlph '0' = AZ
          toAlph '1' = AO
          toAlph _   = error "Input must consist of 0 and 1"

--      _0_
--      |  |
--       \/
-- o--1--o--0--o
-- \__ε_/

ena :: Eps.EpsWordNFA Sts Alph
ena = Eps.EpsWNFA { Eps.delta = d, Eps.epsDelta = ed, Eps.start = DS.singleton Z, Eps.acc = DS.singleton Tw, Eps.states = allStates }
    where
        d AO Z = DS.fromList [O]
        d AZ O = DS.fromList [O,Tw]
        d _  _ = DS.empty
        --  ed Z = DS.singleton O
        --  ed Z = DS.fromList [O,Tw]
        ed Z = DS.fromList [O]
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
da'' = WordDFA { delta=d, start=s, acc=a, states=allStates }
   where
      s = Z
      a = DS.fromList [ Tw, F ]
      d AZ Z   = Thr
      d AO Z   = O
      d AZ O   = Tw
      d AO O   = Tw
      d AZ Tw  = Tw
      d AO Tw  = O
      d AZ Thr = F
      d AO Thr = F
      d AZ F   = F
      d AO F   = Thr

--           v------1----\
-- o----1--->o------*--->o
ma'' :: WordDFA (DS.Set Sts) Alph
ma'' = minimize da''

allWords :: [String]
allWords = "" : concatMap (\s -> ['0':s, '1':s]) allWords

------------------------
---- using regExp ------
------------------------

rParser :: Parser (RegExp Alph)
rParser = regExp (void $ char 'e') (choice [ char '0' >> return AZ, char '1' >> return AO ])

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left  _) = error "this should be 'Right'"


r = fromRight $ parse rParser "" "(01|1*10|e)"
rna = Eps.fromRegExp r
rda = determinize rna
rma = minimize rda

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

main :: IO ()
main = do
    mapM_ (\(s,d,m) -> putStrLn $ s ++ ":\t" ++ d ++ "\t" ++ m) $ take 40 $ fmap (\s -> let w = parseAlphString s in (s, "DFA: " ++ show (automatonAccepts da'' w), "MDFA: " ++ show (automatonAccepts ma'' w))) allWords
    mapM_ (\(s,n,d,m) -> putStrLn $ s ++ ":\t" ++ n ++ "\t" ++ d ++ "\t" ++ m) $ take 40 $ fmap (\s -> let w = parseAlphString s in (s, "DFA: " ++ show (automatonAccepts ena w), "DFA: " ++ show (automatonAccepts da' w), "MDFA: " ++ show (automatonAccepts ma' w))) allWords