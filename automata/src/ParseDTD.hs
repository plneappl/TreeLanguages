
module ParseDTD where

import Parser
import ParseRegExp

import DTD

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

