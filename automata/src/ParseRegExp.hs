
module ParseRegExp
    ( module ParseRegExp
    , module RegExp
    ) where

import RegExp

import Parser

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
