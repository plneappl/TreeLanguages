
module ParseRegExp
    ( module ParseRegExp
    , module RegExp
    ) where

import RegExp

import Parser

regExp :: Parser () -> Parser a -> Parser (RegExp a)
regExp unit singleton = do
  let empty  = return Empty
      unit'  = unit >> return Unit
      sngl   = fmap Singleton singleton
      re     = regExp unit singleton
      rParen = between (char '(')
                  (char ')')
                  re
      star   = do
         s <- choice [sngl, unit', rParen]
         char '*'
         return $ Star s
      union  = do
         rs <- sepBy
                  (choice $ fmap try [conc, star, sngl, unit', rParen])
                  (char '|' <|> char '+' <|> char '∪')
         case rs of
               []  -> unexpected "empty union"
               --  [e] -> return e
               [e] -> unexpected "singleton union"
               _   -> return $ Union rs
      conc   = do
         e <- choice $ fmap try [star, unit', sngl, rParen]
         es <- many1 $ choice $ fmap try [star, unit', sngl, rParen]
         case es of
               [] -> unexpected "concat too short"
               _  -> return $ Concat (e:es)
  choice $ fmap try [union, conc, star, unit', sngl, rParen, empty]
  --  choice $ fmap try [conc, unit', sngl, empty]

