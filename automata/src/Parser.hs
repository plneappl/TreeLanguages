
module Parser
    ( module Parser
    , module Text.Parsec
    )  where

import Text.Parsec hiding (Empty)

type Parser a = Parsec String () a
