
module DTD where

import RegExp

data DTD a = DTD{ root :: a
                , rules :: a -> RegExp a
                }

