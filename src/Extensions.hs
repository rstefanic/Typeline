module Extensions where

import Control.Applicative ((<*), (*>), (<|>))
import Text.Parsec
       ( parse
       , try
       , many1
       , char
       , spaces
       , optional
       , noneOf
       , oneOf
       , digit
       , letter
       , string
       , sepBy1
       , anyChar
       )
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)

bg :: Parser String
bg = string "bg" >> "background"
