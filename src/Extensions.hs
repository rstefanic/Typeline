module Extensions where

import Control.Applicative ((<*), (*>), (<|>))
import Text.Parsec (string, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)

-- Spacing

m :: Parser String
m = string "m" <* spaces >> return "margin"

-- Background Rules

bg :: Parser String
bg = string "bg" >> return "background"

bgi :: Parser String
bgi = string "bgi" >> return "backgroud-image"

bgr :: Parser String
bgr = string "bgr" >> return "background-repeat"
