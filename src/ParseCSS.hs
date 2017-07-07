module ParseCSS
        ( parseCSSFile
        , beautifyCSS
        , writeCSSFile
        ) where

import Data.List (sortBy)
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
       , endBy
       , anyChar
       , between
       )
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)


type Filename = String
type Selector = String
type CSSFile  = String

data Rule = Rule String String deriving Show
data Ruleset = Ruleset Selector [Rule]
             | Comment String deriving Show

rule :: Parser Rule
rule = do
  p <- spaces *> (many1 $ letter <|> char '-') <* char ':' <* spaces
  v <- many1 (noneOf ";\n") <* (optional $ char ';' <|> char '\n') <* spaces
  return $ Rule p v
  
ruleset :: Parser Ruleset
ruleset = do
  s <- spaces *> selector `sepBy1` spaces
  r <- oneOf "{\n" <* spaces *> many1 rule
    <* oneOf "}\n" <* spaces
  return $ Ruleset (unwords s) r

comment :: Parser Ruleset
comment = do
  commentString <- try $
        ((string "--") *> manyTill anyChar (try $ char '\n'))           
    <|> ((string "/*") *> manyTill anyChar (try $ string "*/"))
  return $ Comment commentString

selector :: Parser Selector
selector = many1 (oneOf "#." <|> letter <|> digit) <* spaces

parseCSSFile :: String -> Either ParseError [Ruleset]
parseCSSFile = parse (many1 ruleset) "Creating CSS File"

beautifyCSS :: [Ruleset] -> [Ruleset]
beautifyCSS rulesets = fmap organizeRules rulesets
  where organizeRules (Ruleset x rules) = Ruleset x (sortedRules rules)
        organizeRules (Comment comment) = Comment comment

minifyCSS :: [Ruleset] -> [Ruleset]
minifyCSS = undefined

sortedRules :: [Rule] -> [Rule]
sortedRules = (returnRules . sortRules . fmap getRuleLength)
  where sortRules = sortBy (\rule1 rule2 -> compare (snd rule1) (snd rule2))
        returnRules = fmap fst
                         
getRuleLength :: Rule -> (Rule, Int)
getRuleLength rule@(Rule property value) = (rule, (length property + length value))

writeCSSFile :: FilePath -> [Ruleset] -> IO ()
writeCSSFile file rules = writeFile file (concat rules')
  where rules' = fmap rulesToString rules

rulesToString :: Ruleset -> String
rulesToString (Comment comment) = "/*" ++ comment ++ "*/"
rulesToString (Ruleset selector rules) =
  let writeRule (Rule property value) = "  " ++ property ++ ": " ++ value ++ ";\n"
  in selector ++ " {\n" ++ (concat $ fmap writeRule rules) ++ "}\n"