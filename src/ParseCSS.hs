module ParseCSS
        ( parseCSSFile
        , writeCSSFile
        , beautifyRulesToString
        , CompileOption(..)
        , Ruleset(..)
        , Rule(..)
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
       , anyChar
       )
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)

data CompileOption = Beautify | Minify deriving (Eq, Show)

type Selector = String
data Rule = Rule String String deriving (Eq, Show)
data Ruleset = Ruleset Selector [Rule]
             | Comment String deriving (Eq, Show)

rule :: Parser Rule
rule = do
  p <- spaces *> (many1 $ letter <|> char '-') <* char ':' <* spaces
  v <- many1 (noneOf ";\n") <* (optional $ char ';' <|> char '\n') <* spaces
  return $ Rule p v
  

ruleset :: Parser Ruleset
ruleset = do
  s <- selector `sepBy1` spaces
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
selector = many1 (oneOf "#." <|> letter <|> digit <|> char '-') <* spaces

parseCSSFile :: String -> Either ParseError [Ruleset]
parseCSSFile = parse (many1 ruleset) "Compiling CSS File"

beautifyCSS :: [Ruleset] -> [Ruleset]
beautifyCSS rulesets = fmap organizeRules rulesets
  where organizeRules (Ruleset x rules) = Ruleset x (sortedRules rules)
        organizeRules (Comment comment) = Comment comment

sortedRules :: [Rule] -> [Rule]
sortedRules = (returnRules . sortRules . fmap getRuleLength)
  where sortRules = sortBy (\rule1 rule2 -> compare (snd rule1) (snd rule2))
        returnRules = fmap fst
                         
getRuleLength :: Rule -> (Rule, Int)
getRuleLength rule@(Rule property value) = (rule, (length property + length value))

writeCSSFile :: CompileOption -> FilePath -> [Ruleset] -> IO ()
writeCSSFile Minify   file rules = writeFile file (concat $ fmap minifyRulesToString rules)
writeCSSFile Beautify file rules = writeFile file (concat $ fmap beautifyRulesToString rules')
  where rules' = beautifyCSS rules

beautifyRulesToString :: Ruleset -> String
beautifyRulesToString (Comment comment) = "/*" ++ comment ++ "*/"
beautifyRulesToString (Ruleset selector rules) =
  let writeRule (Rule property value) = "  " ++ property ++ ": " ++ value ++ ";\n"
  in selector ++ " {\n" ++ (concat $ fmap writeRule rules) ++ "}\n"

minifyRulesToString :: Ruleset -> String
minifyRulesToString (Comment _) = []
minifyRulesToString (Ruleset selector rules) =
  let writeRule (Rule property value) = property ++ ":" ++ value ++ ";"
  in selector ++ "{" ++ (concat $ fmap writeRule rules) ++ "}"
