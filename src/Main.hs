module Main where

import Text.Parsec
       ( parse
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
       )
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Data.List (sortBy)
import System.IO (readFile, writeFile)
import System.Environment (getArgs)
import Control.Applicative ((<*), (*>), (<|>))

type Filename = String
type Selector = String
type CSSFile  = String

data Rule = Rule String String deriving Show
data Ruleset = Ruleset Selector [Rule] deriving Show
data Comment = Comment String deriving Show

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

selector :: Parser Selector
selector = many1 (oneOf "#." <|> letter <|> digit) <* spaces

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [fileName] -> do
      file <- readFile fileName
      case parseCSSFile file of
        Right validCSS -> writeCSSFile "test.css" (beautifyCSS validCSS)
        Left err       -> putStrLn $ show err      
    _          -> help

help :: IO () 
help = putStr $ unlines $
  "CSS Styler\n" :
  "Style organizes your CSS file by rule length for any given rule set\n" :
  "Run the name of the file that you want the program to output" : []

parseCSSFile :: String -> Either ParseError [Ruleset]
parseCSSFile = parse (many1 ruleset) "css file"

beautifyCSS :: [Ruleset] -> [Ruleset]
beautifyCSS rulesets = fmap organizeRules rulesets
  where organizeRules (Ruleset x rules) = Ruleset x (sortedRules rules)

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
rulesToString (Ruleset selector rules) =
  let writeRule (Rule property value) = "  " ++ property ++ ": " ++ value ++ ";\n"
  in selector ++ " {\n" ++ (concat $ fmap writeRule rules) ++ "}\n"


