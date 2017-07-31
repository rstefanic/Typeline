module ParseCSS
        ( parseCSS
        , writeCSSFile
        , beautifyRulesToString
        , CompileOption(..)
        , Block(..)
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

import Extensions

data CompileOption = Beautify | Minify deriving (Eq, Show)

type Selector = String
data Rule = Rule String String 
          | Comment' String
          deriving (Eq, Show)

data Block = Ruleset Selector [Rule]
             | Comment String deriving (Eq, Show)

rule :: Parser Rule
rule = do try $ commentRule <|> do
            p <- spaces *> (many1 $ letter <|> char '-') <* char ':' <* spaces
            v <- many1 (noneOf ";\n") <* (optional $ char ';' <|> char '\n') <* spaces
            return $ Rule p v

commentRule :: Parser Rule
commentRule = do
  commentString <- comment
  case commentString of
    Comment x -> return $ Comment' x
    _         -> return $ Comment' ""

ruleset :: Parser Block
ruleset = do try comment
      <|> do
          s <- selector `sepBy1` spaces
          r <- oneOf "{\n" <* spaces *> many1 rule
            <* oneOf "}\n" <* spaces
          return $ Ruleset (unwords s) r

comment :: Parser Block
comment = do
  commentString <- try $
        ((string "--") *> manyTill anyChar (try $ string "\n"))           
    <|> ((string "/*") *> manyTill anyChar (try $ string "*/"))
  return $ Comment commentString

selector :: Parser Selector
selector = many1 (oneOf "#." <|> letter <|> digit <|> char '-') <* spaces

parseCSS :: String -> Either ParseError [Block]
parseCSS = parse (many1 ruleset) "Compiling CSS File"

beautifyCSS :: [Block] -> [Block]
beautifyCSS rulesets = fmap organizeRules rulesets
  where organizeRules (Ruleset x rules) = Ruleset x (sortedRules rules)
        organizeRules (Comment comment) = Comment comment

sortedRules :: [Rule] -> [Rule]
sortedRules = (returnRules . sortRules . fmap getRuleLength)
  where sortRules = sortBy (\rule1 rule2 -> compare (snd rule1) (snd rule2))
        returnRules = fmap fst
                         
getRuleLength :: Rule -> (Rule, Int)
getRuleLength rule@(Rule property value) = (rule, (length property + length value))

writeCSSFile :: CompileOption -> FilePath -> [Block] -> IO ()
writeCSSFile Minify   file rules = writeFile file (concat $ fmap minifyRulesToString rules)
writeCSSFile Beautify file rules = writeFile file (concat $ fmap beautifyRulesToString rules')
  where rules' = beautifyCSS rules

beautifyRulesToString :: Block -> String
beautifyRulesToString (Comment comment) = "/*" ++ comment ++ "*/"
beautifyRulesToString (Ruleset selector rules) =
  let writeRule (Rule property value) = "  " ++ property ++ ": " ++ value ++ ";\n"
  in selector ++ " {\n" ++ (concat $ fmap writeRule rules) ++ "}\n"

minifyRulesToString :: Block -> String
minifyRulesToString (Comment _) = []
minifyRulesToString (Ruleset selector rules) =
  let writeRule (Rule property value) = property ++ ":" ++ value ++ ";"
  in selector ++ "{" ++ (concat $ fmap writeRule rules) ++ "}"
