module Main where

import Test.Hspec
import ParseCSS
  ( parseCSSFile
  , Ruleset(..)
  , Rule(..))

main :: IO ()
main = do
  putStrLn "Running Test Suite..."

  hspec $ do
    describe "parseCSSFile" $ do
      it "return CSS fully parsed" $ do
        parseCSSFile css `shouldBe` parsedCSS
        where css = "p{ display: block; margin: 0 auto; }"
              parsedCSS = Right $ [Ruleset "p"
                                   [ Rule "display" "block"
                                   , Rule "margin" "0 auto"
                                   ]]
