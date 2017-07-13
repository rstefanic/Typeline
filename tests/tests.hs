module Main where

import Test.Hspec
import ParseCSS
  ( parseCSSFile
  , Ruleset(..)
  , Rule(..))

ruleset1 :: String
ruleset1 = "p{ display: block; margin: 0 auto; }"

ruleset2 :: String
ruleset2 = "    #header {\n  background-color: #000;\n  background-size: cover;\n}"

main :: IO ()
main = do
  putStrLn "\nRunning Test Suite..."

  hspec $ do
    describe "Parse CSS Rules" $ do
      it "parse a rule of CSS" $ do
        parseCSSFile ruleset1 `shouldBe` parsedCSS1
      it "parse 2 rules of CSS" $ do
        parseCSSFile (ruleset1 ++ ruleset2) `shouldBe` parsedCSS2

        where
          parsedCSS1 = Right $ [  Ruleset "p"
                                [ Rule "display" "block"
                                , Rule "margin" "0 auto" ]
                               ]
          parsedCSS2 = Right $ [( Ruleset "p"
                                [ Rule "display" "block"
                                , Rule "margin" "0 auto" ]
                                ),
                                ( Ruleset "#header"
                                [ Rule "background-color" "#000"
                                , Rule "background-size" "cover" ]
                                )
                               ]                     
            
