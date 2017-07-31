module Main where

import Test.Hspec
import ParseCSS
  ( parseCSS
  , Block(..)
  , Rule(..)
  )

ruleset1 :: String
ruleset1 = "p{ display: block; margin: 0 auto; }"

ruleset2 :: String
ruleset2 = "    #header {\n  background-color: #000;\n  background-size: cover;\n}"

ruleset3 :: String
ruleset3 = ".class {\n margin: 0 auto;\n --comment\n background-repeat: no-repeat;\n}"

main :: IO ()
main = do
  hspec $ plainCSSSpec
  hspec $ noSemicolonSpec


plainCSSSpec :: Spec
plainCSSSpec = do
  describe "Parse CSS Rules" $ do
    it "parse a rule of CSS" $ do
      parseCSS ruleset1 `shouldBe` parsedCSS1
    it "parse 2 rules of CSS" $ do
      parseCSS (ruleset1 ++ ruleset2) `shouldBe` parsedCSS2
    it "parse comments in rulesets" $ do
      parseCSS (ruleset3) `shouldBe` parsedCSS3

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
        parsedCSS3 = Right $ [( Ruleset ".class"
                              [ Rule "margin" "0 auto"
                              , Comment' "comment"
                              , Rule "background-repeat" "no-repeat" ]
                              )
                             ]

noSemicolonSpec :: Spec
noSemicolonSpec = do
  describe "Parse terse CSS Syntax" $ do
    it "parse a rule of terse CSS" $ do
      parseCSS ".class {\n\t\tmargin: 0 auto\n\t\tdisplay:block\n}\n"
        `shouldBe` (Right $ [( Ruleset ".class"
                             [ Rule "margin" "0 auto"
                             , Rule "display" "block" ]
                            )
                            ])
    it "parse 2 rules of terse CSS" $ do
      parseCSS "#header {\n\t\tbackground-image: url(\"http://www.google.com\")\nbackground-color: red\n\n\n\n}\n\t\t\t\t\t.class {\n\t\tmargin: 0 auto\ndisplay:inline-block\n}\n"
      `shouldBe` (Right $ [( Ruleset "#header"
                           [ Rule "background-image" "url(\"http://www.google.com\")"
                           , Rule "background-color" "red"])
                           ,
                           ( Ruleset ".class"
                           [ Rule "margin" "0 auto"
                           , Rule "display" "inline-block" ])
                           ])
                 
                          
                           
