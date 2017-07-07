module Main where

import ParseCSS (parseCSSFile, beautifyCSS, writeCSSFile)

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.IO (readFile, writeFile)
import System.Exit (exitSuccess)
import System.Environment (getArgs)
import System.Console.GetOpt
       ( getOpt
       , usageInfo
       , ArgOrder(..)
       , OptDescr(..)
       , ArgDescr(..)
       )

data Options = Options
             { optHelp    :: Bool
             , optVersion :: Bool
             , optInput   :: String
             , optOutput  :: Maybe String
             } deriving Show

defaultOptions :: Options
defaultOptions =
  Options { optHelp    = False
          , optVersion = False
          , optInput   = " "
          , optOutput  = Nothing
          }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
           (NoArg (\opt -> opt { optHelp = True }))
           "Show Help Screen"
  , Option ['v'] ["version"]
           (NoArg (\opt -> opt {optVersion = True }))
           "Show Version Number"
  , Option ['i'] []
           (ReqArg (\fname opt -> opt { optInput = optInput opt ++ fname })
           "FILE")
           "Input File Name"
  , Option ['o'] ["output"]
           (OptArg ((\fname opt -> opt { optOutput = Just fname }) . fromMaybe "output")
           "FILE")
           "Output File Name"
  ]

determineOptions :: [String] -> IO (Options, [String])
determineOptions argv =
  case getOpt Permute options argv of
    (o, n,  []    ) -> return (foldl (flip id) defaultOptions o, n)
    (o, [], []    ) -> ioError $ userError $ "Must add fileout name"
    (_, _,  errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where header = "Usage: css-styler [OPTION...] filenames"


main :: IO ()
main = do
  argv <- getArgs
  (opts, fnames) <- determineOptions argv
  
  when (optHelp opts) $ do
    help
    exitSuccess
  when (optVersion opts) $ do
    putStrLn "CSS-Styler -- Version 1.0.0"
    exitSuccess

  print opts
  
{-  case parseCSSFile file of
        Right validCSS -> writeCSSFile "test.css" (beautifyCSS validCSS)
        Left err       -> putStrLn $ show err      
-}

help :: IO () 
help = putStr $ unlines $
  "CSS Styler\n" :
  "Style organizes your CSS file by rule length for any given rule set\n" :
  "Run the name of the file that you want the program to output" : []




