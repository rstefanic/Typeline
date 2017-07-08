module Main where

import ParseCSS (parseCSSFile, writeCSSFile, CompileOption(..))

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
             , optStyle   :: CompileOption
             } deriving Show

defaultOptions :: Options
defaultOptions =
  Options { optHelp    = False
          , optVersion = False
          , optOutput  = Nothing
          , optStyle   = Beautify
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
           (ReqArg (\fname opt -> opt { optInput = fname }) "FILE")
           "Input File Name"
  , Option ['o'] ["output"]
           (OptArg ((\fname opt -> opt { optOutput = Just fname }) . fromMaybe "output")
           "FILE")
           "Output File Name"
  , Option ['m'] ["minify"]
           (NoArg (\opt -> opt { optStyle = Minify }))
           "Minify CSS"
  ]

determineOptions :: [String] -> IO (Options, [String])
determineOptions argv =
  case getOpt Permute options argv of
    (o,  n, []    ) -> return $ (foldl (flip id) defaultOptions o, n)
    (_,  _, errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where header = "Usage: css-styler [OPTION...] filenames"

main :: IO ()
main = do
  argv <- getArgs
  (opts, fnames) <- determineOptions argv
  let style = optStyle opts

  print opts
  
  when (optHelp opts) $ do
    help
    exitSuccess
  when (optVersion opts) $ do
    putStrLn "CSS-Styler -- Version 1.0.0"
    exitSuccess
  when (optOutput opts /= Nothing) $ do
    case parseCSSFile $ optInput opts of
      Right validCSS -> writeCSSFile style (fromMaybe "" $ optOutput opts) validCSS
      Left  err      -> ioError $ userError $ show err
    exitSuccess

  case parseCSSFile $ optInput opts of
    Right validCSS -> writeCSSFile style "output.css" validCSS
    Left  err      -> ioError $ userError $ show err
  exitSuccess
    

help :: IO () 
help = putStr $ unlines $
  "CSS Styler\n" :
  "Style organizes your CSS file by rule length for any given rule set\n" :
  "Run the name of the file that you want the program to output" : []
