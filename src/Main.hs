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
       , ArgOrder(Permute)
       , OptDescr(..)
       , ArgDescr(..)
       )

data Options = Options
             { optHelp    :: Bool
             , optVersion :: Bool
             , optInput   :: String
             , optOutput  :: String
             , optStyle   :: CompileOption
             } deriving Show

defaultOptions :: Options
defaultOptions =
  Options { optHelp    = False
          , optVersion = False
          , optInput   = []
          , optOutput  = "output.css"
          , optStyle   = Beautify
          }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['i'] ["info"]
           (NoArg (\opt -> opt { optHelp = True }))
           "Show info screen"
  , Option ['v'] ["version"]
           (NoArg (\opt -> opt {optVersion = True }))
           "Show version number"
  , Option ['c'] ["compile"]
           (ReqArg (\fname opt -> opt { optInput = fname }) "FILE")
           "Input file to compile"
  , Option ['o'] ["output"]
           (ReqArg (\fname opt -> opt { optOutput = fname }) "FILE")
           "Output file name"
  , Option ['m'] ["minify"]
           (NoArg (\opt -> opt { optStyle = Minify }))
           "Minify CSS"
  ]

determineOptions :: [String] -> IO (Options, [String])
determineOptions argv =
  case getOpt Permute options argv of
    (o, n, []    ) -> return $ (foldl (flip id) defaultOptions o, n)
    (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo header options
  where header = "\nUsage: css-styler [OPTION...] filenames"

main :: IO ()
main = do
  argv <- getArgs
  (opts, fnames) <- determineOptions argv
  let output = optOutput opts
  let style = optStyle opts

  when (optHelp opts) $ do
    info
    exitSuccess
  when (optVersion opts) $ do
    putStrLn "CSS-Styler -- Version 1.0.0"
    exitSuccess
  when (optInput opts == []) $ do
    let inputUsage = "\nMust include at least one input file to compile!\n Uses:\n"
    ioError $ userError $ usageInfo inputUsage options
    
  case parseCSSFile $ optInput opts of
    Right validCSS -> writeCSSFile style output validCSS
    Left  err      -> ioError $ userError $ show err
  exitSuccess
    

info :: IO () 
info = putStr $ unlines $
  "CSS Styler\n" :
  "Style organizes your CSS file by rule length for any given rule set\n" :
  "Run the name of the file that you want the program to output" : []
