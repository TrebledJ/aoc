module Utils where

import           Control.Monad
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Void
import           Debug.Trace as T
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char


type Parser = Parsec Void String

data Options = Options
  { file     :: String
  , runPart1 :: Bool
  , runPart2 :: Bool
  }


defaultMain :: String -> (String -> a) -> (a -> Int) -> (a -> Int) -> IO ()
defaultMain defaultFile parse p1 p2 = do
  opts  <- parseArgs (nullOpts { file = defaultFile }) <$> getArgs
  input <- parse <$> readFile (file opts)
  defaultRun opts input p1 p2

defaultMainWithParser :: (Show c) => String -> Parser a -> (a -> Int) -> (a -> c) -> IO ()
defaultMainWithParser defaultFile parser p1 p2 = do
  opts     <- parseArgs (nullOpts { file = defaultFile }) <$> getArgs
  contents <- readFile (file opts)
  case runParser parser (file opts) contents of
    Right input -> defaultRun opts input p1 p2
    Left  err   -> putStrLn $ errorBundlePretty err

defaultRun :: (Show c) => Options -> a -> (a -> Int) -> (a -> c) -> IO ()
defaultRun opts input part1 part2 = do
  when (runPart1 opts) $ do
    putStr "part1: "
    print $ part1 input
  when (runPart2 opts) $ do
    putStr "part2: "
    print $ part2 input

nullOpts :: Options
nullOpts = Options { file = "", runPart1 = False, runPart2 = False }

parseArgs :: Options -> [String] -> Options
parseArgs o [] = if not (runPart1 o) && not (runPart2 o)
  then o { runPart1 = True, runPart2 = True }
  else o
parseArgs o ("-f" : f : rest) = parseArgs (o { file = f }) rest
parseArgs o ("p1"     : rest) = parseArgs (o { runPart1 = True }) rest
parseArgs o ("p2"     : rest) = parseArgs (o { runPart2 = True }) rest
parseArgs o (_        : rest) = o

number :: (Num i, Read i) => Parser i
number = read <$> some digitChar

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

firstBy :: (a -> Bool) -> [a] -> a
firstBy p = head . filter p

lastBy :: (a -> Bool) -> [a] -> a
lastBy p = last . filter p

trace :: String -> a -> a
trace = T.trace

traceShow :: (Show a) => a -> b -> b
traceShow = T.traceShow

trace' :: (Show a) => a -> a
trace' x = T.traceShow x x

counter :: (Hashable a, Eq a) => [a] -> M.HashMap a Int
counter = foldr (\x -> M.insertWith (+) x 1) M.empty
