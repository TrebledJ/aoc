module Utils where

import           Control.Monad
import           Data.Maybe
import           System.Environment


data Options = Options
    { filename  :: Maybe String
    , runPart1  :: Bool
    , runPart2 :: Bool
    }

defaultMain :: String -> (String -> a) -> (a -> Int) -> (a -> Int) -> IO ()
defaultMain defaultFile parse part1 part2 = do
    opts <- parseArgs nullOpts <$> getArgs
    let file = fromMaybe defaultFile (filename opts)
    input <- parse <$> readFile file
    when (runPart1 opts) $ do
        putStr "part1: "
        print $ part1 input
    when (runPart2 opts) $ do
        putStr "part2: "
        print $ part2 input

  where
    nullOpts =
        Options { filename = Nothing, runPart1 = False, runPart2 = False }
    parseArgs o [] = if not (runPart1 o) && not (runPart2 o)
        then o { runPart1 = True, runPart2 = True }
        else o
    parseArgs o ("-f" : f : rest) = parseArgs (o { filename = Just f }) rest
    parseArgs o ("p1"     : rest) = parseArgs (o { runPart1 = True }) rest
    parseArgs o ("p2"     : rest) = parseArgs (o { runPart2 = True }) rest
    parseArgs o (_        : rest) = o

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

firstBy :: (a -> Bool) -> [a] -> a
firstBy p = head . filter p

lastBy :: (a -> Bool) -> [a] -> a
lastBy p = last . filter p
