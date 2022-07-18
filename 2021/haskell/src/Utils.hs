{-# LANGUAGE FlexibleInstances #-}
module Utils where

import           Control.Monad
import           Data.Char                      ( digitToInt )
import qualified Data.HashMap.Strict           as M
import           Data.Hashable                  ( Hashable )
import           Data.Void                      ( Void )
import           Debug.Trace                   as T
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char


type Parser = Parsec Void String

data Options = Options
  { file     :: String
  , runPart1 :: Bool
  , runPart2 :: Bool
  }


class Show a => Print a where
  print' :: a -> IO ()
  print' = print

-- Implement for common types, especially String.
instance Print Int where
instance Print Integer where
instance Print Float where
instance Print Double where
instance Print String where
  print' = putStrLn

-- Wrapper because of the silly undecidable instances.
newtype Answer a = Answer a deriving Show
instance Show a => Print (Answer a) where
  print' (Answer x) = print x

class ParseLike p where
  -- Parser object, filename, contents -> result.
  doParse :: p a -> String -> String -> a

instance ParseLike ((->) String) where
  doParse f _ = f

instance ParseLike Parser where
  doParse p file txt = case runParser p file txt of
    Right res -> res
    Left  err -> T.trace (errorBundlePretty err) undefined


defaultMain
  :: (ParseLike p, Print b, Print c)
  => String   -- Default input file, if no -f option was provided from args.
  -> p a      -- Any instance of ParseLike, e.g. a function (String -> a) or a parser combinator (Parser a).
  -> (a -> b) -- Function to solve part 1. Takes in input and returns something printable.
  -> (a -> c) -- Function to solve part 2.
  -> IO ()
defaultMain defaultFile parse part1 part2 = do
  opts  <- parseArgs (nullOpts { file = defaultFile }) <$> getArgs
  input <- doParse parse (file opts) <$> readFile (file opts)
  when (runPart1 opts) $ do
    putStr "part1: "
    print' $ part1 input
  when (runPart2 opts) $ do
    putStr "part2: "
    print' $ part2 input

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

integer :: (Num i, Read i, Show i) => Parser i
integer = ((0 -) <$> try (char '-' *> number)) <|> number

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

-- Helper for concatenating stuff to strings.
(++$) :: (Show a) => String -> a -> String
(++$) s x = s ++ " " ++ show x

counter :: (Hashable a, Eq a) => [a] -> M.HashMap a Int
counter = foldr (\x -> M.insertWith (+) x 1) M.empty

fromBinary :: String -> Int
fromBinary = foldl (\acc d -> 2 * acc + digitToInt d) 0

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x : xs) = foldM f x xs
foldM1 _ []       = undefined
