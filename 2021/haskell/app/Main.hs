import qualified Days.D01                      as D01
-- import qualified Days.D02 as D02
-- import qualified Days.D03 as D03
import qualified Days.D04                      as D04
-- import qualified Days.D05 as D05
import qualified Days.D06                      as D06
import qualified Days.D07                      as D07
import qualified Days.D08                      as D08
import qualified Days.D09                      as D09
import qualified Days.D10                      as D10
-- import qualified Days.D11 as D11
import qualified Days.D12                      as D12
import qualified Days.D13                      as D13
import qualified Days.D14                      as D14
-- import qualified Days.D15 as D15
import qualified Days.D16                      as D16
import qualified Days.D17                      as D17
-- import qualified Days.D18 as D18
-- import qualified Days.D19 as D19
-- import qualified Days.D20 as D20
import qualified Days.D21                      as D21
import qualified Days.D22                      as D22
-- import qualified Days.D23 as D23
-- import qualified Days.D24 as D24
import qualified Days.D25                      as D25

import           Control.Monad
import           Data.Bifunctor
import           System.Environment

import           Utils


main :: IO ()
main = do
  (days, rest) <- parseDayArgs [] <$> getArgs
  forM_ days $ \day -> do
    if day < 1 || day > 25
      then putStrLn "Day should be between 1 and 25 inclusive."
      else case mains !! (day - 1) of
        Nothing -> putStrLn $ "Day" ++$ day ++ " hasn't been implemented yet."
        Just dayMain -> do
          putStrLn $ "Running day" ++$ day ++ ":"
          withArgs rest $ dayMain day
    return ()

parseDayArgs :: [Int] -> [String] -> ([Int], [String])
parseDayArgs days []                  = (if null days then [1] else days, [])
parseDayArgs days ("-d" : day : rest) = parseDayArgs (read day : days) rest
parseDayArgs days ("-day" : day : rest) = parseDayArgs (read day : days) rest
parseDayArgs days (arg        : rest) = second (arg :) $ parseDayArgs days rest

mains :: [Maybe (Int -> IO ())]
mains =
  [ Just $ fromImpl (D01.parse, D01.part1, D01.part2)
  , Nothing
  , Nothing
  , Just $ fromImpl (D04.parse, D04.part1, D04.part2)
  , Nothing
  , Just $ fromImpl (D06.parse, D06.part1, D06.part2)
  , Just $ fromImpl (D07.parse, D07.part1, D07.part2)
  , Just $ fromImpl (D08.parse, D08.part1, D08.part2)
  , Just $ fromImpl (D09.parse, D09.part1, D09.part2)
  , Just $ fromImpl (D10.parse, D10.part1, D10.part2)
  , Nothing
  , Just $ fromImpl (D12.parse, D12.part1, D12.part2)
  , Just $ fromImpl (D13.parse, D13.part1, D13.part2)
  , Just $ fromImpl (D14.parse, D14.part1, D14.part2)
  , Nothing
  , Just $ fromImpl (D16.parse, D16.part1, D16.part2)
  , Just $ fromImpl (D17.parse, D17.part1, D17.part2)
  , Nothing
  , Nothing
  , Nothing
  , Just $ fromImpl (D21.parse, D21.part1, D21.part2)
  , Just $ fromImpl (D22.parse, D22.part1, D22.part2)
  , Nothing
  , Nothing
  , Just $ fromImpl (D25.parse, D25.part1, D25.part2)
  ]
  where fromImpl (parse, part1, part2) day = defaultMain (getDefaultFile day) parse part1 part2

