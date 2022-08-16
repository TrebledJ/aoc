import qualified Days.D01 as D01 (main)
-- import qualified Days.D02 as D02 (main)
-- import qualified Days.D03 as D03 (main)
import qualified Days.D04 as D04 (main)
-- import qualified Days.D05 as D05 (main)
import qualified Days.D06 as D06 (main)
import qualified Days.D07 as D07 (main)
import qualified Days.D08 as D08 (main)
import qualified Days.D09 as D09 (main)
import qualified Days.D10 as D10 (main)
-- import qualified Days.D11 as D11 (main)
import qualified Days.D12 as D12 (main)
import qualified Days.D13 as D13 (main)
import qualified Days.D14 as D14 (main)
-- import qualified Days.D15 as D15 (main)
import qualified Days.D16 as D16 (main)
import qualified Days.D17 as D17 (main)
-- import qualified Days.D18 as D18 (main)
-- import qualified Days.D19 as D19 (main)
-- import qualified Days.D20 as D20 (main)
import qualified Days.D21 as D21 (main)
import qualified Days.D22 as D22 (main)
-- import qualified Days.D23 as D23 (main)
-- import qualified Days.D24 as D24 (main)
import qualified Days.D25 as D25 (main)

import           Control.Monad
import           Data.Bifunctor
import           System.Environment

import Utils



main :: IO ()
main = do
  (days, rest) <- parseDayArgs [] <$> getArgs
  forM_ days $ \day -> do
    if day < 1 || day > 25 then
      putStrLn "Day should be between 1 and 25 inclusive."
    else
      case mains !! (day - 1) of
        Nothing -> putStrLn $ "Day " ++$ day ++ " hasn't been implemented yet."
        Just io -> withArgs rest io
    return ()

parseDayArgs :: [Int] -> [String] -> ([Int], [String])
parseDayArgs days [] = (if null days then [1] else days, [])
parseDayArgs days ("-d":day:rest) = parseDayArgs (read day:days) rest
parseDayArgs days (arg:rest) = second (arg:) $ parseDayArgs days rest

mains :: [Maybe (IO ())]
mains = [
    Just D01.main,
    Nothing,
    Nothing,
    Just D04.main,
    Nothing,
    Just D06.main,
    Just D07.main,
    Just D08.main,
    Just D09.main,
    Just D10.main,
    Nothing,
    Just D12.main,
    Just D13.main,
    Just D14.main,
    Nothing,
    Just D16.main,
    Just D17.main,
    Nothing,
    Nothing,
    Nothing,
    Just D21.main,
    Just D22.main,
    Nothing,
    Nothing,
    Just D25.main
  ]
