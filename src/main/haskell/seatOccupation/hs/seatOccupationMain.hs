module Main where

import Data.List
import GHC.Real (fromIntegral)
import SeatOccupation (pd, pr)

main :: IO ()
main =
  do
    (putStrLn . intercalate "\n" . map show) [(fromIntegral n, pd n) | n <- [2 .. 100]]
    (putStrLn . intercalate "\n" . map show) [(fromIntegral n, pr n) | n <- [2 .. 100]]