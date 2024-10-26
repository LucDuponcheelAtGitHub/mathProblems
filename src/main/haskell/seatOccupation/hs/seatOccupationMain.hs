module Main where

import SeatOccupation (pd, pr)

main :: IO ()
main =
  do
    putStrLn "printing result as double for 10 (for 100 is way too slow)"
    print(10, pd 10)
    putStrLn "printing result as rational for 10 (for 100 is way too slow)"
    print(10, pr 10) 