module Main where

import PolygonPuzzlesGenerator (polygonPuzzles)

main :: IO ()
main =
  do    
    putStrLn (unlines (map show polygonPuzzles))
