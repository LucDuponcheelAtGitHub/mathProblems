module Main where

import PolygonGameExamples (polygonGames)

main :: IO ()
main =
  do    
    putStrLn (unlines (map show polygonGames))
