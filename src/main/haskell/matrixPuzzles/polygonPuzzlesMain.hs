module Main where

import PolygonPuzzles (uniquePolygonPuzzlesSolutions)

polygonPuzzlesMain :: IO ()
polygonPuzzlesMain =
  do    
    putStr "polygon puzzle with 5 vertices with sides of 3 values in [1..20] has "
    putStr (show (length uniquePolygonPuzzlesSolutions))
    putStrLn " unique solutions"
    putStrLn (unlines (map show uniquePolygonPuzzlesSolutions))

main :: IO ()
main = polygonPuzzlesMain
