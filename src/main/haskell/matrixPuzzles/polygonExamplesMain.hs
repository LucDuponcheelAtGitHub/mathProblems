module Main where

import PolygonExamples (uniquePolygonExamplesSolutions)

polygonExamplesMain :: IO ()
polygonExamplesMain =
  do    
    putStr "polygon puzzle with 5 vertices with sides of 3 values in [1..20] has "
    putStr (show (length uniquePolygonExamplesSolutions))
    putStrLn " unique solutions"
    putStrLn (unlines (map show uniquePolygonExamplesSolutions))

main :: IO ()
main = polygonExamplesMain
