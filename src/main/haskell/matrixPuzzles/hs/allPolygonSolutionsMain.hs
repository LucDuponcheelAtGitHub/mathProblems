module Main where

import AllPolygonSolutionsFor (allPolygonSolutionsFor)

polygonSize :: Int
polygonSize = 3

polygonSideSize :: Int
polygonSideSize = 3

polygonMaxNumber :: Integer
polygonMaxNumber = 16

allPolygonSolutions = allPolygonSolutionsFor (polygonSize, polygonSideSize, polygonMaxNumber)

allPolygonSolutionsMain :: IO ()
allPolygonSolutionsMain =
  do
    putStr "for polygons with "
    putStr (show polygonSize)
    putStr " vertices with "
    putStr (show polygonSideSize)
    putStr " values in [1.."
    putStr (show polygonMaxNumber)
    putStrLn "] on their sides"
    -- putStrLn "all the polygon puzzle solutions are "
    -- putStr (unlines (map show allPolygonSolutions))
    putStr "there are "
    putStr (show (length allPolygonSolutions))
    putStrLn " polygon puzzle solutions"

main :: IO ()
main = allPolygonSolutionsMain
