module Main where

import AllPolygonPuzzlesFor (allPolygonPuzzlesFor)

polygonSize :: Int
polygonSize = 3

polygonSideSize :: Int
polygonSideSize = 3

polygonMaxNumber :: Integer
polygonMaxNumber = 8

allPolygonPuzzles = allPolygonPuzzlesFor (polygonSize, polygonSideSize, polygonMaxNumber)

allPolygonPuzzlesMain :: IO ()
allPolygonPuzzlesMain =
  do
    putStr "for polygons with "
    putStr (show polygonSize)
    putStr " vertices with "
    putStr (show polygonSideSize)
    putStr " values in [1.."
    putStr (show polygonMaxNumber)
    putStrLn "] on their sides"
    putStrLn "all the polygon puzzles are "
    putStr (unlines (map show allPolygonPuzzles))
    putStr "there are "
    putStr (show (length allPolygonPuzzles))
    putStrLn " polygon puzzles with unique solution"

main :: IO ()
main = allPolygonPuzzlesMain
