module Main where

import MatrixPuzzles (List, MatrixPuzzle, MatrixSolution)
import PolygonSpecifics (polygonSolver)

polygonMaxNumber :: Integer
polygonMaxNumber = 8

polygonPuzzle :: MatrixPuzzle Integer
polygonPuzzle = [[Just 8, Nothing], [Just 2, Nothing], [Just 6, Nothing]]

polygonSolutions :: List (MatrixSolution Integer)
polygonSolutions = polygonSolver polygonMaxNumber polygonPuzzle

polygonSolutionsOfPolygonPuzzleMain :: IO ()
polygonSolutionsOfPolygonPuzzleMain =
  do
    putStrLn "polygon puzzle "
    putStrLn (show polygonPuzzle)
    putStrLn ("has solutions ")
    putStr (unlines (map show polygonSolutions))

main :: IO ()
main = polygonSolutionsOfPolygonPuzzleMain
