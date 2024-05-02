module PolygonPuzzles where

import Data.List (zipWith3)
import MatrixPuzzles (Choices, Matrix, isUniqueSolution)
import PolygonSpecifics (PolygonPuzzle, PolygonSolution, polygonChoices, polygonSolutions)
import Utilities (listOfListsCombinations)

polygonPuzzlesFor :: Int -> Int -> Choices (Maybe Integer) -> [PolygonPuzzle]
polygonPuzzlesFor polygonSize sideSize polygonChoices =
  zipWith3
    (\xs ys zss -> xs : ys : zss)
    (map init combinations)
    (map second combinations)
    (replicate (length combinations) others)
  where
    combinations = listOfListsCombinations (replicate sideSize polygonChoices)
    second = (\x -> x : replicate (sideSize - 2) Nothing) . last
    others = replicate (polygonSize - 2) (replicate (sideSize - 1) Nothing)

polygonSize :: Int
polygonSize = 6

sideSize :: Int
sideSize = 3

-- maybe also some of the generated examples (?)
polygonPuzzles :: [PolygonPuzzle]
polygonPuzzles = polygonPuzzlesFor polygonSize sideSize polygonChoices

uniqueSolutions :: [PolygonPuzzle] -> [PolygonSolution]
uniqueSolutions = map head . filter isUniqueSolution . map polygonSolutions

uniquePolygonPuzzlesSolutions :: [PolygonSolution]
uniquePolygonPuzzlesSolutions = uniqueSolutions polygonPuzzles 
