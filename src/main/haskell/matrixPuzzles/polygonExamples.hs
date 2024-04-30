module PolygonExamples where

import Data.List (zipWith3)
import MatrixPuzzles (Choices, Matrix, isUniqueSolution)
import PolygonSpecifics (polygonChoices, polygonSolutions)
import Utilities (listOfListCombinations)

polygonExamplesFor :: Int -> Int -> Choices (Maybe Integer) -> [Matrix (Maybe Integer)]
polygonExamplesFor polygonSize sideSize polygonChoices =
  zipWith3
    (\xs ys zss -> xs : ys : zss)
    (map init combinations)
    (map second combinations)
    (replicate (length combinations) others)
  where
    combinations = listOfListCombinations (replicate sideSize polygonChoices)
    second = (\x -> x : replicate (sideSize - 2) Nothing) . last
    others = replicate (polygonSize - 2) (replicate (sideSize - 1) Nothing)

polygonSize :: Int
polygonSize = 5

sideSize :: Int
sideSize = 3

polygonExamples :: [Matrix (Maybe Integer)]
polygonExamples = polygonExamplesFor polygonSize sideSize polygonChoices

uniqueSolutions :: [Matrix (Maybe Integer)] -> [Matrix (Maybe Integer)]
uniqueSolutions = map head . filter isUniqueSolution . map polygonSolutions

uniquePolygonExamplesSolutions :: [Matrix (Maybe Integer)]
uniquePolygonExamplesSolutions = uniqueSolutions polygonExamples 
