module AllPolygonSolutionsFor where

import MatrixPuzzles (List, MatrixPuzzle, MatrixSolution)
import PolygonSpecifics (polygonSolver)

emptyPolygonPuzzle :: (Int, Int) -> MatrixPuzzle Integer
emptyPolygonPuzzle (polygonSize, polygonSideSize) =
  replicate polygonSize (replicate (polygonSideSize - 1) Nothing)

allPolygonSolutionsFor :: (Int, Int, Integer) -> List (MatrixSolution Integer)
allPolygonSolutionsFor (polygonSize, polygonSideSize, polygonMaxNumber) =
  polygonSolver polygonMaxNumber (emptyPolygonPuzzle (polygonSize, polygonSideSize))
