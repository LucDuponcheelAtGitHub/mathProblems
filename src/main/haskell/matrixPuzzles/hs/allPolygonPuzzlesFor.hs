module AllPolygonPuzzlesFor where

import AllPolygonSolutionsFor (allPolygonSolutionsFor)
import Data.List ((\\))
import MatrixPuzzles (solver)
import PolygonSpecifics (polygonSolver)
import Utilities (allReplaceJustsByNothings)

-- TODO: abtract to solver (here polygonSolver polygonMaxNumber)
uniqueSolutionPolygonPuzzlesFor polygonMaxNumber (currentPolygonPuzzleCandidates, polygonSolution) =
  (nextPolygonPuzzleCandidates, (polygonSolution, uniqueSolutionPolygonPuzzles))
  where
    polygonPuzzles = allReplaceJustsByNothings polygonSolution
    hasSolution candidate = polygonSolver polygonMaxNumber candidate /= []
    nextPolygonPuzzleCandidates = filter hasSolution (polygonPuzzles \\ currentPolygonPuzzleCandidates)
    isGoodCandidate candidate = polygonSolver polygonMaxNumber candidate == [polygonSolution]
    uniqueSolutionPolygonPuzzles = filter isGoodCandidate nextPolygonPuzzleCandidates

-- TODO: abtract polygonMaxNumber to solver (here polygonSolver polygonMaxNumber)
allUniqueSolutionPolygonPuzzlesFor polygonMaxNumber [] = ([], [])
allUniqueSolutionPolygonPuzzlesFor polygonMaxNumber (s : ss) = (candidates ++ recCandidates, result : recResult)
  where
    (recCandidates, recResult) = allUniqueSolutionPolygonPuzzlesFor polygonMaxNumber ss
    (candidates, result) = uniqueSolutionPolygonPuzzlesFor polygonMaxNumber (recCandidates, s)

-- TODO: abtract to allSolutions (here allPolygonSolutionsFor (polygonSize, polygonSideSize, polygonMaxNumber))
allPolygonPuzzlesFor (polygonSize, polygonSideSize, polygonMaxNumber) =
  (concatMap snd . snd) (allUniqueSolutionPolygonPuzzlesFor polygonMaxNumber (allPolygonSolutionsFor (polygonSize, polygonSideSize, polygonMaxNumber)))