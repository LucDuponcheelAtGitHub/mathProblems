module PolygonPuzzlesGenerator where

import Data.List (nub)
import MatrixPuzzles (Matrix)
import PolygonPuzzles (sideSize, uniquePolygonPuzzlesSolutions)
import PolygonSpecifics (PolygonPuzzle, PolygonSolution, polygonSolutions)
import Utilities (unconcat, zipWithHead)

polygonPuzzleCandidates :: [[PolygonPuzzle]]
polygonPuzzleCandidates = map replaceJustsByNothings uniquePolygonPuzzlesSolutions
  where
    replaceJustsByNothings = map (unconcat (sideSize - 1)) . replaceJustsByNothingsHelper . concat
      where
        replaceJustsByNothingsHelper mxs = reverse (replaceJustsByNothingAtMost (length mxs) mxs)
          where
            replaceJustsByNothingAtMost 1 [] = [[]]
            replaceJustsByNothingAtMost 1 (Nothing : mxs) = map (Nothing :) (replaceJustsByNothingAtMost 1 mxs)
            replaceJustsByNothingAtMost 1 ((Just x) : mxs) = (Nothing : mxs) : map (Just x :) (replaceJustsByNothingAtMost 1 mxs)
            replaceJustsByNothingAtMost n mxs = nub (concatMap (replaceJustsByNothingAtMost 1) rec1 ++ rec1)
              where
                rec1 = replaceJustsByNothingAtMost (n - 1) mxs
                rec2 = concatMap (replaceJustsByNothingAtMost 1) rec1

polygonPuzzles :: [PolygonPuzzle]
polygonPuzzles = concatMap filterCorrectPolygonPuzzles polygonPuzzleCandidates
  where
    filterCorrectPolygonPuzzles = extractPuzzles . filter correct . zipWithSolution
      where
        zipWithSolution = zipWithHead
        correct (s, g) = polygonSolutions g == [s]
        extractPuzzles = map snd
