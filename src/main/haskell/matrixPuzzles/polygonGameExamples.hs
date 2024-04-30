module PolygonGameExamples where

import Data.List (nub)
import MatrixPuzzles (Matrix, Row, matrixMap)
import PolygonExamples (sideSize, uniquePolygonExamplesSolutions, uniqueSolutions)
import PolygonSpecifics (polygonSolutions)
import Utilities (unconcat, zipWithHead)

replaceByNothings :: (Eq x) => Matrix (Maybe x) -> [Matrix (Maybe x)]
replaceByNothings = map (unconcat (sideSize - 1)) . replaceByNothingsHelper . concat
  where
    replaceByNothingsHelper mxs = reverse (replaceByNothingAtMost (length mxs) mxs)
      where
        replaceByNothingAtMost 1 [] = [[]]
        replaceByNothingAtMost 1 (Nothing : mxs) = map (Nothing :) (replaceByNothingAtMost 1 mxs)
        replaceByNothingAtMost 1 ((Just x) : mxs) = (Nothing : mxs) : map (Just x :) (replaceByNothingAtMost 1 mxs)
        replaceByNothingAtMost n mxs = nub (concatMap (replaceByNothingAtMost 1) rec1 ++ rec1)
          where
            rec1 = replaceByNothingAtMost (n - 1) mxs
            rec2 = concatMap (replaceByNothingAtMost 1) rec1

polygonGameCandidates :: [[Matrix (Maybe Integer)]]
polygonGameCandidates = map replaceByNothings uniquePolygonExamplesSolutions

filterCorrectPolygonGames :: [Matrix (Maybe Integer)] -> [Matrix (Maybe Integer)]
filterCorrectPolygonGames = map snd . filter (\(s, g) -> polygonSolutions g == [s]) . zipWithHead

polygonGames :: [Matrix (Maybe Integer)]
polygonGames = concatMap filterCorrectPolygonGames polygonGameCandidates

