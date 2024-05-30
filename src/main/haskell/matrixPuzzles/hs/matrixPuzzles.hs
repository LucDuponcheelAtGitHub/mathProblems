module MatrixPuzzles where

import Data.List ((\\))
import Utilities (cp, single)

type List z = [z]

type Matrix z = List (List z)

type MatrixPuzzle z = Matrix (Maybe z)

type MatrixSolution z = Matrix (Maybe z)

type MatrixPuzzleSolver z = MatrixPuzzle z -> List (MatrixSolution z)

type Choices z = List z

solver ::
  (Eq z) =>
  (Matrix (Choices (Maybe z)) -> Bool) ->
  (Matrix (Choices (Maybe z)) -> Matrix (Choices (Maybe z))) ->
  (Matrix (Choices (Maybe z)) -> Bool) ->
  (Choices (Maybe z) -> MatrixPuzzleSolver z)
solver noDuplicateSingles pruneNonSingles fail values = pruneNonSinglesAndThenSearch . choices
  where
    choices = map (map choice)
      where
        choice mz = if mz == Nothing then values else [mz]

    pruneNonSinglesAndThenSearch = search . pruneNonSingles
      where
        search mcmz
          | blocked mcmz || fail mcmz = stop
          | complete mcmz = continue mcmz
          | otherwise = finish mcmz

        blocked mcmz = void mcmz || not (noDuplicateSingles mcmz)
          where
            void = any (any null)

        stop = []

        complete = all (all single)

        continue = cp . map cp

        finish mcmz =
          [ mcmz''
            | mcmz' <- expandFirstNonSingle mcmz,
              mcmz'' <- pruneNonSinglesAndThenSearch mcmz'
          ]
          where
            expandFirstNonSingle mcmz =
              [mcmz1 ++ [lcmz1 ++ [cmz2] : tlcmz2] ++ tmcmz2 | cmz2 <- hlcmz2]
              where
                (mcmz1, hmcmz2 : tmcmz2) = break (any (not . single)) mcmz
                (lcmz1, hlcmz2 : tlcmz2) = break (not . single) hmcmz2
