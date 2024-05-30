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
solver noDupSingles pruneNonSingles fail values = pruneAndThenSearch . choices
  where
    choices = map (map choice)

    choice mz = if empty mz then values else [mz]

    empty = (== Nothing)

    pruneAndThenSearch = search . pruneNonSingles

    search mcmz
      | blocked mcmz || fail mcmz = []
      | complete mcmz = collapse mcmz
      | otherwise = [mcmz'' | mcmz' <- expand mcmz, mcmz'' <- pruneAndThenSearch mcmz']

    blocked mcmz = void mcmz || not (noDupSingles mcmz)
    
    void = any (any null)

    complete = all (all single)

    collapse = cp . map cp

    expand mcmz =
      [mcmz1 ++ [lcmz1 ++ [cmz2] : tlcmz2] ++ tmcmz2 | cmz2 <- hlcmz2]
      where
        (mcmz1, hmcmz2 : tmcmz2) = break (any (not . single)) mcmz
        (lcmz1, hlcmz2 : tlcmz2) = break (not . single) hmcmz2
