module SudokuSpecifics where

import Data.List (transpose)
import MatrixPuzzles (MatrixPuzzleSolver, solver)
import Utilities (chop, reduce, consistent)

sudokuSolver :: MatrixPuzzleSolver Char
sudokuSolver zss = solver safe prune fail values zss
  where

    values = map (Just . head . show) [1 .. toInteger sudokuSize]

    sudokuSize = length zss

    fail zsss = False

    prune = pruneBy boxs . pruneBy cols . pruneBy rows
      where
        pruneBy f = f . map reduce . f

    rows = id

    cols = transpose

    boxs = unpack . map cols . pack
      where
        pack = split . map split
        split = chop boxSize
        unpack = map concat . concat

    boxSize = intSqrt sudokuSize

    intSqrt = round . sqrt . fromIntegral

    safe cm =
      all consistent (rows cm)
        && all consistent (cols cm)
        && all consistent (boxs cm)

