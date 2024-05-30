module SudokuSpecifics where

import Data.List (transpose)
import MatrixPuzzles (MatrixPuzzleSolver, solver)
import Utilities (chop, reduce, noDupSinglesIn)

sudokuSolver :: MatrixPuzzleSolver Char
sudokuSolver zss = solver noDupSingles pruneNonSingles fail values zss
  where

    values = map (Just . head . show) [1 .. toInteger sudokuSize]

    sudokuSize = length zss

    fail zsss = False

    pruneNonSingles = pruneNonSinglesOf boxs . pruneNonSinglesOf cols . pruneNonSinglesOf rows
      where
        pruneNonSinglesOf f = f . map reduce . f

    rows = id

    cols = transpose

    boxs = unpack . map cols . pack
      where
        pack = split . map split
        split = chop boxSize
        unpack = map concat . concat

    boxSize = intSqrt sudokuSize

    intSqrt = round . sqrt . fromIntegral

    noDupSingles cm =
      all noDupSinglesIn (rows cm)
        && all noDupSinglesIn (cols cm)
        && all noDupSinglesIn (boxs cm)

