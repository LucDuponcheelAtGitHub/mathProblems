module AllSudokuSolutionsFor where

import MatrixPuzzles (List, MatrixPuzzle, MatrixSolution)
import SudokuSpecifics (sudokuSolver)

emptySudokuPuzzle :: Int -> MatrixPuzzle Char
emptySudokuPuzzle sudokuSize = replicate sudokuSize (replicate sudokuSize Nothing)

allSudokuSolutionsFor :: Int -> List (MatrixSolution Char)
allSudokuSolutionsFor sudokuSize = sudokuSolver (emptySudokuPuzzle sudokuSize)
