module Main where

import MatrixPuzzles (List, MatrixPuzzle, MatrixSolution)
import SudokuSpecifics (sudokuSolver)

sudokuPuzzle :: MatrixPuzzle Char
sudokuPuzzle =
  [ [Nothing, Just '9', Just '8', Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Just '7', Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Just '1', Just '5', Nothing, Nothing, Nothing],
    [Just '1', Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Just '2', Nothing, Nothing, Nothing, Nothing, Just '9'],
    [Nothing, Nothing, Nothing, Just '9', Nothing, Just '6', Nothing, Just '8', Just '2'],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just '3', Nothing],
    [Just '5', Nothing, Just '1', Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Just '4', Nothing, Nothing, Nothing, Just '2', Nothing]
  ]

sudokuSolutions :: List (MatrixSolution Char)
sudokuSolutions = sudokuSolver sudokuPuzzle

sudokuSolutionsOfSudokuPuzzleMain :: IO ()
sudokuSolutionsOfSudokuPuzzleMain =
  do
    putStrLn "sudoku puzzle "
    putStr (unlines (map show sudokuPuzzle))
    putStrLn ("has solutions ")
    putStr (unlines (map (unlines . map show) sudokuSolutions))

main :: IO ()
main = sudokuSolutionsOfSudokuPuzzleMain
