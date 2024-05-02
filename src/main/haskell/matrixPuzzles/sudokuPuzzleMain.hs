module Main where

import SudokuPuzzle (sudokuPuzzle, uniqueSudokuPuzzleSolution)

sudokuPuzzleMain :: IO ()
sudokuPuzzleMain =
  do
    putStrLn (unlines sudokuPuzzle)
    putStrLn (unlines uniqueSudokuPuzzleSolution)

main :: IO ()
main = sudokuPuzzleMain
