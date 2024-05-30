module Main where

import AllSudokuSolutionsFor (allSudokuSolutionsFor)

sudokuGridSize = 4

allSudokuSolutions = allSudokuSolutionsFor sudokuGridSize

allSudokuSolutionsMain :: IO ()
allSudokuSolutionsMain =
  do
    putStr "for "
    putStr (show sudokuGridSize)
    putStr "x"
    putStr (show sudokuGridSize)
    putStr " sudoku grids with values in [1.."
    putStr (show sudokuGridSize)
    putStrLn "]"
    putStrLn "all the sudoku puzzle solutions are "
    putStr (unlines (map (unlines . map show) allSudokuSolutions))
    putStr "there are "
    putStr (show (length allSudokuSolutions))
    putStrLn " sudoku puzzle solutions"

main :: IO ()
main = allSudokuSolutionsMain
