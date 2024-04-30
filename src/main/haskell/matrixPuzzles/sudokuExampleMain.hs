module Main where

import SudokuExample (sudokuExample, uniqueSudokuExampleSolution)

sudokuExampleMain :: IO ()
sudokuExampleMain =
 do
    putStrLn (unlines sudokuExample)
    putStrLn (unlines uniqueSudokuExampleSolution)

main :: IO ()
main = sudokuExampleMain
