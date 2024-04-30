module SudokuExample where

import MatrixPuzzles ( Matrix, isUniqueSolution )

import SudokuSpecifics ( sudokuSolutions )

sudokuExample01 :: Matrix Char
sudokuExample01 =          [".98......",
                            "....7....",
                            "....15...",
                            "1........",
                            "...2....9",
                            "...9.6.82",
                            ".......3.",
                            "5.1......",
                            "...4...2."]

sudokuExample :: Matrix Char
sudokuExample = sudokuExample01   

uniqueSolution :: Matrix Char -> Matrix Char
uniqueSolution m
  | isUniqueSolution ms = head ms
  | otherwise = error "not a unique solution"
  where
    ms = sudokuSolutions m

uniqueSudokuExampleSolution :: Matrix Char
uniqueSudokuExampleSolution = uniqueSolution sudokuExample

