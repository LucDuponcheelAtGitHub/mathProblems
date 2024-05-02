module SudokuPuzzle where

import MatrixPuzzles ( Matrix, isUniqueSolution )

import SudokuSpecifics ( SudokuPuzzle, SudokuSolution, sudokuSolutions )

sudokuPuzzle01 :: Matrix Char
sudokuPuzzle01 =          [".98......",
                            "....7....",
                            "....15...",
                            "1........",
                            "...2....9",
                            "...9.6.82",
                            ".......3.",
                            "5.1......",
                            "...4...2."]

sudokuPuzzle :: SudokuPuzzle
sudokuPuzzle = sudokuPuzzle01   

uniqueSolution :: SudokuPuzzle -> SudokuSolution
uniqueSolution m
  | isUniqueSolution ms = head ms
  | otherwise = error "not a unique solution"
  where
    ms = sudokuSolutions m

uniqueSudokuPuzzleSolution :: SudokuSolution
uniqueSudokuPuzzleSolution = uniqueSolution sudokuPuzzle

