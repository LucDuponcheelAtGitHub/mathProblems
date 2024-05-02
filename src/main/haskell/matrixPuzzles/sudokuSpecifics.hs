module SudokuSpecifics where

import Data.List (transpose)
import MatrixPuzzles
  ( Choices,
    ListOfSuccesses,
    Matrix,
    Row,
    isNoChoice,
    matrixAny,
    matrixOfChoicesToListOfSolutions,
    matrixToMatrixOfChoices,
    rowAny,
    rowHasDuplicates,
    rowMap,
    rowOfChoicesPrune,
    rowSingleChoices,
  )
import Utilities (unconcat)

rowsToRows :: Matrix a -> [Row a]
rowsToRows = id

columnsToRows :: Matrix a -> [Row a]
columnsToRows = transpose

boxesToRows :: Matrix a -> [Row a]
boxesToRows m = (unpack . map columnsToRows . pack) m
  where
    pack = unconcat n . map (unconcat n)
      where
        n = intSqrt (length m)
        intSqrt = round . sqrt . fromIntegral
    unpack = map concat . concat

sudokuMatrixOfChoicesFails :: (Eq a) => Matrix (Choices a) -> Bool
sudokuMatrixOfChoicesFails m =
  matrixAny isNoChoice m
    || rowAny (rowHasDuplicates . rowSingleChoices) (boxesToRows m)
    || rowAny (rowHasDuplicates . rowSingleChoices) (columnsToRows m)
    || rowAny (rowHasDuplicates . rowSingleChoices) (rowsToRows m)

sudokuMatrixOfChoicesPrune :: (Eq a) => Matrix (Choices a) -> Matrix (Choices a)
sudokuMatrixOfChoicesPrune =
  rowsOfChoicesPrune boxesToRows
    . rowsOfChoicesPrune columnsToRows
    . rowsOfChoicesPrune rowsToRows
  where
    rowsOfChoicesPrune f = f . rowMap rowOfChoicesPrune . f

sudokuChoices :: Choices Char
sudokuChoices = ['1' .. '9']

sudokuNoChoice :: Char -> Bool
sudokuNoChoice = (== '.')

type SudokuPuzzle = Matrix Char

type SudokuSolution = Matrix Char

sudokuSolutions :: SudokuPuzzle -> ListOfSuccesses SudokuSolution
sudokuSolutions =
  matrixOfChoicesToListOfSolutions sudokuMatrixOfChoicesFails sudokuMatrixOfChoicesPrune
    . matrixToMatrixOfChoices sudokuChoices sudokuNoChoice
