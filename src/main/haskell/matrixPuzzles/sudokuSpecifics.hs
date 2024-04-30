module SudokuSpecifics where


import Data.List (transpose)
import Utilities (unconcat)
import MatrixPuzzles
  ( Choices,
    ListOfSuccesses,
    Matrix,
    Row,
    isNoChoice,
    isUniqueSolution,
    rowAny,
    rowMap,
    rowHasDuplicates,
    rowSingleChoices,
    rowOfChoicesPrune,
    matrixAny,
    matrixToMatrixOfChoices,
    matrixOfChoicesToListOfSolutions,
  )

rowsToRows :: Matrix a -> [Row a]
rowsToRows = id

columnsToRows :: Matrix a -> [Row a]
columnsToRows = transpose

-- concat :: [Row a] => Row a
-- concat [] = []
-- concat (xs : xss) = xs ++ concat xss

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
    || rowAny (rowHasDuplicates . rowSingleChoices) (rowsToRows m)
    || rowAny (rowHasDuplicates . rowSingleChoices) (columnsToRows m)
    || rowAny (rowHasDuplicates . rowSingleChoices) (boxesToRows m)

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

sudokuSolutions :: Matrix Char -> ListOfSuccesses (Matrix Char)
sudokuSolutions =
  matrixOfChoicesToListOfSolutions sudokuMatrixOfChoicesFails sudokuMatrixOfChoicesPrune
    . matrixToMatrixOfChoices sudokuChoices sudokuNoChoice

