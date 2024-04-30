module PolygonSpecifics where

import Utilities ( unconcat )

import MatrixPuzzles
  ( Choices,
    ListOfSuccesses,
    Matrix,
    Row,
    isNoChoice,
    matrixAny,
    matrixMap,
    matrixOfChoicesToListOfSolutions,
    matrixToMatrixOfChoices,
    rowAny,
    rowHasDuplicates,
    rowMap,
    rowOfChoicesPrune,
    rowOfChoicesToListOfSuccessfulRows,
    rowSingleChoices,
  )

matrixSingleChoices :: Matrix (Choices a) -> Row a
matrixSingleChoices = rowSingleChoices . concat

instance Num (Maybe Integer) where
  negate (Just x) = Just (-x)
  negate _ = Nothing
  (+) (Just x) (Just y) = Just (x + y)
  (+) _ _ = Nothing
  (*) (Just x) (Just y) = Just (x * y)
  (*) _ _ = Nothing
  abs (Just x) = Just (abs x)
  abs _ = Nothing
  signum (Just x)
    | x >= 0 = Just 1
    | otherwise = Just (-1)
  signum _ = Nothing
  fromInteger = Just

polygonMatrixOfChoicesFails :: (Eq a, Num a) => Matrix (Choices a) -> Bool
polygonMatrixOfChoicesFails m =
  matrixAny isNoChoice m
    || (rowHasDuplicates . matrixSingleChoices) m
    || rowAny (notElem productOfFirstRow) productsOfOtherRowsOfChoices
  where
    productsOfRowsOfChoices = map (map product) choicesRows
    choicesRows = map rowOfChoicesToListOfSuccessfulRows rowsOfChoices
    rowsOfChoices = zipWith (\r x -> r ++ [x]) m (rowMap head (tail m) ++ [head (head m)])
    productOfFirstRow = head (head productsOfRowsOfChoices)
    productsOfOtherRowsOfChoices = tail productsOfRowsOfChoices

polygonMatrixOfChoicesPrune :: (Eq a) => Matrix (Choices a) -> Matrix (Choices a)
polygonMatrixOfChoicesPrune = rowMap rowOfChoicesPrune

polygonChoices :: Choices (Maybe Integer)
polygonChoices = map Just [1 .. 20]

polygonNoChoice :: Maybe Integer -> Bool
polygonNoChoice = (== Nothing)

polygonSolutions :: Matrix (Maybe Integer) -> ListOfSuccesses (Matrix (Maybe Integer))
polygonSolutions =
  matrixOfChoicesToListOfSolutions polygonMatrixOfChoicesFails polygonMatrixOfChoicesPrune
    . matrixToMatrixOfChoices polygonChoices polygonNoChoice
