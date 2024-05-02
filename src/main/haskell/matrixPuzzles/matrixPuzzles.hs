module MatrixPuzzles where

import Data.List ( (\\) )

import Utilities ( isUnique, listOfListsProduct )

type Choices a = [a]

type ListOfSuccesses a = [a]

type Matrix a = [Row a]

type Row a = [a]

isUniqueChoice :: Choices a -> Bool
isUniqueChoice = isUnique

isNotUniqueChoice :: Choices a -> Bool
isNotUniqueChoice = not . isUniqueChoice

-- external

isNoChoice :: Choices a -> Bool
isNoChoice = null

isUniqueSolution :: ListOfSuccesses a -> Bool
isUniqueSolution = isUnique

matrixAny :: (a -> Bool) -> Matrix a -> Bool
matrixAny = any . rowAny

matrixOfChoicesToListOfSolutions
 :: Eq a =>
     (Matrix (Choices a) -> Bool) ->
     (Matrix (Choices a) -> Matrix (Choices a)) ->
     Matrix (Choices a) -> ListOfSuccesses (Matrix a)
matrixOfChoicesToListOfSolutions
 matrixOfChoicesFails
 pruneMatrixOfChoices =
  prunedMatrixOfChoicesToListOfSolutions . pruneMatrixOfChoices
  where
   prunedMatrixOfChoicesToListOfSolutions m
     | matrixOfChoicesFails m = []
     | matrixAny isNotUniqueChoice m = matrixOfChoicesToListOfSolutionsStep m
     | otherwise = matrixOfChoicesToListOfSuccessfulMatrices m
   matrixOfChoicesToListOfSolutionsStep m =
    [m | 
     csm <- matrixOfChoicesToListOfSuccessfulMatricesStep m,
     m <- matrixOfChoicesToListOfSolutions
            matrixOfChoicesFails
            pruneMatrixOfChoices csm]

matrixToMatrixOfChoices :: Choices a -> (a -> Bool) -> Matrix a -> Matrix (Choices a)
matrixToMatrixOfChoices choices noChoice = matrixMap toChoices
  where
    toChoices v = if noChoice v then choices else [v]

rowAny :: (a -> Bool) -> Row a -> Bool
rowAny = any

rowHasDuplicates :: Eq a => Row a -> Bool
rowHasDuplicates [] = False
rowHasDuplicates (x:xs) = elem x xs || rowHasDuplicates xs

rowMap :: (a -> b) -> Row a -> Row b
rowMap = map

rowOfChoicesPrune :: Eq a => Row (Choices a) -> Row (Choices a)
rowOfChoicesPrune csr = [if isUniqueChoice cs then cs else cs \\ rowSingleChoices csr | cs <- csr]

rowOfChoicesToListOfSuccessfulRows :: Row (Choices a) -> ListOfSuccesses (Row a)
rowOfChoicesToListOfSuccessfulRows = listOfListsProduct

rowSingleChoices :: Row (Choices a) -> Choices a
rowSingleChoices = concat . filter isUniqueChoice

matrixMap :: (a -> b) -> Matrix a -> Matrix b
matrixMap = map . rowMap

matrixSpan :: (a -> Bool) -> [[a]] -> (Matrix a, Row a, a, Row a, Matrix a)
matrixSpan p m = (m1,r1,x,r2,m2)
  where
    (m1,r,m2) = rowSpan (all p) m
    (r1,x,r2) = rowSpan p r
    rowSpan p xs = (xs1,x,xs2)
      where
        xs1 = takeWhile p xs
        x:xs2 = dropWhile p xs

matrixOfChoicesToListOfSuccessfulMatricesStep
  :: Matrix (Choices a) -> ListOfSuccesses (Matrix (Choices a))
matrixOfChoicesToListOfSuccessfulMatricesStep m =
  [csm1 ++ [csr1 ++ [[x]] ++ csr2] ++ csm2 | x <- cs]
    where (csm1,csr1,cs,csr2,csm2) = matrixSpan isUniqueChoice m

matrixOfChoicesToListOfSuccessfulMatrices
 :: Matrix (Choices a) -> ListOfSuccesses (Matrix a)
matrixOfChoicesToListOfSuccessfulMatrices =
  rowOfChoicesToListOfSuccessfulRows . map rowOfChoicesToListOfSuccessfulRows
