module MatrixPuzzles where

import Data.List ( (\\) )

type Choices a = [a]

type ListOfSuccesses a = [a]

type Matrix a = [Row a]

type Row a = [a]

 -- framework definitions

isSingleChoice :: Choices a -> Bool
isSingleChoice [_] = True
isSingleChoice cs = False

isNotSingleChoice :: Choices a -> Bool
isNotSingleChoice = not . isSingleChoice

isNoChoice :: Choices a -> Bool
isNoChoice = null

isUniqueSolution :: ListOfSuccesses a -> Bool
isUniqueSolution [_] = True
isUniqueSolution cs = False

rowAny :: (a -> Bool) -> Row a -> Bool
rowAny = any

rowMap :: (a -> b) -> Row a -> Row b
rowMap = map

rowHasDuplicates :: Eq a => Row a -> Bool
rowHasDuplicates [] = False
rowHasDuplicates (x:xs) = elem x xs || rowHasDuplicates xs

rowSingleChoices :: Row (Choices a) -> Choices a
rowSingleChoices = concat . filter isSingleChoice

rowOfChoicesPrune :: Eq a => Row (Choices a) -> Row (Choices a)
rowOfChoicesPrune csr = [if isSingleChoice cs then cs else cs \\ rowSingleChoices csr | cs <- csr]

matrixAny :: (a -> Bool) -> Matrix a -> Bool
matrixAny = any . rowAny

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

matrixToMatrixOfChoices :: Choices a -> (a -> Bool) -> Matrix a -> Matrix (Choices a)
matrixToMatrixOfChoices choices noChoice = matrixMap toChoices
  where
    toChoices v = if noChoice v then choices else [v]

matrixOfChoicesToListOfSuccessfulMatricesStep
  :: Matrix (Choices a) -> ListOfSuccesses (Matrix (Choices a))
matrixOfChoicesToListOfSuccessfulMatricesStep m =
  [csm1 ++ [csr1 ++ [[x]] ++ csr2] ++ csm2 | x <- cs]
    where (csm1,csr1,cs,csr2,csm2) = matrixSpan isSingleChoice m

rowOfChoicesToListOfSuccessfulRows
 :: Row (Choices a) -> ListOfSuccesses (Row a)
rowOfChoicesToListOfSuccessfulRows [] = [[]]
rowOfChoicesToListOfSuccessfulRows  (xs:xss) = [y:ys | y <- xs, ys <- rowOfChoicesToListOfSuccessfulRows xss]

matrixOfChoicesToListOfSuccessfulMatrices
 :: Matrix (Choices a) -> ListOfSuccesses (Matrix a)
matrixOfChoicesToListOfSuccessfulMatrices =
  rowOfChoicesToListOfSuccessfulRows . map rowOfChoicesToListOfSuccessfulRows

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
   prunedMatrixOfChoicesToListOfSolutions :: Matrix (Choices a) -> ListOfSuccesses (Matrix a)
   prunedMatrixOfChoicesToListOfSolutions m
     | matrixOfChoicesFails m = []
     | matrixAny isNotSingleChoice m = matrixOfChoicesToListOfSolutionsStep m
     | otherwise = matrixOfChoicesToListOfSuccessfulMatrices m
   matrixOfChoicesToListOfSolutionsStep:: Matrix (Choices a) -> ListOfSuccesses (Matrix a) 
   matrixOfChoicesToListOfSolutionsStep m =
    [m | 
     csm <- matrixOfChoicesToListOfSuccessfulMatricesStep m,
     m <- matrixOfChoicesToListOfSolutions
            matrixOfChoicesFails
            pruneMatrixOfChoices csm]