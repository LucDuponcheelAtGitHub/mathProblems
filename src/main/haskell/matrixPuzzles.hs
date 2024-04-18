--------------------
-- Matrix Puzzles --
--------------------

import Data.List ( transpose, (\\), zipWith3 )

-- general definitions

listOfListProduct :: [[a]] -> [[a]]
listOfListProduct [] = [[]]
listOfListProduct  (xs:xss) = [y:ys | y <- xs, ys <- listOfListProduct xss]

listOfListCombinations :: Eq a => [[a]] -> [[a]]
listOfListCombinations [] = [[]]
listOfListCombinations (xs:xss) = 
  [y:ys | y <- xs, ys <- listOfListCombinations (map (filter (/= y)) xss)]

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

 -- framework declarations

type Choices a = [a]

type ListOfSuccesses a = [a]

type Matrix a = [Row a]

type Row a = [a]

 -- framework definitions

isSingle :: Choices a -> Bool
isSingle [_] = True
isSingle cs = False

isNotSingle :: Choices a -> Bool
isNotSingle = not . isSingle

isEmpty :: Choices a -> Bool
isEmpty = null

rowAny :: (a -> Bool) -> Row a -> Bool
rowAny = any

rowMap :: (a -> b) -> Row a -> Row b
rowMap = map

rowHasDuplicates :: Eq a => Row a -> Bool
rowHasDuplicates [] = False
rowHasDuplicates (x:xs) = elem x xs || rowHasDuplicates xs

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

rowSingles :: Row (Choices a) -> Choices a
rowSingles = concat . filter isSingle

rowOfChoicesPrune :: Eq a => Row (Choices a) -> Row (Choices a)
rowOfChoicesPrune csr = [if isSingle cs then cs else cs \\ rowSingles csr | cs <- csr]

matrixToMatrixOfChoices :: Choices a -> (a -> Bool) -> Matrix a -> Matrix (Choices a)
matrixToMatrixOfChoices choices isEmpty = matrixMap toChoices
  where
    toChoices v = if isEmpty v then choices else [v]

matrixOfChoicesToListOfSuccessfulMatricesStep
  :: Matrix (Choices a) -> ListOfSuccesses (Matrix (Choices a))
matrixOfChoicesToListOfSuccessfulMatricesStep m =
  [csm1 ++ [csr1 ++ [[x]] ++ csr2] ++ csm2 | x <- cs]
    where (csm1,csr1,cs,csr2,csm2) = matrixSpan isSingle m

matrixOfChoicesToListOfSuccessfulMatrices
 :: Matrix (Choices a) -> ListOfSuccesses (Matrix a)
matrixOfChoicesToListOfSuccessfulMatrices =
  listOfListProduct . map listOfListProduct

matrixOfChoicesToListOfSolutions
 :: Eq a =>
     (Matrix (Choices a) -> Bool) ->
     (Matrix (Choices a) -> Matrix (Choices a)) ->
     Matrix (Choices a) -> ListOfSuccesses (Matrix a)
matrixOfChoicesToListOfSolutions
 matrixOfChoicesFails
 matrixOfChoicesPrune =
  prunedMatrixOfChoicesToListOfSolutions . matrixOfChoicesPrune
  where
   prunedMatrixOfChoicesToListOfSolutions m
     | matrixOfChoicesFails m = []
     | matrixAny isNotSingle m = matrixOfChoicesToListOfSolutionsStep m
     | otherwise = matrixOfChoicesToListOfSuccessfulMatrices m
   matrixOfChoicesToListOfSolutionsStep m =
    [m | 
     csm <- matrixOfChoicesToListOfSuccessfulMatricesStep m,
     m <- matrixOfChoicesToListOfSolutions
            matrixOfChoicesFails
            matrixOfChoicesPrune csm]

-- sudoku definitions

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
    unconcat n [] = []
    unconcat n xs = [take n xs] ++ unconcat n (drop n xs)
    concat [] = []
    concat (xs:xss) = xs ++ concat xss

sudokuMatrixOfChoicesFails :: Eq a => Matrix (Choices a) -> Bool
sudokuMatrixOfChoicesFails m =
   matrixAny isEmpty m ||
   rowAny (rowHasDuplicates . rowSingles) (rowsToRows m) ||
   rowAny (rowHasDuplicates . rowSingles) (columnsToRows m) ||
   rowAny (rowHasDuplicates . rowSingles) (boxesToRows m)

sudokuMatrixOfChoicesPrune :: Eq a => Matrix (Choices a) -> Matrix (Choices a)
sudokuMatrixOfChoicesPrune =
 rowsOfChoicesPrune boxesToRows .
 rowsOfChoicesPrune columnsToRows.
 rowsOfChoicesPrune rowsToRows
  where
    rowsOfChoicesPrune f = f . rowMap rowOfChoicesPrune . f

sudokuChoices :: Choices (Char)
sudokuChoices = ['1' .. '9']

sudokuHasNoValue :: Char -> Bool
sudokuHasNoValue = (==' ')

sudokuSolutions :: Matrix Char -> ListOfSuccesses (Matrix Char)
sudokuSolutions =
 matrixOfChoicesToListOfSolutions sudokuMatrixOfChoicesFails sudokuMatrixOfChoicesPrune .
 matrixToMatrixOfChoices sudokuChoices sudokuHasNoValue

-- polygon definitions

matrixSingles :: Matrix (Choices a) -> Row a
matrixSingles = rowSingles . concat

polygonMatrixOfChoicesFails :: (Eq a, Num a) => Matrix (Choices a) -> Bool
polygonMatrixOfChoicesFails m =
  matrixAny isEmpty m ||
  (rowHasDuplicates . matrixSingles) m ||
  rowAny (not . (elem productOfFirstRow)) productsOfOtherRowsOfChoices
    where
      productsOfRowsOfChoices = map (map product) choicesRows
      choicesRows = map listOfListProduct rowsOfChoices
      rowsOfChoices = zipWith (\r x -> r ++ [x]) m (rowMap head (tail m) ++ [head (head m)])
      productOfFirstRow = head (head productsOfRowsOfChoices)
      productsOfOtherRowsOfChoices = tail productsOfRowsOfChoices

polygonMatrixOfChoicesPrune :: Eq a => Matrix (Choices a) -> Matrix (Choices a)
polygonMatrixOfChoicesPrune = rowMap rowOfChoicesPrune

polygonChoices :: Choices (Maybe Integer)
polygonChoices = map Just [1 .. 12]

polygonHasNoValue :: Maybe Integer -> Bool
polygonHasNoValue = (== Nothing)

polygonSolutions :: Matrix (Maybe Integer) -> ListOfSuccesses (Matrix (Maybe Integer))
polygonSolutions =
  matrixOfChoicesToListOfSolutions polygonMatrixOfChoicesFails polygonMatrixOfChoicesPrune .
  matrixToMatrixOfChoices polygonChoices polygonHasNoValue

-- sudoku example

sudokuExample :: Matrix Char
sudokuExample =          [" 98      ",
                          "    7    ",
                          "    15   ",
                          "1        ",
                          "   2    9",
                          "   9 6 82",
                          "       3 ",
                          "5 1      ",
                          "   4   2 "]

allSudokuSolutions :: ListOfSuccesses (Matrix Char)
allSudokuSolutions = sudokuSolutions sudokuExample

sudokuMain :: IO ()
sudokuMain =
 do
    putStrLn (unlines (sudokuExample))
    putStrLn (unlines (head allSudokuSolutions))

-- polygon example(s)

polygonExamples :: Int -> Int -> Choices (Maybe Integer) -> [Matrix (Maybe Integer)]
polygonExamples ps ss cs =
  zipWith3
   (\xs ys zss -> xs : ys : zss)
   (map init combinations)
   (map second combinations)
   (replicate (length combinations) others)
  where
    combinations = listOfListCombinations (replicate ss cs)
    second = (\x -> [x] ++ (replicate (ss - 2) Nothing)) . last
    others = replicate (ps - 2) (replicate (ss - 1) Nothing)

polygonSize :: Int
polygonSize = 3

sideSize :: Int
sideSize = 3

polygonExample :: [Matrix (Maybe Integer)]
polygonExample = polygonExamples polygonSize sideSize polygonChoices

allPolygonSolutions :: ListOfSuccesses (Matrix (Maybe Integer))
allPolygonSolutions = concat (map polygonSolutions polygonExample)

polygonMain :: IO ()
polygonMain =
  do
    putStr "polygon with 3 vertices with sides of 3 values in [1..12] has "
    putStr (show (length (allPolygonSolutions)))
    putStrLn " solutions"
    putStrLn "where the first 10 solutons are "
    putStrLn (unlines (map show (take 10 allPolygonSolutions)))

-- main(s)

main :: IO ()
-- main = sudokuMain
main = polygonMain
