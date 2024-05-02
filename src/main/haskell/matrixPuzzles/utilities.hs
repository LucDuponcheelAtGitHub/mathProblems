module Utilities where

isUnique :: [a] -> Bool
isUnique [_] = True
isUnique cs = False

listOfListsProduct :: [[a]] -> [[a]]
listOfListsProduct [] = [[]]
listOfListsProduct  (xs:xss) = [y:ys | y <- xs, ys <- listOfListsProduct xss]

listOfListsCombinations :: Eq a => [[a]] -> [[a]]
listOfListsCombinations [] = [[]]
listOfListsCombinations (xs:xss) = 
  [y:ys | y <- xs, ys <- listOfListsCombinations [ [ x | x <- xs, x /= y ] | xs <- xss ] ]

unconcat :: Int -> [a] -> [[a]]
unconcat n [] = []
unconcat n xs = take n xs : unconcat n (drop n xs)

zipWithHead :: [a] -> [(a, a)]
zipWithHead [] = []
zipWithHead (x : xs) = (x, x) : zip (replicate (length xs) x) xs
