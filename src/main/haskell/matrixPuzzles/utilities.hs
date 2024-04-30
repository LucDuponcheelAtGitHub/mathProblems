module Utilities where

-- TODO: maybe also listOfListProduct

listOfListCombinations :: Eq a => [[a]] -> [[a]]
listOfListCombinations [] = [[]]
listOfListCombinations (xs:xss) = 
  [y:ys | y <- xs, ys <- listOfListCombinations [ [ x | x <- xs, x /= y ] | xs <- xss ] ]

unconcat :: Int -> [a] -> [[a]]
unconcat n [] = []
unconcat n xs = take n xs : unconcat n (drop n xs)

zipWithHead :: [a] -> [(a, a)]
zipWithHead [] = []
zipWithHead (x : xs) = (x, x) : zip (replicate (length xs) x) xs