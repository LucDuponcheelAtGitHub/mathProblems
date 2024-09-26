module Fibonacci where

import Functions
import IO
import Types

--
-- fibonacci
--

type FibonacciValue = Value Int

type FibonacciRow = Row FibonacciValue

type FibonacciPair = Pair FibonacciValue FibonacciValue

fibonacciRow :: FibonacciRow
fibonacciRow =
  let first :: FibonacciPair
      first = (0, 1)
      nextFunction :: FibonacciPair -> FibonacciPair
      nextFunction (z, z') = (z', z + z')
   in map fst (forever nextFunction first)

type FibonacciArgument = Int

type FibonacciResult = FibonacciValue

fibonacciFunction :: FibonacciArgument -> FibonacciResult
fibonacciFunction n = fibonacciRow !! n

fibonacciRowToValueAt :: Int -> FibonacciRow
fibonacciRowToValueAt n = take (n + 1) fibonacciRow

printFibonacciRowToValueAt :: Int -> IO ()
printFibonacciRowToValueAt = printRow . fibonacciRowToValueAt