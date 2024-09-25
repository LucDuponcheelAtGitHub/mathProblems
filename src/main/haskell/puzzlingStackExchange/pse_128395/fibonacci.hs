module Fibonacci where

import Types

import Functions

import IO
--
-- bonus: fibonacci
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

printFibonacciRowToValueAt :: Int -> IO ()
printFibonacciRowToValueAt n = printRowToValueAt (n + 1) fibonacciRow