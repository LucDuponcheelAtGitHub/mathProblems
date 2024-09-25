module Pascal where

import Types

import Functions

import IO
--
-- pascal
--

type PascalValue = Value Int

type PascalRow = Row PascalValue

type PascalTriangle = Row PascalRow

pascalTriangle :: PascalTriangle
pascalTriangle =
  let first :: PascalRow
      first = [0]
      nextFunction :: PascalRow -> PascalRow
      nextFunction zs = [1] ++ zipWith (+) zs (tail zs) ++ [1]
   in forever nextFunction first

type PascalArgument = Pair Int Int

type PascalResult = PascalValue

pascalFunction :: PascalArgument ->  PascalResult
pascalFunction (n, m) = pascalTriangle !! n !! m

printPascalTriangleToRowAt :: Int -> IO ()
printPascalTriangleToRowAt n = printTriangleToRowAt (n + 1) pascalTriangle

--
-- pascal entries
--

type PascalEntry = Entry PascalArgument PascalResult

type PascalEntryRow = Row PascalEntry

type PascalEntryTriangle = Triangle PascalEntry

pascalEntryTriangle :: PascalEntryTriangle
pascalEntryTriangle =
  let first :: PascalEntryRow
      first = [((0, 0), 0)]
      nextFunction :: PascalEntryRow -> PascalEntryRow
      nextFunction zs =
        let (n, _) = fst (head zs)
            ((n, m), z) +++ ((n', m'), z') = ((n, m'), z + z')
         in [((n + 1, 0), 1)] ++ zipWith (+++) zs (tail zs) ++ [((0, n + 1), 1)]
   in forever nextFunction first

printPascalEntryRowAt :: Int -> IO ()
printPascalEntryRowAt n = printRow (pascalEntryTriangle !! n)

printPascalEntryTriangleToEntryRowAt :: Int -> IO ()
printPascalEntryTriangleToEntryRowAt n = printTriangleToRowAt (n + 1) pascalEntryTriangle