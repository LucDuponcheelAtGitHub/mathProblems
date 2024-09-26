module Pascal where

import Functions
import IO
import Types

--
-- pascal
--

type PascalValue = Value Int

type PascalRow = Row PascalValue

type PascalTriangle = Row PascalRow

pascalTriangle :: PascalTriangle
pascalTriangle =
  let operation = (+)
      prefix row = [1]
      postfix row = [1]
      topRow = [0]
   in triangle operation prefix postfix topRow

pascalRowAt :: Int -> PascalRow
pascalRowAt n = (!! n) pascalTriangle

pascalTriangleToRowAt :: Int -> PascalTriangle
pascalTriangleToRowAt n = take (n + 1) pascalTriangle

printPascalRowAt :: Int -> IO ()
printPascalRowAt = printRow . pascalRowAt

printPascalTriangleToRowAt :: Int -> IO ()
printPascalTriangleToRowAt = printTriangle . pascalTriangleToRowAt

--
-- pascal entries
--

type PascalEntry = Entry (Int, Int) Int

type PascalEntryRow = Row PascalEntry

type PascalEntryTriangle = Triangle PascalEntry

pascalEntryTriangle :: PascalEntryTriangle
pascalEntryTriangle =
  let (_, _) `operation` (z, z') = z + z'
      prefix row = [((fst (fst (head row)) + 1, 0), 1)]
      postfix row = [((0, fst (fst (head row)) + 1 + 1), 1)]
      topRow = [((0, 0), 0)]
   in entryTriangle operation prefix postfix topRow

pascalEntryRowAt :: Int -> PascalEntryRow
pascalEntryRowAt n = (!! n) pascalEntryTriangle

pascalEntryTriangleToEntryRowAt :: Int -> PascalEntryTriangle
pascalEntryTriangleToEntryRowAt n = take (n + 1) pascalEntryTriangle

printPascalEntryRowAt :: Int -> IO ()
printPascalEntryRowAt = printRow . pascalEntryRowAt

printPascalEntryTriangleToEntryRowAt :: Int -> IO ()
printPascalEntryTriangleToEntryRowAt = printTriangle . pascalEntryTriangleToEntryRowAt