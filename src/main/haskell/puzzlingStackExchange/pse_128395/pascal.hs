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
  let prefix row = [1]
      operation = (+)
      postfix row = [1]
      topRow = [0]
   in triangle prefix operation postfix topRow

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
  let prefix row =
        let (n, _) = fst (head row)
         in [((n + 1, 0), 1)]
      (_, _) `operation` (z, z') = z + z'
      postfix row =
        let (n, _) = fst (head row)
         in [((0, n + 1), 1)]
      topRow = [((0, 0), 0)]
   in entryTriangle prefix operation postfix topRow

pascalEntryRowAt :: Int -> PascalEntryRow
pascalEntryRowAt n = (!! n) pascalEntryTriangle

pascalEntryTriangleToEntryRowAt :: Int -> PascalEntryTriangle
pascalEntryTriangleToEntryRowAt n = take (n + 1) pascalEntryTriangle

printPascalEntryRowAt :: Int -> IO ()
printPascalEntryRowAt = printRow . pascalEntryRowAt

printPascalEntryTriangleToEntryRowAt :: Int -> IO ()
printPascalEntryTriangleToEntryRowAt = printTriangle . pascalEntryTriangleToEntryRowAt