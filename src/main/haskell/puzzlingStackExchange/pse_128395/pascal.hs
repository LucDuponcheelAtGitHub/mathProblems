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

-- pascalTriangle :: PascalTriangle
-- pascalTriangle =
--   let first :: PascalRow
--       first = [0]
--       nextFunction :: PascalRow -> PascalRow
--       nextFunction zs = [1] ++ zipWith (+) zs (tail zs) ++ [1]
--    in forever nextFunction first

pascalTriangle :: PascalTriangle
pascalTriangle =
  let prefix row = [1]
      operation = (+)
      postfix row = [1]
      row= [0]
   in triangle prefix operation postfix row

type PascalArgument = Pair Int Int

type PascalResult = PascalValue

pascalFunction :: PascalArgument -> PascalResult
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
  let prefix row =
        let (n, _) = fst (head row)
         in [((n + 1, 0), 1)]
      ((n, m), z) `operation` ((n', m'), z') = ((n, m'), z + z')
      postfix row =
        let (n, _) = fst (head row)
         in [((0, n + 1), 1)]
      row = [((0, 0), 0)]
   in triangle prefix operation postfix row

printPascalEntryRowAt :: Int -> IO ()
printPascalEntryRowAt n = printRow (pascalEntryTriangle !! n)

printPascalEntryTriangleToEntryRowAt :: Int -> IO ()
printPascalEntryTriangleToEntryRowAt n = printTriangleToRowAt (n + 1) pascalEntryTriangle