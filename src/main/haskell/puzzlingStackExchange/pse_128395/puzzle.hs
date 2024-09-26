module Puzzle where

import Functions
import IO
import Types

--
-- puzzle entres
--

type PuzzleEntry = Entry (Int, Int) Rational

type PuzzleEntryRow = Row PuzzleEntry

type PuzzleEntryTriangle = Triangle PuzzleEntry

puzzleEntryTriangle :: PuzzleEntryTriangle
puzzleEntryTriangle =
  let (n', m') `operation` (z, z') =
        let rm' = fromIntegral m' -- necessary conversion to Rational
            rn' = fromIntegral n' + 1 -- necessary conversion to Rational
            rs' = rn' + rm'
            y
              | n' + 1 < m' =
                  let p = rm' / rs'
                   in p + (1 - p) * z' + p * z
              | n' + 1 > m' =
                  let p = rn' / rs'
                   in p + p * z' + (1 - p) * z
              | m' == n' + 1 =
                  let p = 1 / 2
                   in p + p * z' + p * z
         in y
      prefix row =
        let (n, _) = fst (head row)
         in [((n + 1, 0), fromIntegral (n + 1))]
      postfix row =
        let (n, _) = fst (head row)
         in [((0, n + 1), fromIntegral (n + 1))]
      topRow = [((0, 0), 0)]
   in entryTriangle operation prefix postfix topRow

puzzleEntryRowAt :: Int -> PuzzleEntryRow
puzzleEntryRowAt n = (!! n) puzzleEntryTriangle

puzzleEntryTriangleToEntryRowAt :: Int -> PuzzleEntryTriangle
puzzleEntryTriangleToEntryRowAt n = take (n + 1) puzzleEntryTriangle

printPuzzleEntryRowAt :: Int -> IO ()
printPuzzleEntryRowAt = mapM_ print . puzzleEntryRowAt

printPuzzleEntryTriangleToEntryRowAt :: Int -> IO ()
printPuzzleEntryTriangleToEntryRowAt = printTriangle . puzzleEntryTriangleToEntryRowAt

solution :: (Int, Double) -> ((Int, Int), (Rational, Double))
solution (n, d) =
  let convert = map (map (\((n, m), s) -> ((n, m), (s, fromRational s))))
      successMoreThan d ((_, _), (_, s)) = s > d
   in (last . takeWhile (successMoreThan d) . (!! n) . convert) puzzleEntryTriangle

printSolution :: (Int, Double) -> IO ()
printSolution = print . solution
