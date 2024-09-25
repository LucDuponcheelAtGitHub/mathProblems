module Puzzle where

import Types

import Functions

import IO

--
-- puzzle entres
--

type PuzzleArgument = Pair Int Int

type PuzzleResult = Rational

type PuzzleEntry = Entry PuzzleArgument PuzzleResult

type PuzzleEntryRow = Row PuzzleEntry

type PuzzleEntryTriangle = Triangle PuzzleEntry

puzzleEntryTriangle :: PuzzleEntryTriangle
puzzleEntryTriangle =
  let first :: PuzzleEntryRow
      first = [((0, 0), 0)]
      nextFunction :: PuzzleEntryRow -> PuzzleEntryRow
      nextFunction zs =
        let (n, _) = fst (head zs)
            (+++) :: PuzzleEntry -> PuzzleEntry -> PuzzleEntry
            ((n, m), z) +++ ((n', m'), z')
              | n' + 1 < m' =
                  let p = fromIntegral m' / (fromIntegral (n' + 1) + fromIntegral m')
                      s = p + (1 - p) * z' + p * z
                   in ((n, m'), s)
              | n' + 1 > m' =
                  let p = fromIntegral (n' + 1) / (fromIntegral (n' + 1) + fromIntegral m')
                      s = p + p * z' + (1 - p) * z
                   in ((n, m'), s)
              | m' == n' + 1 =
                  let p = 1 / 2
                      s = p + p * z' + p * z
                   in ((n, m'), s)
         in [((n + 1, 0), fromIntegral (n + 1))] ++ zipWith (+++) zs (tail zs) ++ [((0, n + 1), fromIntegral (n + 1))]
   in forever nextFunction first

convert :: [[((Int, Int), Rational)]] -> [[((Int, Int), (Rational, Double))]]
convert = map (map (\((n, m), s) -> ((n, m), (s, fromRational s))))

printPuzzleEntryTriangleToEntryRowAt :: Int -> IO ()
printPuzzleEntryTriangleToEntryRowAt n = printTriangleToRowAt (n + 1) (convert puzzleEntryTriangle)

successMoreThan :: Double -> ((Int, Int), (Rational, Double)) -> Bool
successMoreThan d ((_, _), (_, s)) = s > d

solution :: Int -> Double -> IO ()
solution n d = (print . last . takeWhile (successMoreThan d) . (!! n) . convert) puzzleEntryTriangle