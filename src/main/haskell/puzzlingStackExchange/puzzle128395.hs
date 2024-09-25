--
-- general types
--

type Function z y = z -> y

type Value z = z

type Row z = [Value z]

type Triangle z = Row (Row z)

type Pair z y = (z, y)

type Entry k v = Pair k v

type EntryRow k v = Row (Entry k v)

type EntryTriangle k v = Triangle (Entry k v)

--
-- general function
--

forever :: (z -> z) -> z -> [z]
forever z2z z =
  let z2zs = forever z2z
      z' = z2z z
      zs = z2zs z'
   in z : zs

--
-- general IO
--

printRow :: (Show z) => Row z -> IO ()
printRow = putStrLn . unwords . map show

printRowToValueAt :: (Show z) => Int -> Row z -> IO ()
printRowToValueAt n = printRow . take n

printTriangle :: (Show z) => Triangle z -> IO ()
printTriangle = mapM_ printRow

printTriangleToRowAt :: (Show z) => Int -> Triangle z -> IO ()
printTriangleToRowAt n = printTriangle . take n

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

main :: IO ()
main = solution 100 60.0
-- main = printPuzzleEntryTriangleToEntryRowAt 5
-- main = printPascalEntryTriangleToEntryRowAt 5


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

fibonacciFunction :: Function FibonacciArgument FibonacciResult
fibonacciFunction n = fibonacciRow !! n

printFibonacciRowToValueAt :: Int -> IO ()
printFibonacciRowToValueAt n = printRowToValueAt (n + 1) fibonacciRow