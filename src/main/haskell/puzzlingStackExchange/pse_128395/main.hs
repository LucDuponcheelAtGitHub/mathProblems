import Fibonacci
import Functions
import IO
import Pascal
import Puzzle
import Types

main :: IO ()
main =
  do
    putStrLn "==========================================================================================================================="

    putStrLn ""
    putStrLn "pascalTriangleToRowAt 5"
    putStrLn ""
    printPascalTriangleToRowAt 5
    putStrLn ""

    putStrLn "==========================================================================================================================="

    putStrLn ""
    putStrLn "pascalEntryTriangleToEntryRowAt 5"
    putStrLn ""
    printPascalEntryTriangleToEntryRowAt 5
    putStrLn ""

    putStrLn "==========================================================================================================================="

    putStrLn ""
    putStrLn "puzzleEntryRowAt 100"
    putStrLn ""
    printPuzzleEntryRowAt 100
    putStrLn ""

    putStrLn "==========================================================================================================================="

    putStrLn ""
    putStrLn "puzzleEntryTriangleToEntryRowAt 5"
    putStrLn ""
    printPuzzleEntryTriangleToEntryRowAt 5
    putStrLn ""

    putStrLn "==========================================================================================================================="

    putStrLn ""
    putStrLn "printSolution (100, 55.875)"
    putStrLn ""
    printSolution (100, 55.875)
    putStrLn ""

    putStrLn "==========================================================================================================================="

    putStrLn ""
    putStrLn "fibonacciRowToValueAt 10"
    putStrLn ""
    printFibonacciRowToValueAt 10
    putStrLn ""

    putStrLn "==========================================================================================================================="
