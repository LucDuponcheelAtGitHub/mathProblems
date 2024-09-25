module IO where

import Types

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
