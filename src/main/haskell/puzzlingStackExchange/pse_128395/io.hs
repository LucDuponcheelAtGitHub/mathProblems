module IO where

import Types

--
-- general IO
--

printRow :: (Show z) => Row z -> IO ()
printRow = putStrLn . unwords . map show

printTriangle :: (Show z) => Triangle z -> IO ()
printTriangle = mapM_ printRow
