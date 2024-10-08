module IO where

--
-- general IO
--

printList :: (Show z) => [z] -> IO ()
printList = putStrLn . unwords . map show

