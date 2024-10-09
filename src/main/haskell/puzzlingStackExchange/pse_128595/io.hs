module IO where

printList :: (Show z) => [z] -> IO ()
printList = putStrLn . unwords . map show

