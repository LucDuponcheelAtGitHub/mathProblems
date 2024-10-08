import Types ( Person (Man, Me, Woman) )

import SpecificFunctions ( isWoman, movedToCorrectPositionIn, correctPositionIn, numberOfWomenIn)

import IO (printList)

ps = [Man, Woman, Me, Woman, Woman, Man, Man]

main :: IO ()
main = 
    do
        putStrLn "================================================================="

        putStrLn "persons"
        
        printList ps

        putStrLn "================================================================="

        putStrLn "moved to correct position in persons"
        
        printList (movedToCorrectPositionIn ps)

        putStrLn "================================================================="

        putStrLn "test if correct position is number of women (positions start from 0)"
        
        print (correctPositionIn ps == numberOfWomenIn ps)

        putStrLn "================================================================="