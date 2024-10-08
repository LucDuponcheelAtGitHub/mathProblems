import Types ( Person (Man, Me, Woman) )

import SpecificFunctions ( isWoman, movedToCorrectIndexIn, correctIndexIn, numberOfWomenIn)

import IO (printList)

ps = [Man, Woman, Me, Woman, Woman, Man, Man]

main :: IO ()
main = 
    do
        putStrLn "================================================================="

        putStrLn "persons"
        
        printList ps

        putStrLn "================================================================="

        putStrLn "moved to correct index in persons"
        
        printList (movedToCorrectIndexIn ps)

        putStrLn "================================================================="

        putStrLn "test if correct index is number of women (indexs start from 0)"
        
        print (correctIndexIn ps == numberOfWomenIn ps)

        putStrLn "================================================================="