#! /usr/bin/bash

cd ../hs

mv ../bin/*.hi .
mv ../bin/*.o .

ghc -O2 sudokuSolutionsOfSudokuPuzzleMain.hs sudokuSpecifics.hs matrixPuzzles.hs utilities.hs instances.hs

mv *.o ../bin
mv *.hi ../bin
mv sudokuSolutionsOfSudokuPuzzleMain ../bin

cd ../sh
