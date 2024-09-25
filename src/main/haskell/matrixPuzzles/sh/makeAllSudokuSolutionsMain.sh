#! /usr/bin/bash

cd ../hs

mv ../bin/*.hi .
mv ../bin/*.o .

ghc -O2 allSudokuSolutionsMain.hs allSudokuSolutionsFor.hs sudokuSpecifics.hs matrixPuzzles.hs utilities.hs

mv *.o ../bin
mv *.hi ../bin
mv allSudokuSolutionsMain ../bin

cd ../sh