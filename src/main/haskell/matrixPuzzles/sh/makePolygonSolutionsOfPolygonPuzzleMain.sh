#! /usr/bin/bash

cd ../hs

mv ../bin/*.hi .
mv ../bin/*.o .

ghc -O2 polygonSolutionsOfPolygonPuzzleMain.hs polygonSpecifics.hs matrixPuzzles.hs utilities.hs

mv *.o ../bin
mv *.hi ../bin
mv polygonSolutionsOfPolygonPuzzleMain ../bin

cd ../sh