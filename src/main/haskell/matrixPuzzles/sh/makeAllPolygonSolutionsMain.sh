#! /usr/bin/bash

cd ../hs

mv ../bin/*.hi .
mv ../bin/*.o .

ghc -O2 allPolygonSolutionsMain.hs allPolygonSolutionsFor.hs polygonSpecifics.hs matrixPuzzles.hs utilities.hs

mv *.o ../bin
mv *.hi ../bin
mv allPolygonSolutionsMain ../bin

cd ../sh
