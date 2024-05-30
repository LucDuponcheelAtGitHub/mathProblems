#! /usr/bin/bash

cd ../hs

mv ../bin/*.hi .
mv ../bin/*.o .

ghc -O2 allPolygonPuzzlesMain.hs allPolygonSolutionsFor.hs allPolygonPuzzlesFor.hs polygonSpecifics.hs matrixPuzzles.hs utilities.hs instances.hs

mv *.o ../bin
mv *.hi ../bin
mv allPolygonPuzzlesMain ../bin

cd ../sh
