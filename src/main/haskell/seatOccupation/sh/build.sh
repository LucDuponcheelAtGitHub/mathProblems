#! /usr/bin/bash

cd ../hs

ghc -O2 seatOccupation.hs seatOccupationMain.hs

mv *.o ../bin
mv *.hi ../bin
mv seatOccupationMain ../bin

cd ../sh