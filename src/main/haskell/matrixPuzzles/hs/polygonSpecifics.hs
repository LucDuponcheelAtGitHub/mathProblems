module PolygonSpecifics where

import MatrixPuzzles (MatrixPuzzleSolver, solver)
import Utilities (cp, reduce, noDupSinglesIn)

polygonSolver :: Integer -> MatrixPuzzleSolver Integer
polygonSolver polygonMaxNumber = solver noDuplicateSingles pruneNonSingles fail values
  where

    values = map Just [1 .. polygonMaxNumber]

    fail zsss =
      all
        ( \productOfChoicesOfFirstSide ->
            any
              (notElem productOfChoicesOfFirstSide)
              (tail productsOfChoicesOfSides)
        )
        (head productsOfChoicesOfSides)
      where
        productsOfChoicesOfSides =
          zipWith
            (\zss zs -> map product (cp (zss ++ [zs])))
            zsss
            (map head (tail zsss) ++ [head (head zsss)])

    pruneNonSingles = map reduce

    noDuplicateSingles = noDupSinglesIn . pol

    pol = concat
