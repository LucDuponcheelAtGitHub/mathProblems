module PolygonSpecifics where

import MatrixPuzzles (MatrixPuzzleSolver, solver)
import Utilities (cp, reduce, consistent)

polygonSolver :: Integer -> MatrixPuzzleSolver Integer
polygonSolver polygonMaxNumber = solver safe prune fail values
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

    prune = map reduce

    safe = consistent . pol

    pol = concat
