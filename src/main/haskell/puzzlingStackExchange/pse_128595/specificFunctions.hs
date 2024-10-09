module SpecificFunctions where

import Data.List (elemIndex)
import GenericFunctions (fixedPointOf)
import Types (Person (Man, Me, Woman))

isMan p = p == Man

isWoman p = p == Woman

beforeAndAfter (p : ps) =
  if p == Me
    then
      ([], ps)
    else
      let (bps, aps) = beforeAndAfter ps
          before = p : bps
          after = aps
       in (before, after)

move ps =
  let (before, after) = beforeAndAfter ps
      (numberOfMenBefore, numberOfWomenAfter) =
        (length [p | p <- before, isMan p], length [p | p <- after, isWoman p])
      moveBackward = before ++ [head after] ++ [Me] ++ tail after
      moveForward = init before ++ [Me] ++ [last after] ++ after
      doNotMove = ps
   in if numberOfMenBefore < numberOfWomenAfter -- more women after
        then moveBackward -- eiher one man more before or one woman less after
        else
          if numberOfMenBefore > numberOfWomenAfter -- more men before
            then moveForward -- eiher one woman more after or one man less before
            else doNotMove

-- this cannot go on forever because

-- | numberOfMenBefore - numberOfWomenAfter| keeps on decreasing
movedToCorrectIndexIn = fixedPointOf move

correctIndexIn = elemIndex Me . movedToCorrectIndexIn

numberOfWomenIn = Just . length . filter isWoman