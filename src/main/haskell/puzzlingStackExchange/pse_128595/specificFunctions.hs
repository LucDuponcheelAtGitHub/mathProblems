module SpecificFunctions where

import Data.List (elemIndex)

import Types ( Person (Man, Me, Woman) )

import GenericFunctions ( fixedPointOf )

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
      (numberOfMenBefore, numberOfWomenAfter) = (length [p | p <- before, isMan p], length [p | p <- after, isWoman p])
      moveBackward = before ++ [head after] ++ [Me] ++ tail after
      moveForward = init before ++ [Me] ++ [last after] ++ after
      doNotMove = ps
   in --  |numberOfMenBefore - numberOfWomenAfter| decreases
      if numberOfMenBefore < numberOfWomenAfter -- more women after me
        then moveBackward -- eiher one man more before me or one woman less after me
        else
          if numberOfMenBefore > numberOfWomenAfter -- more men before me
            then moveForward -- eiher one woman more after me or one man less before me
            else doNotMove

movedToCorrectPositionIn = fixedPointOf move

correctPositionIn = elemIndex Me . movedToCorrectPositionIn

numberOfWomenIn = Just . length . filter isWoman