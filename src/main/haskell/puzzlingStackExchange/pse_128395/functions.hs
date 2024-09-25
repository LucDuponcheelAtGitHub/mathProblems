module Functions where

import Types

--
-- general functions
--

forever :: (z -> z) -> z -> Row z
forever z2z z = z : forever z2z (z2z z)

triangle :: (Row z -> Row z) -> (z -> z -> z) -> (Row z -> Row z) -> Row z -> Triangle z
triangle prefix operation postfix row =
  let row2row row = prefix row ++ zipWith operation row (tail row) ++ postfix row
   in forever row2row row
