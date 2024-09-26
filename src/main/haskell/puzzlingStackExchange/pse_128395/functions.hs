module Functions where

import Types

--
-- general functions
--

-- most general
forever :: (z -> z) -> z -> Row z
forever z2z z = z : forever z2z (z2z z)

-- more specific
triangle :: (Row z -> Row z) -> (z -> z -> z) -> (Row z -> Row z) -> Row z -> Triangle z
triangle prefix operation postfix topRow =
  let row2row row = prefix row ++ zipWith operation row (tail row) ++ postfix row
   in forever row2row topRow

-- most specific
entryTriangle ::
  (Row ((n, m), z) -> Row ((n, m), z)) ->
  ((n, m) -> (z, z) -> z) ->
  (Row ((n, m), z) -> Row ((n, m), z)) ->
  Row ((n, m), z) ->
  Triangle ((n, m), z)
entryTriangle prefix operation postfix topRow =
  let ((n, m), z) `op` ((n', m'), z') = ((n, m'), (n', m') `operation` (z, z'))
   in triangle prefix op postfix topRow
