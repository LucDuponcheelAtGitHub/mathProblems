module Functions where

import Types

--
-- general functions
--

-- most general
forever :: (z -> z) -> z -> Row z
forever z2z z = z : forever z2z (z2z z)

-- more specific
triangle :: (z -> z -> z) -> (Row z -> Row z) -> (Row z -> Row z) -> Row z -> Triangle z
triangle operation prefix postfix topRow =
  let row2row row = prefix row ++ zipWith operation row (tail row) ++ postfix row
   in forever row2row topRow

-- most specific
entryTriangle ::
  ((n, m) -> (z, z) -> z) ->
  (EntryRow (n, m) z -> EntryRow (n, m) z) ->
  (EntryRow (n, m) z -> EntryRow (n, m) z) ->
  EntryRow (n, m) z ->
  EntryTriangle (n, m) z
entryTriangle operation =
  let ((n, m), z) `operation'` ((n', m'), z') = ((n, m'), (n', m') `operation` (z, z'))
   in triangle operation'
