module Functions where

--
-- general function
--

forever :: (z -> z) -> z -> [z]
forever z2z z =
  let z2zs = forever z2z
      z' = z2z z
      zs = z2zs z'
   in z : zs