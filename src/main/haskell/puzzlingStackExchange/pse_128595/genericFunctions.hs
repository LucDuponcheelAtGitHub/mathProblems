module GenericFunctions where

-- makes code self-contained
import Prelude hiding (until)

until p f x = if p x then x else until p f (f x)

fixedPointOf f = until (\x -> f x == x) f

