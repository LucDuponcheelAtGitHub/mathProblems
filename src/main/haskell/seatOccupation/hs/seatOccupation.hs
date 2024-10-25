module SeatOccupation where

p :: (Fractional a2, Integral a1) => a1 -> a2
p 2 = 1/2
p n = (1 + sum [ p (n-(m-1)) | m <- [2 .. (n-1)] ]) / fromIntegral n
 
-- probability as Double
pd :: Int -> Double
pd = p

-- probability as Rational
pr :: Integer -> Rational
pr = p