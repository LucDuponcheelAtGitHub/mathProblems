module SeatOccupation where

p n =
  sum
    [ (1 / fromIntegral (n - 1)) * (1 / fromIntegral (n - m + 1))
      | m <- [2 .. (n - 1)]
    ]

-- probability as Double
pd :: Int -> Double
pd = p

-- probability as Rational
pr :: Integer -> Rational
pr = p
