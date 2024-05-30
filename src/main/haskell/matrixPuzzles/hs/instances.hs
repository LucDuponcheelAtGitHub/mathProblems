module Instances where

instance Num (Maybe Integer) where
  negate (Just x) = Just (-x)
  negate _ = Nothing
  (+) (Just x) (Just y) = Just (x + y)
  (+) _ _ = Nothing
  (*) (Just x) (Just y) = Just (x * y)
  (*) _ _ = Nothing
  abs (Just x) = Just (abs x)
  abs _ = Nothing
  signum (Just x)
    | x >= 0 = Just 1
    | otherwise = Just (-1)
  signum _ = Nothing
  fromInteger = Just