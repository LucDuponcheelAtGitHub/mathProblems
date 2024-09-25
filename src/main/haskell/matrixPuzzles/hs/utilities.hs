module Utilities where

import Data.List (nub, (\\))

-- module Instances where

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

-- import Instances

cp :: [[z]] -> [[z]]
cp [] = [[]]
cp (zs : zss) = [z' : zs' | z' <- zs, zs' <- cp zss]

chop :: Int -> [z] -> [[z]]
chop n [] = []
chop n zs = take n zs : chop n (drop n zs)

reduce :: (Eq z) => [[z]] -> [[z]]
reduce zss = [zs `minus` singles | zs <- zss]
  where
    singles = concat (filter single zss)

single :: [z] -> Bool
single [_] = True
single _ = False

minus :: (Eq z) => [z] -> [z] -> [z]
zs `minus` ys = if single zs then zs else zs \\ ys

noDupSinglesIn :: (Eq z) => [[z]] -> Bool
noDupSinglesIn = nodups . concat . filter single
  where
    nodups [] = True
    nodups (z : zs) = not (z `elem` zs) && nodups zs

allReplaceJustsByNothings :: Eq z => [[Maybe z]] -> [[[Maybe z]]]
allReplaceJustsByNothings zsss = (map (chop (length (head zsss))) . allReplaceJustsByNothings' . concat) zsss
  where
    allReplaceJustsByNothings' zsss =
      replaceJustsByNothingsAtMost (length zsss) zsss
    replaceJustsByNothingsAtMost 1 [] =
      [[]]
    replaceJustsByNothingsAtMost 1 (Nothing : zsss) =
      map (Nothing :) (replaceJustsByNothingsAtMost 1 zsss)
    replaceJustsByNothingsAtMost 1 (Just z : zsss) =
      (Nothing : zsss) : map (Just z :) (replaceJustsByNothingsAtMost 1 zsss)
    replaceJustsByNothingsAtMost n zsss =
      nub (concatMap (replaceJustsByNothingsAtMost 1) (replaceJustsByNothingsAtMost (n - 1) zsss))
