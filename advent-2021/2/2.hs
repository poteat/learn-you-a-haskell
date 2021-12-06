#!/usr/bin/env runhaskell

import qualified GHC.Types

-- | Given two ordinals, determine whether or not the second is
-- | greater than the first.
doesIncrease :: Ord a => a -> a -> Bool
doesIncrease a b = b > a

-- | Given a positive integer, and a list, construct a list of
-- | sliding windows of size n.
-- slide :: Int -> [a] -> [[a]]
-- slide 1 xs = map (: []) xs
-- slide 2 xs = zip xs (tail xs) ^.. each
-- slide n xs = map (take n) (slide (n - 1) xs) ++ slide n (tail xs)

-- | Convert a list into a list on its consecutive pairs.
--
-- >>> pairs [1, 2, 3, 4]
-- [(1,2),(2,3),(3,4)]
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs x = zip x (tail x)

-- | Given a list of numbers, return a list of whether or not
-- | the current element is greater than the previous element.
--
-- >>> isBigger [1,2,3,4]
-- [True,True,True]
isBigger :: Ord a => [a] -> [Bool]
isBigger [] = []
isBigger x = map (uncurry doesIncrease) (pairs x)

-- | Given a list of booleans, count the number of True values.
--
-- >>> countTrue [True, False, True, True]
-- 3
countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue x = length (filter (== True) x)

-- | Given a list of numbers, count how many are larger than their
-- | predecessor.
--
-- >>> countIncrease [1, 2, 3, 4]
-- 3
countIncrease :: [Int] -> Int
countIncrease [] = 0
countIncrease x = countTrue (isBigger x)

sum3 xs = map (\x -> x) (zip3 xs (tail xs) (tail (tail xs)))

main = do
  contents <- readFile "input.txt"
  let numbers = map read (lines contents) :: [Int]
  print (countIncrease numbers)
