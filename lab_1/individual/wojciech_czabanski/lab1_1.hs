module Lab1_1 where
import Data.List
import Test.QuickCheck

-- Part 1
right_sum_1 :: Int -> Int
right_sum_1 n = n * ((n + 1) * ((2 * n) + 1)) `div` 6

left_sum_1 :: Int -> Int
left_sum_1 n = sum(map (^2) ([1..n]))

prop_1 :: Int -> Bool
prop_1 n = left_sum_1 abs n == right_sum_1 abs n

-- Part 2
left_sum_2 :: Int -> Int
left_sum_2 n = sum(map (^3) ([1..n]))

right_sum_2 :: Int -> Int
right_sum_2 n = (n * (n + 1) `div` 2) ^ 2

prop_2 :: Int -> Bool
prop_2 n = left_sum_2 abs  n == right_sum_2 abs n

-- Time: 2.5 hours
