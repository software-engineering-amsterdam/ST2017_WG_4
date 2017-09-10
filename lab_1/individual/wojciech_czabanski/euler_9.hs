module Euler_9 where

-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

isTriplet :: Int -> Int -> Int -> Bool
isTriplet a b c = (a^2 + b^2) == c^2

tupleProduct :: (Int, Int, Int) -> Int
tupleProduct (a, b, c) = a * b * c

findTriplet :: Int -> Int -> Int -> Int -> (Int, Int, Int)
findTriplet a b c lim | isTriplet a b c && a + b + c == 1000 = (a, b, c)
                      | a < b = findTriplet (a+1) b c lim
                      | b < c = findTriplet 1 (b+1) c lim
                      | c < lim = findTriplet 1 1 (c+1) lim
                      | otherwise = (-1, -1, -1)

solution :: Int -> Int
solution n = tupleProduct (findTriplet 1 1 1 n)

-- Time: 50 minutes
