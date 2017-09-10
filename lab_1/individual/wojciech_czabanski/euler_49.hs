module Euler_49 where
import Data.Numbers.Primes
import Data.Digits

-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
--  What 12-digit number do you form by concatenating the three terms in this sequence?

-- steps
-- domain: 3 prime number arithmetic sequences smaller than 10000
-- TODO: how to form arithmetic sequences of primes?
-- check each trio for properties:
-- property 1: all items are primes
-- property 2: permutations of each element contains the other
-- output a1 . a2 . a3

domain :: Int -> [[Int]]
domain = []

arePermutations :: [Int] -> Bool
arePermutations ns = True
-- break down into digits
-- generate permutations
-- intersect permutation lists -> should result in 3 elements

solution :: Int -> Int
solution n = 0

-- using library Primes
-- using library Digits

-- Time: 20 minutes
