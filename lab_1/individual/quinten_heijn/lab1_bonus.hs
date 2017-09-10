-- Assignment: Lab1
-- Exercise: Bonus
-- Student: Quinten Heijn
-- Time needed: 30 minutes
-- Not finished!
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime

-- Euler 49

checkTriplePrime :: [Int] -> Bool
checkTriplePrime [a, b, c] = isPrime a && isPrime b && isPrime c
checkTriplePrime n = False

checkSequence :: [Int] -> Bool
checkSequence [a, b, c] = b - a == c - b
checkSequence n = False

-- generatePermutations :: [[[Int]]]
-- generatePermutations = map (map createNum (combinationsWRep 3) (combinationsWRep [1, 2, 3, 5, 6, 7, 8, 9, 0] 4)

-- findPrimes :: [[Int]]
-- findPrimes = filter checkTriplePrime (filter checkSequence generatePermutations)

createNum :: Int -> [Int] -> Int
createNum n [] = n
createNum n (x:xs) = createNum ((n * 10) + x) xs


combinationsWRep :: Int -> [Int] -> [[Int]]
combinationsWRep n xs = filter ((n==).length) $ mapM (const xs) [1..n]
-- https://stackoverflow.com/questions/21775378/get-all-possible-combinations-of-k-elements-from-a-list
