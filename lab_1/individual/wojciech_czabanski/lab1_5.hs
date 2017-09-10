module Lab1_5 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes

slice :: Int -> Int -> [Int] -> [Int]
slice from to xs = take (to - from + 1) (drop from xs)

-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python

isPrimeWindowValid :: [Int] -> Bool
isPrimeWindowValid ns = isPrime (sum ns)

findPrimeList :: Int -> Int -> [Int]
findPrimeList start end | isPrimeWindowValid (slice start end primes) = slice start end primes
                       | otherwise = findPrimeList (start+1) (end+1)

prop :: Int
prop = sum (findPrimeList 1 101)

-- Since I am iterating over primes subsequences and the function terminates
-- whenever it find the first sequence that is a prime I can testimonies
-- either the condition that I am using to check whether a particular
-- subsequence is satisfactory (isPrimeWindowValid) or the function
-- that I use to generate the prime subsequences itself (slice)
-- The function that I use to compute primes is a library function
-- so it is *assumed* to be working according to its specification.

-- using library Primes

-- Time: 20 minutes
