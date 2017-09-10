-- Assignment: Lab1
-- Exercise: 5
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime

-- findPrimeSum calls findPrimeSum', to find the first sequence of 101 primes
-- were the sum of this sequence is also a prime.
findPrimeSum :: Int
findPrimeSum = findPrimeSum' 0 101

-- findPrimeSum' takes the sum of a sequence of n primes recursively, until
-- the sum of this sequence is also a prime. It returns this prime.
findPrimeSum' :: Int -> Int -> Int
findPrimeSum' start n = let slice = sum (takeSlice primes start n) in
  if isPrime (slice)
  then slice
  else findPrimeSum' (start + 1) n

-- takeSlice returns a slice of a list.
takeSlice :: [Int] -> Int -> Int -> [Int]
takeSlice list firstElem lastElem = take lastElem (drop firstElem list)
