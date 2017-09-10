-- Assignment: Lab1
-- Exercise: 5
-- Student: Quinten Heijn
-- Time needed: 30 minutes
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime

findPrimeSum :: Int
findPrimeSum = findPrimeSum' 0 101

findPrimeSum' :: Int -> Int -> Int
findPrimeSum' start end = let slice = sum (mySlice primes start end) in
  if isPrime (slice)
  then slice
  else findPrimeSum' (start + 1) end

-- This function takes a slice from a list.
slice :: [Int] -> Int -> Int -> [Int]
slice list firstElem lastElem = take lastElem (drop firstElem list)
