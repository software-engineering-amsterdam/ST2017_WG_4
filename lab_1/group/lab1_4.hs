-- Assignment: Lab1
-- Exercise: 4
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
--------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime

-- reversal reverses the digits of an integer.
reversal :: Int -> Int
reversal = read . reverse . show

-- primesUntil produces a list of all primes smaller than lim.
primesUntil :: Int -> [Int]
primesUntil lim = takeWhile (<lim) primes

-- isReversePrime checks if n and the reverse of n are both primes.
isReversePrime :: Int -> Bool
isReversePrime n = isPrime n && isPrime (reversal n)

-- reversePrimesUntil shows all primes smaller than n that are still a primes
-- when reversed.
reversePrimesUntil :: Int -> [Int]
reversePrimesUntil n = filter isReversePrime (primesUntil n)

-- Test approaches:
-- 1) check if all the returned values belong to the list of primes smaller than n.
-- 2) check if the reverse of all the returned values are also prime.
