-- Assignment: Lab1
-- Exercise: 4
-- Student: Quinten Heijn
-- Time needed: 25 min
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime

-- I wrote this function for testing reversal, but is has problems with negative numbers and numbers ending with a zero.
--myTestReversal :: Integer -> Bool
--myTestReversal n = n < 0 || n == reversal (reversal n)

reversal :: Int -> Integer
reversal = read . reverse . show

revPrimes :: Int -> [Int]
revPrimes n = filter checkRev (take n primes)

checkRev :: Int -> Bool
checkRev n = isPrime (reversal n)

-- This function should actually only look at prime lower than 10000
-- takeWhile
