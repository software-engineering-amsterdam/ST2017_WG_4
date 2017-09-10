module Lab1_4 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes

reversal :: Int -> Int
reversal = read . reverse . show

primesUntil :: Int -> [Int]
primesUntil lim = takeWhile (<lim) primes

isReversePrime :: Int -> Bool
isReversePrime n = isPrime n && isPrime (reversal n)

reversePrimesUntil :: Int -> [Int]
reversePrimesUntil n = filter isReversePrime (primesUntil n)

-- Test approaches:
-- 1) check if all the returned values belong to the list of primes smaller than n
-- 2) check if the reverse of all the returned values are also prime 

-- Time: 30 minutes
