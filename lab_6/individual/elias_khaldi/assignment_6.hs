-- Assignment: Lab6
-- Exercise: 6
-- Student: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 90 minutes
--------------------------------------------------------------------------

module Assignment_6 where
 
import Data.List
import System.Random
import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
 k <- [2..], 
 prime (6*k+1), 
 prime (12*k+1), 
 prime (18*k+1) ]

testMR :: [Integer] -> IO [(Integer,Bool)]
testMR [] = return []
testMR (h:t) = do
 a <- primeMR 3 h
 b <- testMR t
 return ((h,a):b)
 
testMR_CM :: Int -> IO [(Integer,Bool)]
testMR_CM n = testMR (take n (carmichael))

-- Using the primality test described above we find that unlike the Fermat's primality check, the MR check is able to identify the elements in the carmichael set as non-prime
-- NOTE: The bigger you take n, the longer it takes.
-- Example of output:
-- 
-- *Assignment_6> testMR_CM 1
-- [(294409,False)]

mersennePrimes :: [Integer] -> IO [Integer]
mersennePrimes [] = return []
mersennePrimes (h:t) = do
 x <- return (2 ^ h - 1)
 a <- primeMR 3 x
 b <- mersennePrimes t
 if a 
  then return (x:b)
  else return b
  
findMersennePrimes :: Int -> IO[Integer]
findMersennePrimes n = mersennePrimes (take n primes)

-- Using the method described above you can find Mersenne primes using the first n primes. 
-- NOTE: For n>9 this takes a very long time.
-- Example of output:
-- 
-- *Assignment_6> findMersennePrimes 9
-- [3,7,31,127,8191,131071,524287]
--
-- A Mersenne prime is a prime that is 1 less than a power of two (2^n - 1).
-- This holds for all the primes found in the example:
-- 3 = 2 ^ 2 - 1
-- 7 = 2 ^ 3 - 1
-- 31 = 2 ^ 5 - 1
-- 127 = 2 ^ 7 - 1
-- 8191 = 2 ^ 13 - 1
-- 131071 = 2 ^ 17 - 1
-- 524287 = 2 ^ 19 - 1


