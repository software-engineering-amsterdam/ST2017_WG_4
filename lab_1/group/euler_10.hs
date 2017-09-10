-- Assignment: Lab1
-- Exercise: Euler 10
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
--------------------------------------------------------------------------

module Euler_10 where
import Data.Numbers.Primes

-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

primesUntil :: Integer -> [Integer]
primesUntil lim = takeWhile (<lim) primes

solution :: Integer -> Integer
solution n = sum $ primesUntil n
