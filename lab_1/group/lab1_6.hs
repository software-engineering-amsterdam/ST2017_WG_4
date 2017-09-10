-- Assignment: Lab1
-- Exercise: 6
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime

-- findCounter calls findCounter', starting from 0.
findCounter :: Int
findCounter = findCounter' 0

-- findCounter' looks for the first counter example of the claim
-- that  list of consecutive primes + 1 is also a prime.
findCounter' :: Int -> Int
findCounter' n = let conjecCase = (product (take n primes)) + 1 in
  if not $ isPrime (conjecCase)
  then n
  else findCounter' (n + 1)
