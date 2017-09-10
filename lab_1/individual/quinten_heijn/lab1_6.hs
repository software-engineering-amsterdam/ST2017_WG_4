-- Assignment: Lab1
-- Exercise: 6
-- Student: Quinten Heijn
-- Time needed: 10 minutes
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes -- Libary used for primes and isPrime


findCounter :: Int
findCounter = findCounter' 0

findCounter' :: Int -> Int
findCounter' n = let conjecCase = (product (take n primes)) + 1 in
  if isPrime (conjecCase)
  then n + 1
  else findCounter' (n + 1)
