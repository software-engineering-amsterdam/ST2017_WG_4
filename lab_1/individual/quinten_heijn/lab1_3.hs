-- Assignment: Lab1
-- Exercise: 3
-- Student: Quinten Heijn
-- Time needed: 5 min
-- General comment: It was difficult testing this rule, because of it's factorial complexity i.e. When the length of the list increases the time to calculate the subsequences becomes very high.
--                 We're testing a mathematical fact, because ??
--------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

myTest :: [Int] -> Bool
myTest l = length (permutations l) == myFactorial (length l)

myFactorial :: Int -> Int
myFactorial n = product [1 .. n]
