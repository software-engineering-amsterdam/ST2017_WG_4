-- Assignment: Lab1
-- Exercise: 2
-- Student: Quinten Heijn
-- Time needed: 5 min
-- General comment: It was difficult testing this rule, because of it's exponential complexity i.e. When the length of the list increases the time to calculate the subsequences becomes very high.
--                 We're testing a mathematical fact, because ??
--------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

myTest :: [Int] -> Bool
myTest l = length (subsequences l) == 2 ^ (length l)
