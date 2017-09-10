-- Assignment: Lab1
-- Exercise: 3
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
-- Description: It was difficult testing this rule, because of it's factorial complexity
--              i.e. When the length of the list increases the time to calculate
--              the subsequences becomes very high.
--              We can't say we're testing a mathematical fact, because we're
--              using a 'myFactorial', that hasn't been properly tested. So
--              we're propably testing our implementation of the fact.
--              However, it might be unrealistic to even try testing a
--              mathematical fact with haskell, because there to many
--              dependencies that are not formaly proven to work.
--------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

-- The function that is tested in this exercise.
myTest :: [Int] -> Bool
myTest l = length (permutations l) == myFactorial (length l)

-- Implementation of factorial.
myFactorial :: Int -> Int
myFactorial n = product [1 .. n]
