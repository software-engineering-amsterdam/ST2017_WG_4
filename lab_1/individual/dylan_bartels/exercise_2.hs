import Data.List
import Test.QuickCheck

-- Assignment 2. time: 10 minutes

exercise4 :: Int -> Int
exercise4 n = length (subsequences [1..n])

solve :: Positive Int -> Bool
solve (Positive n) = (exercise4 n == 2 ^ n)

-- Is the property hard to test? If you find that it is, can you given a reason why?

-- The property is hard to test due to the length of the sequences expanding by
-- the power of 2 every increment. This is very intensive to calculate a batch
-- of the default 100 tests that QuickCheck executes


-- Give your thoughts on the following issue: when you perform the test for
-- exercise 4, what are you testing actually?

-- What is being tested is if the length of subsequences of a list is equal to
-- 2^n, whereby n is the biggest number in the list. QuickCheck executes this
-- test with 100 random numbers.

-- Are you checking a mathematical fact?
-- Or are you testing whether subsequences satisfies a part of its specification?
-- Or are you testing something else still?

-- What is being checked is a mathematical fact.
