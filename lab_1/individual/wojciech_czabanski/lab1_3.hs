module Lab1_3 where
import Data.List
import Test.QuickCheck

factorial :: Int -> Int
factorial n = product [1..n]

prop :: Int -> Bool
prop n = n < 0 || (length(permutations [1..n]) == factorial n)

-- It takes a lot of time, because the number of subsequences
-- to generate grows factorially O(n!) so QuickCheck
-- hardly ever can finish the full 100 test case run

-- The test is checking whether the subsequences function is
-- satisfying a part of its specification.

-- Time: 15 minutes
