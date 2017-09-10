module Lab1_2 where
import Data.List
import Test.QuickCheck

prop :: Int -> Bool
prop n = n < 0 || length(subsequences [1..n]) == 2 ^ n

-- It takes a lot of time, because the number of subsequences
-- to generate grows exponentially O(2^n) so QuickCheck
-- hardly ever can finish the full 100 test case run

-- The test is checking whether the subsequences function is
-- satisfying a part of its specification.

-- Time: 15 minutes
