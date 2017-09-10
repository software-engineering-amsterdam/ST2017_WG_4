-- Assignment: Lab1
-- Exercise: 2
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
-- Description: It was difficult testing this rule, because of it's exponential complexity
--              i.e. When the length of the list increases the time to calculate
--              the subsequences becomes very high.
--              We could say we're testing a mathematical fact, because we can
--              asume that the functions we're using a working properly.
--              However, it might be unrealistic to even try testing a
--              mathematical fact with haskell, because there to many
--              dependencies that are not formaly proven to work.
--------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

-- The function that is tested in this exercise.
myTest :: [Int] -> Bool
myTest l = length (subsequences l) == 2 ^ (length l)
