import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Recognizing Permutations

-- Properties van be ordinary haskell boolean functions
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = if x `elem` (permutations y) then True else False

{--
Next, define some testable properties for this function, and use a number of
well-chosen lists to test isPermutation. You may assume that your input lists
do not contain duplicates. What does this mean for your testing procedure?

define testable properties:

All Permutations
1. pick a random List
2. get al permutations
3. list from 1 must be all Permutations from 2

All no Permutations
1. Same list as previous 1
2. generate all non permutations of the list with the same length
3. All must be isDerangement False

--}
