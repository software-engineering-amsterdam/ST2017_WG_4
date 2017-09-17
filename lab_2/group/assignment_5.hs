-- Assignment: Lab2
-- Exercise: 5
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------

module Lab2_5 where
  
import Data.List
import Test.QuickCheck

-- isDerangement checks if a permutation is a derangement from a other permutation.
-- It does this by first checking it the permutation is a permutation to begin with.
-- Next it will compare every element from xs with the element of the same index
-- from ys.
isDerangement, isDerangement' :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = (isPermutation xs ys) && (isDerangement' xs ys)
isDerangement' [] [] = True
isDerangement' xs ys = (head xs /= head ys) && isDerangement' (tail xs) (tail ys)

-- isPermutation checks if one list is a permutation of the other list.
-- We tested this function in the previous exercise, and we now assume it works
-- perfectly.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys = if elem (head xs) ys
  then isPermutation (tail xs) (delete (head xs) ys)
  else False

-- deran filters all non-derangements from a list of permutations of xs.
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement' xs) (permutations xs)
