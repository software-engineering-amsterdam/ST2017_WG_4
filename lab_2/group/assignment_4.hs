-- Assignment: Lab2
-- Exercise: 4
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------

module Lab2_4 where
  
import Data.List
import Test.QuickCheck

-- isPermutation checks if one list is a permutation of the other list.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] ys = False
isPermutation xs ys = if elem (head xs) ys
  then isPermutation (tail xs) (delete (head xs) ys)
  else False


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- testPermutation generates all permutations and filters them with isPermutation.
-- It return True if isPermutation evaluates all permutations as True. Else it
-- will return false.
testPermutation :: Eq a => [a] -> Bool
testPermutation lst = filter (not . isPermutation lst) (permutations lst) == []

-- testNotPermutationAll generates all combinations of a list and removes all
-- permutations from this new list.
-- It returns True when isPermutation evalutates all the elements of this new
-- list as false. Else it will return False.
testNotPermutationAll :: Eq a => [a] -> Bool
testNotPermutationAll lst = filter (isPermutation lst) combinations == []
   where combinations = (concat $ map permutations $ subsequences lst) \\ (permutations lst)

-- testNotPermutationSingle removes all permutations of lst1 from lst2.
-- It returns True when isPermutation evalutates all the elements of this new
-- list as false. Else it will return False.
testNotPermutationSingle :: Eq a => [a] -> [[a]] -> Bool
testNotPermutationSingle lst1 lst2 = filter (isPermutation lst1) (lst2 \\ (permutations lst1)) == []
