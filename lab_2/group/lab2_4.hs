-- Assignment: Lab2
-- Exercise: 4 (Recognizing and generating derangements)
-- Student: Quinten Heijn
-- Time needed: 30 min
--------------------------------------------------------------------------

module Lab2 where
import Data.List
import Test.QuickCheck

isDerangement, isDerangement' :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = (isPermutation xs ys) && (isDerangement' xs ys)
isDerangement' [] [] = True
isDerangement' xs ys = (head xs /= head ys) && isDerangement' (tail xs) (tail ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys = if elem (head xs) ys
  then isPermutation (tail xs) (delete (head xs) ys)
  else False

deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement' xs) (permutations xs)
