-- Assignment: Lab2
-- Exercise: 3 (Testing properties strength & Recognizing Permutations)
-- Student: Quinten Heijn
-- Time needed: 45 min
--------------------------------------------------------------------------

module Lab2 where
import Data.List
import Test.QuickCheck

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 x = even x
prop2 x = even x && x > 3
prop3 x = even x || x > 3
prop4 x = (even x && x > 3) || even x

propList :: [Int -> Bool]
propList = [prop4, prop3, prop1, prop2]

-- to group!
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys = if elem (head xs) ys
  then isPermutation (tail xs) (delete (head xs) ys)
  else False
