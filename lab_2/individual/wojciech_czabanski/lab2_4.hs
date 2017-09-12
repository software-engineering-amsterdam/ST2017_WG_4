module Lab2_4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Recognizing permutations

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation a [] = False
isPermutation [] b = False
isPermutation (a:as) b = (elem a b) && (isPermutation as (delete a b))

-- Time spent: 20 minutes
