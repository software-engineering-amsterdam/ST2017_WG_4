module Lab2_5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Recognizing and generating derangements

isDerangement :: [Int] -> [Int] -> Bool
isDerangement ns1 ns2 = False

deran :: [Int] -> [[Int]]
deran ns = filter permutations (tail (reverse ns))

-- TODO: how to check for deragements (implement isDerangement)
-- finish generating derangements


-- Time spent: 5 minutes
