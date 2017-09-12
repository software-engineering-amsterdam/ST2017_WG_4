module Lab2_5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Recognizing and generating derangements

isDerangement :: [Int] -> [Int] -> Bool
isDerangement [] [] = True
isDerangement a [] = False
isDerangement [] b = False
isDerangement (a:as) (b:bs) = a /= b && (isDerangement as bs)

deran :: [Int] -> [[Int]]
deran ns = filter (\x -> (isDerangement ns x)) (permutations ns)

-- Time spent: 10 minutes
