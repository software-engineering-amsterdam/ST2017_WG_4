{--
Time spent:
Quinten:      h
--}
module Assignment_1 where

import Data.List
import System.Random
import Lecture6 hiding (exM)

exM :: Integer -> Integer -> Integer -> Integer
exM x expo modu = mod (product [mod (x^y) modu | y <- expList $ toBinary expo]) modu

expList :: [Integer] -> [Integer]
expList xs = expList' 0 $ reverse xs
expList' :: Int -> [Integer] -> [Integer]
expList' n [] = []
expList' n (x:xs) = expList' (n + 1) xs ++ [x * (2^n)]

-- Inspired by 'mordo'
-- https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell
toBinary :: Integer -> [Integer]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]
