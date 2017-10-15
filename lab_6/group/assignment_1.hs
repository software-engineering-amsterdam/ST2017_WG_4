-- Assignment: Lab6
-- Exercise: 1
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time spent:
-- Dylan:      1.5h
-- Quinten:    3.0h
-- Wojciech:   1.25h
-- Total:      4.75h
--------------------------------------------------------------------------
module Assignment_1 where
    
import Data.List
import System.Random
import Test.QuickCheck
import Lecture6 hiding (exM)

-- Solution that uses the squaring of modulos.
exM :: Integer -> Integer -> Integer -> Integer
exM x expo modu = mod (product (squaring (reverse $ toBinary expo) [] x modu)) modu

-- Solution that doesn't use the squaring of modulos, but just breaks down the
-- exponent in exponents with base 2.
exM2 :: Integer -> Integer -> Integer -> Integer
exM2 x expo modu = mod (product [mod (x^y) modu | y <- expList $ toBinary expo]) modu

squaring :: [Integer] -> [Integer] -> Integer -> Integer -> [Integer]
squaring [] solvedList x modu = solvedList
squaring (0:xs) ys x modu = squaring' xs ys newx modu
    where newx = mod x modu
squaring (1:xs) ys x modu = squaring' xs ([newx] ++ ys) newx modu
    where newx = mod x modu

squaring' :: [Integer] -> [Integer] -> Integer -> Integer -> [Integer]
squaring' [] solvedList x modu = solvedList
squaring' (0:xs) ys x modu = squaring' xs ys newx modu
    where newx = mod (x*x) modu
squaring' (1:xs) ys x modu = squaring' xs ([newx] ++ ys) newx modu
    where newx = mod (x*x) modu

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


-- Test to see if both implementations give the same results.
test1 :: IO ()
test1 = quickCheck test1'

test1' :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
test1' (Positive x) (Positive y) (Positive z) = (expM x y z) == (exM x y z)
    