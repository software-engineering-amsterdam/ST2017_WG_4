-- Assignment: Lab2
-- Exercise: 1
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
 p <- getStdRandom random
 ps <- probs (n-1) 
 return (p:ps)

checkIntervals :: [Float] -> [Int]
checkIntervals list = countInInterval list 0.0 0.25 : countInInterval list 0.25 0.5 : countInInterval list 0.5 0.75 : countInInterval list 0.75 1.0 : [] 

countInInterval ::  [Float] -> Float -> Float -> Int
countInInterval [] a b = 0
countInInterval (h:t) a b | (h >= a) && (h < b) = 1 + countInInterval t a b | otherwise = countInInterval t a b

distribution :: Int -> IO [Int]
distribution n = do
 lst <- probs n
 let zx = checkIntervals lst
 return zx
 
test :: [Int] -> Float -> Bool
test [] n = True
test (h:t) n | (fromIntegral h) <= (n/4 + (n/4 * 0.5)) &&  (fromIntegral h)  >= (n/4 - (n/4 * 0.5)) = test t n | otherwise = False

runTest :: Int -> IO Bool 
runTest n = do
 lst <- distribution n
 let zx = test lst (fromIntegral n)
 return zx
