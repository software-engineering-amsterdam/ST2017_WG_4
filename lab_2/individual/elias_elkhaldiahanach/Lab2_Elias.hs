module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Exercise 1
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
 p <- getStdRandom random
 ps <- probs (n-1) 
 return (p:ps)

checkIntervals :: [Float] -> (Int,Int,Int,Int)
checkIntervals list = (countInInterval list 0.0 0.25, countInInterval list 0.25 0.5, countInInterval list 0.5 0.75, countInInterval list 0.75 1.0) 

countInInterval ::  [Float] -> Float -> Float -> Int
countInInterval [] a b = 0
countInInterval (h:t) a b | (h >= a) && (h < b) = 1 + countInInterval t a b | otherwise = countInInterval t a b

testSignificance :: (Int,Int,Int,Int) -> Int -> Bool
testSignificance (a,b,c,d) n = testZ ((fromIntegral a) / n) 0.25 n || testZ ((fromIntegral b) / n) 0.25 n 

testZ :: Float -> Float -> Int -> Bool
testZ m m0 n do
 z <- (m - m0) 

test :: Int -> IO Bool
test a = do 
 x <- probs a 
 i <- checkIntervals x
 b <- testSignificance i a
 return b  

--Exercise 2