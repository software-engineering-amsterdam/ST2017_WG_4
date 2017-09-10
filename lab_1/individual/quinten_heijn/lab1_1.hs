-- Assignment: Lab1
-- Exercise: 1
-- Student: Quinten Heijn
-- Time needed: 20 minutes
--------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

-- Functions for exercise 2 from the workshop.
---------------------------------------------------------------------------
myTest2 :: Int -> Bool
myTest2 n = fun2 $ abs n

fun2 :: Int -> Bool
fun2 n =  powerSum n 2 == (n * (n + 1) * (2 * n + 1)) `div` 6

powerSum :: Int -> Int -> Int
powerSum n p = sum $ map (^p) [1 .. n]


-- Functions for exercise 3 from the workshop.
---------------------------------------------------------------------------
myTest3 :: Int -> Bool
myTest3 n = fun3 $ abs n

fun3 :: Int -> Bool
fun3 n =  powerSum n 3 == ((n * (n + 1)) `div` 2) ^ 2
