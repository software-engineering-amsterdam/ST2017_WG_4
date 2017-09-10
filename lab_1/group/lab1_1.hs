-- Assignment: Lab1
-- Exercise: 1
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
--------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

-- Functions for exercise 2 from the workshop.
---------------------------------------------------------------------------

-- myTest2 tests exercise 2 with only natural numbers.
myTest2 :: Int -> Bool
myTest2 n = fun2 $ abs n

-- The function that is tested in this exercise.
fun2 :: Int -> Bool
fun2 n =  powerSum n 2 == (n * (n + 1) * (2 * n + 1)) `div` 6

-- powerSum takes the sum over a list after exponating every element of the list with p.
powerSum :: Int -> Int -> Int
powerSum n p = sum $ map (^p) [1 .. n]


-- Functions for exercise 3 from the workshop.
---------------------------------------------------------------------------

-- myTest3 tests exercise 3 with only natural numbers.
myTest3 :: Int -> Bool
myTest3 n = fun3 $ abs n

-- The function that is tested in this exercise.
fun3 :: Int -> Bool
fun3 n =  powerSum n 3 == ((n * (n + 1)) `div` 2) ^ 2
