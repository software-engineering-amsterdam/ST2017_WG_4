-- Assignment: Lab4
-- Exercise: 10
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 40 min
--------------------------------------------------------------------------
-- Euler 55
-- If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

-- Not all numbers produce palindromes so quickly. For example,

-- 349 + 943 = 1292,
-- 1292 + 2921 = 4213
-- 4213 + 3124 = 7337

-- That is, 349 took three iterations to arrive at a palindrome.
-- Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome.
-- A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
-- Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number
-- is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand,
-- it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists,
-- has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before
-- producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
-- Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
-- How many Lychrel numbers are there below ten-thousand?

module Lab4 where
import Data.List
import System.Random
import Test.QuickCheck  
   
isLychrel' :: Integer -> Int -> Bool
isLychrel' n i | i > 50 = True
               | i > 0 && (show n == reverse (show n)) = False
               | otherwise = isLychrel' (n + read (reverse (show n))) (i + 1)

isLychrel :: Integer -> Bool
isLychrel n = isLychrel' n 0


lychrelCount :: Int
lychrelCount = length (filter isLychrel [10..10000])

-- Manual testing
-- A well known example of a number that cannot be proven to be non-lychrel as per www.p196.org
testLychrelTrue :: Bool
testLychrelTrue = isLychrel 196

-- A non-lychrel number.
testLychrelFalse :: Bool
testLychrelFalse = isLychrel 1221
