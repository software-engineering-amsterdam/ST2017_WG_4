-- Assignment: Lab6
-- Exercise: 2
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time spent:
-- Quinten:    2.0h
-- Wojciech:   0.75h
-- Total:      2.75h
--------------------------------------------------------------------------
-- See Assignment_2_chart.png for the results.

-- The test were done using ":set +s" in ghci.
-- The parameter for the function were '10 exp 111', with 'exp' being variable
-- since the exponent has the biggest impact on the execution time.

-- exM is clearly a lot faster than expM. So much so that it was difficult to test
-- the differences between them using ghci tool. Once expM was getting too slow
-- to test, exM was still running within miliseconds.

-- exM2, that does not use squiring, was still a lot faster than expM, altough being
-- of the same complexity: roughly twice as fast.

module Assignment_2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6 hiding (exM)
import Assignment_1

prop_expEquivalence :: Integer -> Integer -> Integer -> Bool
prop_expEquivalence x y mod | mod /= 0 = (exM (abs x) (abs y) (abs mod)) == (expM (abs x) (abs y) (abs mod))
                            | otherwise = (exM (abs x) (abs y) 2) == (expM (abs x) (abs y) 2)

testQuickCheck :: IO ()
testQuickCheck = do
  print ("QuickCheck test for equivalence of exM and expM")
  quickCheck prop_expEquivalence  
  return ()
