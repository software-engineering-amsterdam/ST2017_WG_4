-- Assignment: Lab6
-- Exercise: 2
-- Student: Wojciech Czabanski
-- Time needed: 45 minutes
--------------------------------------------------------------------------

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
  print ("QuickCheck equivalence test")
  quickCheck prop_expEquivalence  
  return ()

performanceTest_exM :: IO ()
performanceTest_exM = do
  print ("Performance test for exM")
  print (exM 2 1234567 7)
  return ()

performanceTest_expM :: IO ()
performanceTest_expM = do
  print ("Performance test for expM")
  print (expM 2 1234567 7)
  return ()
  