-- Assignment: Lab3
-- Exercise: 2
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 4 hours 30 min

-- Implement a random data generator for the datatype Set Int, where Set is as
-- defined in SetOrd.hs. First do this from scratch, next give a version that
-- uses QuickCheck to random test this datatype.

-- (Deliverables: two random test generators, indication of time spent.)

-- Information used
-- https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck-Gen.html
-- if/then/else for IO Bool https://mail.haskell.org/pipermail/haskell-cafe/2006-November/019752.html
-- https://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique
-- Test if it's a set (http://geekyplatypus.com/category/haskell/)

--------------------------------------------------------------------------
module Assignment2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Control.Monad

-- From scratch
probs :: Int -> IO [Int]
probs 0 = return []
probs n = do
 p <- getStdRandom (randomR (-50,50))
 ps <- probs (n-1)
 return (p:ps)

-- Creates random set of random length between 0 and 50 containing integers between -50 and 50
genScratchSet :: IO (Set Int)
genScratchSet = do
 n <- getStdRandom (randomR (0,50))
 lst <- probs n
 let set = list2set lst
 return set

-- Using QuickCheck
genQuickCheckSet :: Int -> Gen (Set Int)
genQuickCheckSet 0 = return emptySet
genQuickCheckSet n = do sampleSet <- arbitrary
                        return (list2set sampleSet)

-- Testable properties
prop_isSet :: Set Int -> Bool
prop_isSet (Set x) = isSet x
-- > quickCheck (forAll (sized genQuickCheckSet) prop_isSet)

isSet :: [Int] -> Bool
isSet []     = True
isSet (x:xs) = x `notElem` xs && isSet xs

-- Helper functions
convertIO :: IO (Set Int) -> IO Bool
convertIO x = do
  ioSet <- x
  return (prop_isSet ioSet)

-- Test both generators
test :: IO ()
test =
 do scratchTest1 <- convertIO (genScratchSet)
    when (scratchTest1 == True) $ print "+++ OK, passed 1 custom scrath generator test."
    when (scratchTest1 == False) $ print "Failed"
    scratchTest2 <- convertIO (genScratchSet)
    when (scratchTest2 == True) $ print "+++ OK, passed 1 custom scrath generator test."
    when (scratchTest2 == False) $ print "Failed"
    quickCheck (forAll (sized genQuickCheckSet) prop_isSet)
    return ()
