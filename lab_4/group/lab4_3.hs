-- Assignment: Lab4
-- Exercise: 3
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 45 minutes
--------------------------------------------------------------------------
-- Implement operations for set intersection, set union and set difference,
-- for the datatype Set defined in SetOrd.hs. Next, use automated testing to
-- check that your implementation is correct. First use your own generator,
-- next use QuickCheck.
--------------------------------------------------------------------------

module Lab4_3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

------------------------------
-- Generators
------------------------------

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

------------------------------
-- Operations
------------------------------

setIntersection, setUnion, setDifference :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (xs `intersect` ys)
setUnion        (Set xs) (Set ys) = list2set (xs ++ ys)
setDifference   (Set xs) (Set ys) = list2set (xs \\ ys)

------------------------------
-- Properties
------------------------------

{--
Testable properties
setIntersection:
  -
setUnion:
  - Commutativity   = A (union) B = B (union) A
  - Contains itself = A (union) B should contain every element from A
setDifference:
  -
 --}

prop_unionCommutativity :: Set Int -> Set Int -> Bool
prop_unionCommutativity x y = unionSet x y == unionSet y x

prop_unionContainSelf :: Set Int -> Set Int -> Bool
prop_unionContainSelf (Set xs) (Set ys) = all (\x -> inSet x (unionSet (Set ys) (Set xs))) xs


------------------------------
-- Helper (to let scratch generator accept two generated sets)
------------------------------

testScratchGenerator :: Int -> (Set Int -> Set Int -> Bool) -> IO ()
testScratchGenerator testCounter f =
  if testCounter == 0 then putStrLn $ id ("+++ OK, passed 100 custom scrath generator tests.")
  else do
    set1 <- genScratchSet
    set2 <- genScratchSet
    if (f set1 set2) then testScratchGenerator (testCounter-1) f
    else error ("Failed test")

------------------------------
-- Main automated test
------------------------------

test :: IO ()
test =
 do testScratchGenerator 100 (prop_unionCommutativity)
    testScratchGenerator 100 (prop_unionContainSelf)
    return ()
