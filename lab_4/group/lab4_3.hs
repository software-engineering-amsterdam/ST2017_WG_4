-- Assignment: Lab4
-- Exercise: 3
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 45 minutes
--------------------------------------------------------------------------
-- Implement operations for set intersection, set union and set difference, for the datatype Set
-- defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct.
-- First use your own generator, next use QuickCheck.
--------------------------------------------------------------------------

module Lab4_3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

setIntersection, setUnion, setDifference :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (xs `intersect` ys)
setUnion        (Set xs) (Set ys) = list2set (xs ++ ys)
setDifference   (Set xs) (Set ys) = list2set ((xs \\ ys) ++ (ys \\ xs))

------------------------------
-- Manual testing
------------------------------
testSetA, testSetB :: Set Int
testSetA = Set [1,2,3]
testSetB = Set [2,3,4]

testUnion, testIntersection, testDiff1, testDiff2 :: Set Int

-- Expected output: {1,2,3,4}
testUnion = setUnion testSetA testSetB

-- Expected output: {2,3}
testIntersection = setIntersection testSetA testSetB

-- Expected output: {1}
testDiff1 = setDifference testSetA testSetB

-- Expected output: {4}
testDiff2 = setDifference testSetB testSetA

------------------------------
-- Automated testing
------------------------------

-- Testable properties
-- setIntersection :
-- setUnion:         all inSet of both input
-- setDifference:


-- Main test
-- test :: IO ()
-- test =
--  do ifM (convertIO (genSet 100 0 1000))
--       (print "+++ OK, passed 1 custom scrath generator test.")
--       (print "Failed")
--     ifM (convertIO (genSet 10 0 10))
--       (print "+++ OK, passed 1 custom scrath generator test.")
--       (print "Failed")
--     quickCheck (forAll (sized genQuickCheckSet) prop_isSet)
--     return ()
