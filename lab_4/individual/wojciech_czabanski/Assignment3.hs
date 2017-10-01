-- Assignment: Lab4
-- Exercise: 3
-- Student: Wojciech Czabanski
-- Time needed: 30 minutes
--------------------------------------------------------------------------

module Assignment3 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = Set (nub (a ++ b))

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set (intersect a b)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set (a \\ b)

-- Manual testing 
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
