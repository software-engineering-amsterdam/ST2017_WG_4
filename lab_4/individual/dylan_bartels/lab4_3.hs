-- Assignment: Lab3
-- Exercise: 2
-- Student: Dylan Bartels
-- Time needed: 30 min

-- Implement operations for set intersection, set union and set difference, for
-- the datatype Set defined in SetOrd.hs. Next, use automated testing to check
-- that your implementation is correct. First use your own generator, next use
-- QuickCheck.

-- (Deliverables: implementations, test properties, short test report,
-- indication of time spent.)

--------------------------------------------------------------------------
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

setIntersection, setUnion, setDifference :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (xs `intersect` ys)
setUnion        (Set xs) (Set ys) = list2set (xs ++ ys)
setDifference   (Set xs) (Set ys) = list2set ((xs \\ ys) ++ (ys \\ xs))
-- > setIntersection (list2set [1..10]) (list2set [5..12])

-- todo: tests
