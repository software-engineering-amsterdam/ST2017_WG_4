-- Assignment: Lab4
-- Exercise: 5
-- Student: Elias el Khaldi
-- Time needed: 5 minutes
--------------------------------------------------------------------------

module Assignment5 where
    
import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

type Rel a = [(a,a)]
--
noDup :: Ord a => Rel a -> Rel a
noDup lst = set2list(list2set lst)

set2list :: Ord a => Set a -> [a]
set2list (Set lst) = lst
--
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):t) | not (x == y) = noDup((x,y):(y,x):(symClos t)) | otherwise = noDup((x,y):(symClos t))