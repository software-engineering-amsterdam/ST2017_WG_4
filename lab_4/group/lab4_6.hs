-- Assignment: Lab4
-- Exercise: 6
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi 
-- Time needed: 50 minutes
--------------------------------------------------------------------------
-- Use the datatype for relations from the previous exercise, plus {...} to define a function
-- trClos :: Ord a => Rel a -> Rel a 
-- that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] 
-- should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-- (Deliverable: Haskell program, indication of time spent.)
--------------------------------------------------------------------------

module Lab4_6 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd
import Lab4_5

--
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
 nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Informal description transitive closure:
-- If there is a path from a to b, then there should be an edge from a to b
trClos :: Ord a => Rel a -> Rel a 
trClos x = trClos2 (length x) x

trClos2 :: Ord a => Int -> Rel a -> Rel a
trClos2 i x | (length (noDup(x ++ (x @@ x)))) == i = x | otherwise = trClos2 (length (noDup(x ++ (x @@ x)))) (noDup(x ++ (x @@ x)))

noDup :: Ord a => Rel a -> Rel a
noDup lst = set2list(list2set lst)

set2list :: Ord a => Set a -> [a]
set2list (Set lst) = lst
