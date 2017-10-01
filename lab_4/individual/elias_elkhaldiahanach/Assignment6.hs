-- Assignment: Lab4
-- Exercise: 6
-- Student: Elias el Khaldi
-- Time needed: 20 minutes
--------------------------------------------------------------------------

module Assignment6 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd
import Assignment5

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