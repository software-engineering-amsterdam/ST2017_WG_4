-- Assignment: Lab4
-- Exercise: 6
-- Student: Wojciech Czabanski
-- Time needed: 20 minutes
--------------------------------------------------------------------------

module Assignment6 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import Assignment5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos a = sortBy relComparer (a ++ (a @@ a))

testTrClos :: Rel Int
testTrClos = [(1,2),(2,3),(3,4)]

-- expected: [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]

