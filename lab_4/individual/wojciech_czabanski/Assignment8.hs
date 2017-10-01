-- Assignment: Lab4
-- Exercise: 8
-- Student: Wojciech Czabanski
-- Time needed:  minutes
--------------------------------------------------------------------------

module Assignment8 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import Assignment5 
import Assignment6

symTrClos :: Ord a => Rel a -> Rel a
symTrClos = trClos . symClos

trSymClos :: Ord a => Rel a -> Rel a
trSymClos = symClos . trClos

testRel :: Rel Int
testRel = [(1,2), (2,3), (3,4)]