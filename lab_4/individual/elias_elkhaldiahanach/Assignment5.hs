-- Assignment: Lab4
-- Exercise: 5
-- Student: Elias el Khaldi
-- Time needed: 5 minutes
--------------------------------------------------------------------------

module Assignment5 where
    
import Data.List
import System.Random
import Test.QuickCheck  

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):t) | not (x == y) = (x,y):(y,x):(symClos t) | otherwise = (x,y):(symClos t)