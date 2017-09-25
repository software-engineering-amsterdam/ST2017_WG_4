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

getDomain :: Ord a => Rel a -> [a]
getDomain as = map (\x -> fst x) as

getRange :: Ord a => Rel a -> [a]
getRange as = map (\x -> snd x) as

--closureTransform :: Ord a => Rel a -> Rel a 
--closureTransform as = zipWith trClosureJoin (getDomain as) (getRange as)

-- Plan: 
-- For every element 'a' of the domain
--  Create a pair with element 'b' from the range (codomain) iif 'a' < 'b'
-- Join lists
-- Remove duplicated
-- Order

trClos :: Ord a => Rel a -> Rel a 
trClos a = sortBy relComparer a

testTrClos :: Rel Int
testTrClos = [(1,2),(3,4),(2,3)]

-- expected: [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]

