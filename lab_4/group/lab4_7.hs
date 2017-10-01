-- Assignment: Lab4
-- Exercise: 7
-- Student:Elias el Khaldi
-- Time needed: 90 minutes
--------------------------------------------------------------------------

module Assignment7 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd
import Assignment6
import Assignment5

-- Additional function for counting the number of individual nodes (variables) in the relation, from now on denoted as n
nodes :: Ord a => Rel a -> [a]
nodes [] = []
nodes ((x,y):t) | x == y = (x : (filter (\z -> (not(z == x)) && (not (z == y))) (nodes t))) | otherwise = (x : (filter (\z -> (not(z == x)) && (not (z == y))) (nodes t)))

n :: Ord a =>  Rel a -> Int
n x = length (nodes x)

-- Properties for symClos:
p1,p2,p3,p4 :: Ord a => Rel a -> Bool
-- Length constraint: The length of a symClos of x can at max be double the length of x, and at least the length of x
p1 x = ((length (symClos x)) <= (2 * (length x))) && ((length (symClos x)) >= (length (noDup(x))))
-- After applying symClos once to x, it should always remain the same no matter how many more times you apply it
p2 x = (symClos x) == (symClos (symClos x))
-- All relations in x should be in (symClos x)
p3 x = (list2set x) `subSet` (list2set (symClos x))
-- No new nodes should be included
p4 x = (nodes x) == (nodes (symClos x))

testSym :: Ord a => Rel a -> Bool
testSym a = (p1 a) && (p2 a) && (p3 a) && (p4 a)
-- Test using QC: quickCheck testSym

-- Properties for trClos:
p5, p6, p7, p8 :: Ord a => Rel a -> Bool
-- Length constraint: The length of trClos x should be at least as big as length x and at most n^2 
p5 x = ((length (trClos x)) >= (length (noDup(x)))) && ((length (trClos x)) <= ((n x) ^ 2))
-- All relations in x should be in (trClos x)
p6 x = (list2set x) `subSet` (list2set (trClos x))
-- After applying trClos once to x, it should always remain the same no matter how many more times you apply it
p7 x = (trClos x) == (trClos (trClos x))
-- No new nodes should be included
p8 x = (nodes x) == (nodes (trClos x))

testTra :: Ord a =>  Rel a -> Bool
testTra a = (p5 a) && (p6 a) && (p7 a) && (p8 a)
-- Test using QC: quickCheck testTra

