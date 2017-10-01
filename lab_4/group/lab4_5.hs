-- Assignment: Lab3
-- Exercise: 5
-- Student: Dylan Bartels
-- Time needed: 30 min

-- Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the following definition:
-- > type Rel a = [(a,a)]
-- Use this to implement a function
--
--   symClos :: Ord a => Rel a -> Rel a
-- that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. E.g.,  symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
--
-- (Deliverable: Haskell program, indication of time spent.)

--------------------------------------------------------------------------
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

type Rel a = [(a, a)]

relComparer :: Ord a => (a, a) -> (a, a) -> Ordering
relComparer a b | (fst a) < (fst b) = LT
                | (fst a) > (fst b) = GT
                | ((fst a) == (fst b)) && ((snd a) < (snd b)) = LT
                | ((fst a) == (fst b)) && ((snd a) > (snd b)) = GT

flipPairs :: Ord a => Rel a -> Rel a
flipPairs as = map (\x -> (snd x, fst x)) as

symClos :: Ord a => Rel a -> Rel a
symClos as = sortBy relComparer (as ++ (flipPairs as))

testClos :: Rel Int
testClos = [(1,2),(2,3),(3,4)]
