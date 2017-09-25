-- Assignment: Lab4
-- Exercise: 5
-- Student: Wojciech Czabanski
-- Time needed: 15 minutes
--------------------------------------------------------------------------

module Assignment5 where
    
import Data.List
import System.Random
import Test.QuickCheck  

type Rel a = [(a,a)]

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
