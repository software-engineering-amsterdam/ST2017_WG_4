-- Assignment: Lab4
-- Exercise: 5
-- Student: Quinten Heijn
-- Time needed: ??
--------------------------------------------------------------------------

module Assignment5 where

import Data.List
import System.Random
import Test.QuickCheck

type Rel a = [(a,a)]

relComparer :: Ord a => (a, a) -> (a, a) -> Ordering

flipPairs :: Ord a => Rel a -> Rel a

symClos :: Ord a => Rel a -> Rel a

testClos :: Rel Int
