-- Assignment: Lab2
-- Exercise: 3
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------

module Lab2_3 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Testing properties strength

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1, p2, p3, p4 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = (even x && x > 3) || even x
p4 x = even x

props :: [Int -> Bool]
props = [p1, p2, p3, p4]

domain :: [Int]
domain = [(-10)..10]

propertyEvaluations :: [Int]
propertyEvaluations = map (\l -> foldr (\x acc -> if x then acc+1 else acc) 0 l)
                          (map (\x -> map (\y -> x y) domain) props)

-- The evaluations yield the following result: [4, 14, 11, 11]
-- This means that p1 is the strongest, then come p3 and p4 and p2 is the weakest

-- Time spent: 25 minutes
