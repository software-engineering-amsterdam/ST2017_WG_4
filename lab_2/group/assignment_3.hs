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

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- All properties from Exercise 3
prop1 :: Int -> Bool
prop1 x = even x && x > 3

prop2 :: Int -> Bool
prop2 x = even x || x > 3

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
prop4 x = (even x && x > 3) || even x

-- Consider a small domain like [(−10)..10] as input
testPropositions :: Int -> Int -> [Int]
testPropositions x y = [x..y]

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

{--
a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell
functions of type Int -> Bool. Consider a small domain like [(−10)..10].

b) Provide a descending strength list of all the implemented properties.
--}
