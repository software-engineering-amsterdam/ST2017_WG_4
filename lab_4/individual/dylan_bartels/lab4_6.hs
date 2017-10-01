-- Assignment: Lab3
-- Exercise: 6
-- Student: Dylan Bartels
-- Time needed: 30 min

-- Use the datatype for relations from the previous exercise, plus
-- > infixr 5 @@
-- >
-- > (@@) :: Eq a => Rel a -> Rel a -> Rel a
-- > r @@ s =
-- >   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
-- to define a function
--
--  trClos :: Ord a => Rel a -> Rel a
-- that gives the transitive closure of a relation, represented as an ordered
-- list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] should give
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
--
-- (Deliverable: Haskell program, indication of time spent.)

--------------------------------------------------------------------------
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{--
In a sorted list of pairs the first and last pair is always the first and last
pair in a list of the transitive list of pairs.
--}

trClos :: Ord a => Rel a -> Rel a
trClos n = sort (n ++ (n @@ n))

-- trClos testClos is missing (1,4)
testClos :: Rel Int
testClos = [(1,2),(2,3),(3,4)]

-- trClos testClos' has double (3,3)
testClos' :: Rel Int
testClos' = [(4,5),(3,3),(1,4)]
