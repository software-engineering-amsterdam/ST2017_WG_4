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
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
