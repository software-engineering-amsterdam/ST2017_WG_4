-- Assignment: Lab3
-- Exercise: 2
-- Student: Dylan Bartels
-- Time needed: 30 min

-- Implement operations for set intersection, set union and set difference, for
-- the datatype Set defined in SetOrd.hs. Next, use automated testing to check
-- that your implementation is correct. First use your own generator, next use
-- QuickCheck.

-- (Deliverables: implementations, test properties, short test report,
-- indication of time spent.)

--------------------------------------------------------------------------
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

data Operation = Union (Set a) (Set a)
               | Difference (Set a) (Set a)
               deriving (Eq,Show)

eval :: Operation -> Bool
eval Union x y      = forAll
eval Difference x y =
