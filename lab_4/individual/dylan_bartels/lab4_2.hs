-- Assignment: Lab3
-- Exercise: 2
-- Student: Dylan Bartels
-- Time needed: 10 min

-- Implement a random data generator for the datatype Set Int, where Set is as
-- defined in SetOrd.hs. First do this from scratch, next give a version that
-- uses QuickCheck to random test this datatype.

-- (Deliverables: two random test generators, indication of time spent.)

--------------------------------------------------------------------------
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- From scratch

-- Using QuickCheck

instance (Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    list <- arbitrary
    return $ Set list
-- > sample $ (arbitrary :: Gen (Set Int))
