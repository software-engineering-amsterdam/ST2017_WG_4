-- Assignment: Lab3
-- Exercise: 2
-- Student: Dylan Bartels
-- Time needed: 4 hours 30 min

-- Implement a random data generator for the datatype Set Int, where Set is as
-- defined in SetOrd.hs. First do this from scratch, next give a version that
-- uses QuickCheck to random test this datatype.

-- (Deliverables: two random test generators, indication of time spent.)

-- https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck-Gen.html

--------------------------------------------------------------------------
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Control.Monad

-- From scratch

genSet :: Int -> Int -> Int -> IO (Set Int)
genSet n mini maxi
  | maxi - mini < n = error "Input out of bound"
  | otherwise = do randomSet <- genList mini maxi n
                   return (list2set randomSet)

genList :: Int -> Int -> Int -> IO ([Int])
genList _ _ 0 = do return []
genList maxi mini n = do random <- randomRIO (maxi, mini)
                         listCounter <- genList mini maxi (n-1)
                         if elem random listCounter
                           then (genList mini maxi n)
                           else return (random : listCounter)
-- > genList 10 0 5

-- Using QuickCheck
genQuickCheckSet :: Int -> Gen (Set Int)
genQuickCheckSet 0 = return emptySet
genQuickCheckSet n = do sampleSet <- arbitrary
                        return (list2set sampleSet)

-- Test if it's a set (http://geekyplatypus.com/category/haskell/)
prop_isSet :: Set Int -> Bool
prop_isSet (Set x) = isSet x
-- > quickCheck (forAll (sized genQuickCheckSet) prop_isSet)

-- https://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique
isSet :: [Int] -> Bool
isSet []     = True
isSet (x:xs) = x `notElem` xs && isSet xs

convertIO :: IO (Set Int) -> IO Bool
convertIO x = do
  ioSet <- x
  return (prop_isSet ioSet)

-- if/then/else for IO Bool https://mail.haskell.org/pipermail/haskell-cafe/2006-November/019752.html
if' b t e = if b then t else e
ifM = liftM3 if'

-- Test both generators
test :: IO ()
test =
 do ifM (convertIO (genSet 100 0 1000))
      (print "+++ OK, passed 1 custom scrath generator test.")
      (print "Failed")
    ifM (convertIO (genSet 10 0 10))
      (print "+++ OK, passed 1 custom scrath generator test.")
      (print "Failed")
    quickCheck (forAll (sized genQuickCheckSet) prop_isSet)
    return ()
