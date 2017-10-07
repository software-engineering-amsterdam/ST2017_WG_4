-- Assignment: Lab4
-- Exercise: 2
-- Student: Quinten Heijn
-- Time needed: ??
--------------------------------------------------------------------------

module Assignment2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrds

genSet :: Set (IO Int)
genSet = (genSet' emptySet $ getStdRandom random)

genSet' :: Set (IO Int) -> IO Int -> Set (IO Int)
genSet' s 0 = s
genSet' s n = insertSet (getStdRandom random) $ genSet' s (n-1)
