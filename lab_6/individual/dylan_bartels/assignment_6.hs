{--
Time spent:
Dylan:      h
--}
module Assignment_6 where

import Data.List
import System.Random
import Lecture6 hiding (composites)
import Assignment_3 hiding (main)
import Assignment_5 hiding (main)

main :: IO ()
main = do
  oneK <- primeMR 1 carmichael
  print oneK
  twoK <- primeMR 2 carmichael
  print twoK
  threeK <- primeMR 3 carmichael
  print threeK
  return ()
