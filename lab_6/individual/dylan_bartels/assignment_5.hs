{--
Time spent:
Dylan:      1h
--}
module Assignment_5 where

import Data.List
import System.Random
import Lecture6 hiding (composites)
import Assignment_4 hiding (main)

main :: IO ()
main = do
  oneK <- leastComposite 1 carmichael
  print oneK
  twoK <- leastComposite 2 carmichael
  print twoK
  threeK <- leastComposite 3 carmichael
  print threeK
  return ()

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]
