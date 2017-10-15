{--
Time spent:
Dylan:      1h

Straight away it's noticable that output found with k1,2,3 is higher
than with the sieve generated list. This is due to the output being fermat's
psuedoprimes. The composite integer n is a carmichael number (pseudoprime) if
a^(n-1) = 1 (mod n) for every integer a relatively prime to n. The Fermat
probable primality test will fail to show a carmichael number is composite
untill we run across one of its factors.

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
