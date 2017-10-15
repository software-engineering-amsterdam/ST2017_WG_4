{--
Time spent:
Dylan:      2.5h

Info used:
https://stackoverflow.com/questions/13337937/couldnt-match-expected-type-bool-with-actual-type-io-bool
http://primes.utm.edu/glossary/xpage/PRP.html

The following values were found as least composite number (~20 runs):
k=1 - 9
k=2 - 15
k=3 - 33

The value for k has a direct influence of the size of different values being
used for the base of the Fermat's little theorem. There is a possiblity that a
integer p will pass the test while its not a prime these are called probable
primes. The bigger the input for k the more base values will be tested to provide
more certainty on the conclussion if a number is indeed a prime. 
--}
module Assignment_4 where

import Data.List
import System.Random
import Lecture6 hiding (main, composites, exM)
import Assignment_1 hiding (main)
import Assignment_3

main :: IO ()
main = do
  oneK <- leastComposite 1 composites
  print oneK
  twoK <- leastComposite 2 composites
  print twoK
  threeK <- leastComposite 3 composites
  print threeK
  return ()

leastComposite :: Int -> [Integer] -> IO Integer
leastComposite n (x:xs) = do
   y <- primeTestsF n x
   if y then return x else leastComposite n xs
