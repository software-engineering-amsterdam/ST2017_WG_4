-- Assignment: Lab6
-- Exercise: 3
-- Student: Wojciech Czabanski
-- Time needed: 25 minutes
--------------------------------------------------------------------------

module Assignment_3 where
 
import Data.List
import System.Random
import Lecture6 hiding (composites)

composites :: [Integer]
composites = filter (\x -> (elem x (takeWhile (<=x) primes)) == False) [3..]
