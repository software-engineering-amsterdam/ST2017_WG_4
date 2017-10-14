-- Assignment: Lab6
-- Exercise: 4
-- Student: Wojciech Czabanski
-- Time needed:  minutes
--------------------------------------------------------------------------

module Assignment_4 where
 
import Data.List
import System.Random
import Lecture6
import Assignment_3

-- how to factor out the IO from IO Bool in primeTestF

-- Show yield a true on prime_test_F

fakePrimes :: Integer -> IO [Integer]
fakePrimes n = takeWhile (\x -> primeTestF x == True) (composites n)
