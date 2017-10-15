-- Assignment: Lab6
-- Exercise: 5
-- Student: Wojciech Czabanski
-- Time needed: 10  minutes
--------------------------------------------------------------------------

module Assignment_5 where
 
import Data.List
import System.Random
import Lecture6

-- use the carmichael numbers to test the fermats sieve

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       prime (6*k+1), 
       prime (12*k+1), 
       prime (18*k+1) ]

main :: IO ()
main = do

    return ()

       