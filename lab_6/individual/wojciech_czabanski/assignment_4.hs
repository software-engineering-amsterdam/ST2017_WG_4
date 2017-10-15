-- Assignment: Lab6
-- Exercise: 4
-- Student: Wojciech Czabanski
-- Time needed: 20 minutes
--------------------------------------------------------------------------

module Assignment_4 where
 
import Data.List
import System.Random
import Lecture6 hiding (composites)
import Assignment_3

fakePrimes :: Integer -> IO [Integer]
fakePrimes n = filter (\x -> primeTestF x) (take 100 composites)

main :: IO ()
main = do
    fakePrimes 100
    return ()