module Lab1_6 where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes

checkFoldPrimeList :: [Int] -> Bool
checkFoldPrimeList primes = isPrime (product primes + 1)

checkConsPrimes :: Int -> Bool
checkConsPrimes n = checkFoldPrimeList (take n primes)

genRefutations :: Int -> [Int]
genRefutations n = filter (not . checkConsPrimes) [2..n]

-- The smallest counterexample n = 6.

-- using library Primes

-- Time: 30 minutes
