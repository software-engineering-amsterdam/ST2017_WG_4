import Data.List
import Test.QuickCheck

-- Assignment 3. time: 10 minutes

factorial :: Int -> Int
factorial n = product [1..n]

solve :: Positive Int -> Bool
solve (Positive n) = length (permutations [1..n]) == factorial n

{--
verboseCheck solve

Input bigger than ~12 will get very slow
--}
