import Data.List
import Test.QuickCheck

-- Assignment 3. time: 10 minutes

fact :: Int -> Int
fact n = product [1..n]

solve :: Positive Int -> Bool
solve (Positive n) = length (permutations [1..n]) == fact n
