import Test.QuickCheck

-- Assignment 1. time: 45 minutes

listSumPower :: Int -> Int -> Int
listSumPower x y = sum $ map (^y) [1..x]

exercise2 :: Int -> Bool
exercise2 x = listSumPower x 2 == (x * (x+1) * (2 * x+1)) `div` 6

exercise3 :: Int -> Bool
exercise3 x = listSumPower x 3 == ((x * (x+1)) `div` 2)^2

solve :: Positive Int -> Bool
solve (Positive n) = exercise2 n && exercise3 n
