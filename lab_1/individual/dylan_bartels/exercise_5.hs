import Test.QuickCheck

-- Assignment 5. time: 30 minutes

primeList :: [Integer]
primeList = [x | x <- [1..], prime x]

takePrimes :: Int -> Integer
takePrimes n
  | n >= 101  = if prime (sum $ take 101 $ reverse $ take n primeList)
                then sum $ take 101 $ reverse $ take n primeList
                else takePrimes (n+1)
  | otherwise = error "Slice needs to be at least 101"
