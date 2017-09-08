import Test.QuickCheck

-- Assignment 7.

luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9 then (x*2)-9 else x*2

luhn :: Int -> Bool
luhn n = if (luhnSum n) `mod` 10 == 0 then True else False

luhnSum :: Int -> Int
luhnSum n = (n `div` 10000000000) +
            luhnDouble (n `mod` 1000000000) +
            (n `mod` 100000000) +
            luhnDouble (n `mod` 10000000) +
            (n `mod` 1000000) +
            luhnDouble (n `mod` 100000) +
            (n `mod` 10000) +
            luhnDouble (n `mod` 1000) +
            (n `mod` 100) +
            luhnDouble (n `mod` 10)


-- checkDigit :: Int -> Int
-- checkDigit n = ((luhnSum n)*9) `mod` 10
