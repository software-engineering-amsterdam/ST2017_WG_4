module Lab1 where
import Data.List
import Test.QuickCheck
import Control.Monad

-- Assignment 1. time: 45 minutes

listSumPower :: Int -> Int -> Int
listSumPower x y = sum $ map (^y) [1..x]

exercise2 :: Int -> Bool
exercise2 x = listSumPower x 2 == (x * (x+1) * (2 * x+1)) `div` 6

exercise3 :: Int -> Bool
exercise3 x = listSumPower x 3 == ((x * (x+1)) `div` 2)^2

test :: Int -> Bool
test n
  | n < 0     = (exercise2 (abs n) && exercise3 (abs n))
  | otherwise = (exercise2 n && exercise3 n)

-- Assignment 2. time: 10 minutes

exercise4 :: Int -> Int
exercise4 n = length (subsequences [1..n])

test2 :: Int -> Bool
test2 n
  | n < 0     = (exercise4 (abs n) == 2 ^ (abs n))
  | otherwise = (exercise4 n == 2 ^ n)

-- Is the property hard to test? If you find that it is, can you given a reason why?

-- The property is hard to test due to the length of the sequences expanding by
-- the power of 2 every increment. This is very intensive to calculate a batch
-- of the default 100 tests that QuickCheck executes


-- Give your thoughts on the following issue: when you perform the test for
-- exercise 4, what are you testing actually?

-- What is being tested is if the length of subsequences of a list is equal to
-- 2^n, whereby n is the biggest number in the list. QuickCheck executes this
-- test with 100 random numbers.

-- Are you checking a mathematical fact?
-- Or are you testing whether subsequences satisfies a part of its specification?
-- Or are you testing something else still?

-- What is being checked is a mathematical fact.

-- Assignment 3. time: 10 minutes

fact :: Int -> Int
fact n = product [1..n]

test3 :: Int -> Bool
test3 n = length (permutations [1..(abs n)]) == fact (abs n)

-- Assignment 4. time: 10 minutes

-- Prime funcion from slides
prime :: Integer -> Bool
prime n =  n > 1 && all (\ x -> rem n x /= 0) xs
 where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

test4 :: Integer -> [Integer]
test4 n = [x | x <- [1..n],
               prime x,
               prime (reversal x)]

-- todo: how to test it?

-- Assignment 5. time: 30 minutes

primeList :: [Integer]
primeList = [x | x <- [1..], prime x]

takePrimes :: Int -> Integer
takePrimes n
  | n >= 101  = if prime (sum $ take 101 $ reverse $ take n primeList)
                then sum $ take 101 $ reverse $ take n primeList
                else takePrimes (n+1)
  | otherwise = error "Slice needs to be at least 101"

-- Assignment 6.

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

-- Assignment 8.

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew Peter  = True
accuses Matthew Jack   = True
accuses Matthew Arnold = True
accuses Peter Matthew  = True
accuses Peter Jack     = True
accuses Jack Matthew   = True
accuses Jack Carl      = True
accuses Arnold Carl    = True
accuses Arnold Matthew = True
accuses Arnold Jack    = True
accuses Carl Arnold    = True
accuses Carl Peter     = True
accuses _ _            = False

accusers :: Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Peter   = [Matthew, Carl]
accusers Jack    = [Matthew, Peter, Arnold]
accusers Arnold  = [Matthew, Carl]
accusers Carl    = [Jack, Arnold]

-- guilty :: [Boy]
-- guilty = permutations [True, True, True, False, False] -- Has duplicates

-- Project Euler 9. Time: 10 minutes, answer: (200,375,425)

pythagoreanTriplet :: (Int, Int, Int)
pythagoreanTriplet = head [(a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b],
                                     a^2+b^2 == c^2, a+b+c == 1000]

-- Project Euler 10. time: 5 minutes, answer: 142913828922

sumPrimes :: Integer
sumPrimes = sum $ takeWhile (<2000000) primes

-- Project Euler 49. time: 1 hour

fourDigitPermutations :: Integer -> [Integer]
fourDigitPermutations x = filter (>999) $ map (read::String->Integer)
                          (map nub (replicateM 4 (show x)))

fourDigitPrimes :: [Integer]
fourDigitPrimes = filter prime [1000..10000]

arePermutation :: (Integer, Integer, Integer) -> Bool
arePermutation (x,y,z) = if (isPermutation x y && isPermutation y z) then
                         True else False

isPermutation :: Integer -> Integer -> Bool
isPermutation x y
  | x `elem` fourDigitPermutations y = True
  | otherwise                        = False

solve :: [(Integer, Integer, Integer)]
solve = [(x,y,z) | x <- fourDigitPrimes,
                   y <- fourDigitPrimes,
                   z <- fourDigitPrimes,
                   z - y == y - x,
                   z /= y,
                   arePermutation (x,y,z)]
