import Test.QuickCheck

-- Assignment 4. time: 10 minutes

-- Prime funcion from slides
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
          where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

solve :: Integer -> [Integer]
solve n = [x | x <- [1..n],
               prime x,
               prime (reversal x)]

-- todo: how to test it?
