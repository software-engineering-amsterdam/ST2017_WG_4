import Test.QuickCheck

-- Assignment 5. time: 30 minutes

-- Prime funcion from slides
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
          where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

takePrimes :: Int -> Integer
takePrimes n = if prime (sum $ take 101 $ reverse $ take n primes)
               then sum $ take 101 $ reverse $ take n primes
               else takePrimes (n+1)

solve :: Integer
solve = takePrimes 101

{--
answer 37447

todo: It can be checked by:
--}
