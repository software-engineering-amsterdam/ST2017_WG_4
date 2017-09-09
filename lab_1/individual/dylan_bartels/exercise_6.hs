import Test.QuickCheck

-- Assignment 5. time: 30 minutes

-- Prime funcion from slides
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
          where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primesProduct :: Integer -> Integer
primesProduct n = (product [x | x <- take (fromIntegral n) primes]) + 1

solve :: Integer
solve = head [x | x <- primes,
                  not $ prime $ primesProduct x]

{--
Gives 7 instead of 6???
--}
