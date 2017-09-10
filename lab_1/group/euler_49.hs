-- Assignment: Lab1
-- Exercise: Euler 49
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
--------------------------------------------------------------------------

import Control.Monad

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
