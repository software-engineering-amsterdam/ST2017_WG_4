-- Assignment: Lab6
-- Exercise: 1
-- Student: Wojciech Czabanski
-- Time needed: 40 minutes
--------------------------------------------------------------------------

module Assignment_1 where
 
import Data.List
import System.Random

powersOf2UntilN :: Integer -> [Integer]
powersOf2UntilN n = takeWhile (\v -> v <= n) [2^x | x <- [1..]]

powerIterations :: Integer -> Integer
powerIterations n = toInteger (length (powersOf2UntilN n))

powerRemainder :: Integer -> Integer
powerRemainder x | (powersOf2UntilN x) /= [] = x - (head (reverse (powersOf2UntilN x)))
                 | otherwise = 0

iteratedModuloPower :: Integer -> Integer -> Integer -> Integer -> Integer
iteratedModuloPower x y modulo power | power < y = (x * (iteratedModuloPower x y modulo (power+1))) `mod` modulo
                                     | otherwise = 1

expModPower :: Integer -> Integer -> Integer -> Integer -> Integer
expModPower x y modulo power | power < y = ((expModPower x y modulo (power+1)) * (expModPower x y modulo (power+1))) `mod` modulo
                             | otherwise = x

exM :: Integer -> Integer -> Integer -> Integer
exM x y mod = (expModPower x (powerIterations y) mod 0) * (iteratedModuloPower x (powerRemainder y) mod 0)
