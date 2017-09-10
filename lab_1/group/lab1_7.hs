-- Assignment: Lab1
-- Exercise: 7
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
-- [1] https://en.wikipedia.org/wiki/Luhn_algorithm
-- [2] Inspired by: muhmuhten on stackoverflow
--                  https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
-------------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

-- luhn is a implementation of the luhn algorithm. [1]
luhn :: Integer -> Bool
luhn n = ((sum $ doubleDig $ reverse $ init nList) + (last nList)) `mod` 10 == 0
  where nList = toDigits n

-- roundDig takes the sum of the digits of an integer when it's higher than 9.
roundDig :: Integer -> Integer
roundDig n = if n > 9
  then sum (toDigits n)
  else n

-- doubleDig dubbles the digits of a list, applying roundDig to every doubled digit.
doubleDig :: [Integer] -> [Integer]
doubleDig n = map roundDig (zipWith (*) n (cycle [2,1]))

-- toDigits takes an integer and return a list of the digits.
toDigits :: Integer -> [Integer] --[2]
toDigits = map (read . return) . show

-------------------------------------------------------------------------------

-- isAmericanExpress checks if a credit card number is from 'American Express'.
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = (length (toDigits n) == 15 && n `div` (10 ^ 13) == 34 && luhn n)
  || (length (toDigits n) == 15 && n `div` (10 ^ 13) == 37 && luhn n)

-- isVisa checks if a credit card number is from 'Visa'.
isVisa :: Integer -> Bool
isVisa n = (length (toDigits n) == 13 && n `div` (10 ^ 12) == 4 && luhn n)
  || (length (toDigits n) == 16 && n `div` (10 ^ 15) == 4 && luhn n)
  || (length (toDigits n) == 19 && n `div` (10 ^ 18) == 4 && luhn n)

-- isMaster checks if a credit card number is from 'Master Card'.
isMaster :: Integer -> Bool
isMaster n = (length (toDigits n) == 16 && (n `div` (10 ^ 12)) `elem` [2221 .. 2720] && luhn n)
  || (length (toDigits n) == 16 && (n `div` (10 ^ 14)) `elem` [51 .. 55] && luhn n)
