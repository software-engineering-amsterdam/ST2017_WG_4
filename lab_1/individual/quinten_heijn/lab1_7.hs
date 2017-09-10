-- Assignment: Lab1
-- Exercise: 7
-- Student: Quinten Heijn
-- Time needed: 70 minutes
-- [1] Inspired by: muhmuhten on stackoverflow
--                  https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
-------------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck


luhn :: Integer -> Bool
luhn n = ((sum $ doubleDig $ reverse $ init nList) + (last nList)) `mod` 10 == 0
  where nList = toDigits n

roundDig :: Integer -> Integer
roundDig n = if n > 9
  then sum (toDigits n)
  else n

doubleDig :: [Integer] -> [Integer]
doubleDig n = map roundDig (zipWith (*) n (cycle [2,1]))

toDigits :: Integer -> [Integer] --[1]
toDigits = map (read . return) . show

-------------------------------------------------------------------------------

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = (length (toDigits n) == 15 && n `div` (10 ^ 13) == 34 && luhn n)
  || (length (toDigits n) == 15 && n `div` (10 ^ 13) == 37 && luhn n)

isVisa :: Integer -> Bool
isVisa n = (length (toDigits n) == 13 && n `div` (10 ^ 12) == 4 && luhn n)
  || (length (toDigits n) == 16 && n `div` (10 ^ 15) == 4 && luhn n)
  || (length (toDigits n) == 19 && n `div` (10 ^ 18) == 4 && luhn n)

isMaster :: Integer -> Bool
isMaster n = (length (toDigits n) == 16 && (n `div` (10 ^ 12)) `elem` [2221 .. 2720] && luhn n)
  || (length (toDigits n) == 16 && (n `div` (10 ^ 14)) `elem` [51 .. 55] && luhn n)
