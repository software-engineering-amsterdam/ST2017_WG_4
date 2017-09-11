module Lab2_7 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Inspirations:
-- charToNumber: https://stackoverflow.com/questions/1706154/replacing-characters-with-numbers-in-haskell

-- IBAN validations
-- 1. Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
-- 2. Move the four initial characters to the end of the string
-- 3. Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
-- 4. Interpret the string as a decimal integer and compute the remainder of that number on division by 97
-- If the remainder is 1, the check digit test is passed and the IBAN might be valid.

charToNumber :: Char -> Int
charToNumber c = 10 + (ord c - ord 'A')

convertToNumber :: String -> String
convertToNumber str = foldr (\n acc -> acc : n) "" str

swapFour :: String -> String
swapFour str = (drop 4 str) ++ (take 4 str)

getCountryCode :: String -> String
getCountryCode str = take 2 str

iban :: String -> Bool
iban str = False

ibanPL, ibanNL, ibanUK :: String
ibanPL = "PL60102010260000042270201111"
ibanUK = "GB29RBOS60161331926819"
ibanNL = "NL39RABO0300065264"

-- TODO: implement the iban validity check
-- write test cases for valid ibans and invalid ones
-- define an automated way of testing the iban

-- Time spent: 15 minutes
