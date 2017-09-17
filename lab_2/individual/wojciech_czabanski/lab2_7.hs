module Lab2_7 where

import Data.Maybe -- for fromJust
import qualified Data.Map as Map -- for the Map data structure
import Data.Map (Map)
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Inspirations:
-- charToNumber: https://stackoverflow.com/questions/1706154/replacing-characters-with-numbers-in-haskell
-- evalMapValue: https://stackoverflow.com/questions/19960419/how-to-delete-just-in-maybe-string-or-maybe-int-in-haskell

-- For brevity the list is limited to 3 IBAN country codes and lengths.
ibanLengths :: [([Char], Int)]
ibanLengths = [("PL", 28), ("GB", 22), ("NL", 18)]

evalMapValue :: Maybe Int -> Int
evalMapValue a | a /= Nothing = fromJust a
             | otherwise = -1

lookupIbanLengthPerCountry :: String -> Int
lookupIbanLengthPerCountry x = evalMapValue (Map.lookup x (Map.fromList ibanLengths))

charToNumber :: Char -> String
charToNumber c | isDigit c = [c]
               | otherwise = show (10 + (ord c - ord 'A'))

replaceLetters :: String -> String -> String
replaceLetters [] acc = acc
replaceLetters (c:cs) acc = replaceLetters cs (acc ++ (charToNumber c))

swapFour :: String -> String
swapFour str = (drop 4 str) ++ (take 4 str)

convertToNumber :: String -> Integer
convertToNumber str = read (replaceLetters (swapFour str) [])

getCountryCode :: String -> String
getCountryCode str = take 2 str

checkIbanLength :: String -> Bool
checkIbanLength no = (length no) == (lookupIbanLengthPerCountry (getCountryCode no))

clearCheckDigits :: String -> String
clearCheckDigits iban = (getCountryCode iban) ++ "00" ++ (drop 4 iban)

padResult :: Integer -> String
padResult n | n < 10 = "0" ++ (show n)
            | otherwise = show n

generateCheckDigits :: String -> String
generateCheckDigits iban = padResult (98 - (convertToNumber (clearCheckDigits iban) `mod` 97))

checkIbanChecksum :: String -> Bool
checkIbanChecksum ibanNum = ((convertToNumber ibanNum) `mod` 97 == 1)

iban :: String -> Bool
iban str = (checkIbanLength str) && (checkIbanChecksum str)

-- Correct IBAN number Examples -> iban function yields True
correctIbanPL, correctIbanNL, correctIbanUK :: String
correctIbanPL = "PL60102010260000042270201111"
correctIbanUK = "GB29RBOS60161331926819"
correctIbanNL = "NL39RABO0300065264"

-- Incorrect IBAN number Examples -> iban function yields False
incorrectIbanPL, incorrectIbanNL, incorrectIbanUK, incorrectIbanCC :: String
incorrectIbanCC = "CC39RABO0300065264"          -- Incorrect country code -> fails the checkIbanLength property
incorrectIbanPL = "PL6010201026001112"          -- Incorrect length -> fails the checkIbanLength property
incorrectIbanUK = "GB29RBOS61161331926819"      -- Incorrect IBAN digits -> fails the checkIbanChecksum property
incorrectIbanNL = "NL40RABO0300065264"          -- Incorrect check digit -> fails the checkIbanChecksum property

-- Time spent: 90 minutes
