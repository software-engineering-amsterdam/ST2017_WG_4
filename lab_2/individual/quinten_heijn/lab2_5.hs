-- Assignment: Lab2
-- Exercise: 5 (Implementing and testing ROT13 encoding)
-- Student: Quinten Heijn
-- Time needed: 30 min
--------------------------------------------------------------------------

module Lab2 where
import Data.List
import Test.QuickCheck
import Data.Char

rot13 :: [Char] -> [Char]
rot13 = map rot13Char

rot13Char :: Char -> Char
rot13Char c
  | c >= 'a' && c <= 'm' = chr $ (ord c) + 13
  | c >= 'n' && c <= 'z' = chr $ (ord c) - 13
  | c >= 'A' && c <= 'M' = chr $ (ord c) + 13
  | c >= 'N' && c <= 'Z' = chr $ (ord c) - 13
  | otherwise            = c
