module Lab2_6 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
-- Inspirations: https://stackoverflow.com/questions/26241662/char-arithmetic-in-haskell

-- Implementing ROT13 encoding

shift :: Char -> Char
shift a | a == ' ' = a
        | otherwise = chr ((ord 'A') + ((ord a - ord 'A') + 13) `mod` 26)

rot13 :: [Char] -> [Char]
rot13 str = (map (\x -> shift x) str)

-- Time spent: 15 minutes
