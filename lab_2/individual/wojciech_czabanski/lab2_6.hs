module Lab2_6 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
-- Inspirations: https://stackoverflow.com/questions/26241662/char-arithmetic-in-haskell

-- Implementing ROT13 encoding

indexA :: Int
indexA = ord 'A'

rot13 :: String -> [Char]
rot13 str = (map (\x -> chr (ord x + 13)) str):[]

-- TODO: finish the implementation
-- define a testable property
-- make it QuickCheck testable

-- Time spent: 15 minutes
