module Lab2_2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Recognizing triangles

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a == b && b == c = Equilateral
               | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == b^2 = Rectangular
               | a == b || b == c = Isosceles
               | a + b < c || a + c < b || b + c < a = NoTriangle
               | otherwise = Other

-- TODO: Create a test function

-- Time spent: 10 minutes
