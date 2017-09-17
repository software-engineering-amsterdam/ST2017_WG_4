-- Assignment: Lab2
-- Exercise: 2
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------

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

test :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
test a b c = False

-- TODO: Create a test function
-- Create generators for each type of triangle
-- For a [[Int, Int, Int]]

-- There is not posibility of Rectangular triangle
-- Overleaf

-- Time spent: 10 minutes
