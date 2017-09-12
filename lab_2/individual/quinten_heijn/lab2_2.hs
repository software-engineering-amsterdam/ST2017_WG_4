-- Assignment: Lab2
-- Exercise: 2 (Recognizing triangles)
-- Student: Quinten Heijn
-- Time needed: 30 min
--------------------------------------------------------------------------

module Lab2 where
import Data.List
import Test.QuickCheck
import Data.Ord

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other
  deriving (Eq, Show)

triangleCheck :: [Integer] -> Shape
triangleCheck [a, b, c]
  | a == b && b == c && c == a = Equilateral
  | a == b || b == c || c == a = Isosceles
  | checkRectangular [a, b, c] = Rectangular
  | otherwise                  = Other

triangleCheck [_] = NoTriangle

checkRectangular :: [Integer] -> Bool
checkRectangular sides = (fromIntegral (ordSides !! 0) ** 2) + (fromIntegral (ordSides !! 1) ** 2) == (fromIntegral (ordSides !! 2) ** 2)
  where ordSides = reverse $ sortBy (comparing Down) sides
