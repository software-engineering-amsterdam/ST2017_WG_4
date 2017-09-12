import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | isEquilateral x y z = Equilateral
  | isRectangular x y z = Rectangular
  | isIsosceles x y z   = Isosceles
  | otherwise           = NoTriangle

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z = (x == y) && (y == z)

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z
  | (x == y) && (x /= z) = True
  | (y == z) && (y /= x) = True
  | (z == x) && (z /= y) = True
  | otherwise            = False

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z =
