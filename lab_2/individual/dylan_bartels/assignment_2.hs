import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | x + y <= z || x + z <= y || y + z <= x                   = NoTriangle
  | x == y && y == z                                         = Equilateral
  | x == y || y == z || z == x                               = Isosceles
  | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
  | otherwise                                                = Other

-- Shape indentify functions

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z
  | (x == y) && (y == z) = True
  | otherwise            = False

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z
  | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = True
  | otherwise                                                = False

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z
  | (x == y) && (x /= z) || (y == z) && (y /= x) || (z == x) && (z /= y) = True
  | otherwise                                                            = False

isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle x y z
  |  x + y < z || x + z < y || y + z < x = True
  | otherwise                            = False

-- Shape generation functions

-- > genEquilaterals 10
genEquilaterals :: Integer -> [[Integer]]
genEquilaterals n = [[x,x,x] | x <- [1..n]]

-- > genRectangulars 10
genRectangulars :: Integer -> [[Integer]]
genRectangulars n = [[x,y,z] | x <- [1..n],
                               y <- [1..n],
                               z <- [1..n],
                               (x^2) + (y^2) == (z^2)]
-- > genIsosceless 10
genIsosceles :: Integer -> [[Integer]]
genIsosceles n = [[x,x,y] | x <- [1..n],
                            y <- [1..n],
                            x /= y,
                            2 * y > x]
-- > genNoTriangles 10
genNoTriangles :: Integer -> [[Integer]]
genNoTriangles n = [[x,y,z] | x <- [0..n],
                              y <- [0..(x `div` 2)],
                              z <- [0..(x `div` 2)]]

-- Tests

testEquilateral :: Bool
testEquilateral = testHelper 100 genEquilaterals Equilateral

testRectangular :: Bool
testRectangular = testHelper 100 genRectangulars Rectangular

testIsoceles :: Bool
testIsoceles = testHelper 100 genIsosceles Isosceles

testNoTriangles :: Bool
testNoTriangles = testHelper 100 genNoTriangles NoTriangle

testHelper :: Integer -> (Integer -> [[Integer]]) -> Shape -> Bool
testHelper x y z = all (\x -> (triangle (x !! 0) (x !! 1) (x !! 2)) == z) (y x)

-- Main

mainTest :: Bool
mainTest = testEquilateral &&
           testRectangular &&
           testIsoceles &&
           testNoTriangles

-- Steps
-- Look which relations of triangle are stronger to conclude order
-- generate all shapes
-- test if shapes are indeed categorised accordingly
