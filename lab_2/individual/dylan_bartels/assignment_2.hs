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
isRectangular x y z
  | x^2 + y^2 == z^2 = True
  | x^2 + z^2 == y^2 = True
  | y^2 + z^2 == x^2 = True
  | otherwise        = False

genEquilateral :: Integer -> [[Integer]]
genEquilateral n = permutations [n, n, n]

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
 | n < 0     = []
 | otherwise = case drop (n-1) xs of
                 [ ] -> []
                 [_] -> [xs]
                 _   -> [y:c | c <- combinations (n-1) ys]
                           ++ combinations n ys
