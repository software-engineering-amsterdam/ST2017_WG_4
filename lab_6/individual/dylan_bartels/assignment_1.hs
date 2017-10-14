{--
Time spent:
Dylan:      1.5h

Source regarding squaring:
http://www.tricki.org/article/To_work_out_powers_mod_n_use_repeated_squaring
--}
module Assignment_1 where

import Data.List
import System.Random
-- import Test.QuickCheck
import Lecture6 hiding (exM)

main :: IO ()
main = do
  print $ id (expM 2 33 5)
  print $ id (exM 2 33 5)
  print $ id (expM 3 37 53)
  print $ id (exM 3 37 53)
  return ()

exM :: Integer -> Integer -> Integer -> Integer
exM x 0 z = 1
exM x y z = (appendSquares x y z) `mod` z

appendSquares :: Integer -> Integer -> Integer -> Integer
appendSquares x 1 z = x `mod` z
appendSquares x y z =
  let theGreatest = greatestSquare y
  in (x^(theGreatest) `mod` z) * (appendSquares x (y - theGreatest) z)

greatestSquare :: Integer -> Integer
greatestSquare n = last $ takeWhile (<n) [2^x | x <- [1,2..]]

-- test1 :: IO ()
-- test1 = quickCheck test1'
--
-- test1' :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
-- test1' (Positive x) (Positive y) (Positive z) = (expM x y z) == (exM x y z)
