{--
Time spent:
Dylan:      1.5h

Source regarding squaring:
http://www.tricki.org/article/To_work_out_powers_mod_n_use_repeated_squaring
--}
module Assignment_1 where

import Data.List
import System.Random
import Lecture6 hiding (exM)

main :: IO ()
main = do
  print $ id (expM 2 33 5)
  print $ id (exM 2 33 5)
  print $ id (expM 3 37 53)
  print $ id (exM 3 37 53)
  return ()

exM :: Integer -> Integer -> Integer -> Integer
exM x y z = (appendSquares x y z) `mod` z

appendSquares :: Integer -> Integer -> Integer -> Integer
appendSquares x 1 z = x `mod` z
appendSquares x y z = let
  greatestSquare' = greatestSquare y
  in (x^(greatestSquare') `mod` z) * (appendSquares x (y - greatestSquare') z)

greatestSquare :: Integer -> Integer
greatestSquare n = last $ takeWhile (<n) [2^x | x <- [1,2..]]
