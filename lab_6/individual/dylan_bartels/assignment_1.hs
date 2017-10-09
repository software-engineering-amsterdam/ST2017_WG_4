{--
Time spent:
Dylan:      1.5h
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
exM x 0 n = 1 `mod` n
exM x 1 n = x `mod` n
exM x y n = let
   greatestSquare = last $ takeWhile (<y) [2^x | x <- [1,2..]]
   in ((x^greatestSquare `mod` n) * (x^(y - greatestSquare) `mod` n)) `mod` n

-- > exM 3 37 53
-- Now only works for 1 square, what about more square's?
-- http://www.tricki.org/article/To_work_out_powers_mod_n_use_repeated_squaring
