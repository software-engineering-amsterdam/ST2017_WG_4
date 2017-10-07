module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (main)

main :: IO ()
main = do
  putStrLn $ id ("Generating random solved NRC sudoku")
  y <- genRandomSudoku
  showNode y
  putStrLn $ id ("Generating problem NRC sudoku out of solution")
  s <- genProblem y
  showNode s
