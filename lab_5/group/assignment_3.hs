{--
Time spent:
Quinten:    3h
Dylan:      0.5h
total:      3.5h

The minimal sudoku is example6, calculation takes a long time

--}
module Assignment_3 where

import Data.List
import System.Random
import Control.Monad
import Lecture5 hiding (main)

main :: IO ()
main = do
  putStrLn $ id ("Checking if example 6 is minimal")
  putStrLn (if (minimal example6) then "True" else "False")
  return ()

example6 :: Grid
example6 = [[0,0,0,0,0,0,0,1,0],
            [0,0,0,0,0,2,0,0,3],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,0,0,5,0,0],
            [4,0,1,6,0,0,0,0,0],
            [0,0,7,1,0,0,0,0,0],
            [0,5,0,0,0,0,2,0,0],
            [0,0,0,0,8,0,0,4,0],
            [0,3,0,9,1,0,0,0,0]]

unique :: Grid -> Bool
unique grid = (length $ solveNotShow grid) == 1

oneChange :: Grid -> [Grid]
oneChange grid = nub [replaceWithZero grid i j | i <- [0..8], j <- [0..8]]

replaceWithZero :: Grid -> Int -> Int -> Grid
replaceWithZero grid i j = take i grid ++ (replaceWithZero' (grid !! i) j) : drop (i+1) grid

replaceWithZero' :: [Value] -> Int -> [Value]
replaceWithZero' row j = (take j row) ++ 0 : (drop (j+1) row)

minimal :: Grid -> Bool
minimal grid = (unique grid) && (not $ minimal' $ oneChange grid)

minimal' :: [Grid] -> Bool
minimal' [] = True
minimal' (x:xs) =  if unique x
                    then minimal' xs
                    else False

solveNotShow :: Grid -> [Node]
solveNotShow gr = (solveNs (initNode gr))
