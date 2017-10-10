{--
Time spent:
Dylan:      30min
--}
module Lab5 where

import Data.List
import System.Random
import Control.Monad
import Lecture5 hiding (main)

main :: IO ()
main = do
  putStrLn $ id ("Checking if example 5 is minimal")
  putStrLn (if (minimal example5) then "True" else "False")
  putStrLn $ id ("Checking if example 6 is minimal")
  putStrLn (if (minimal example6) then "True" else "False")
  return ()

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
