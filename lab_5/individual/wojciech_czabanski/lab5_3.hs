-- Assignment: Lab5
-- Exercise: 3
-- Student: Wojciech Czabanski
-- Time needed: 15 minutes
--------------------------------------------------------------------------

module Lab5_3 where
 
import Data.List
import System.Random
import Lecture5

example1Solution :: Grid
example1Solution = [[5,3,4,6,7,8,9,1,2],
                    [6,7,2,1,9,5,3,4,8],
                    [1,9,8,3,4,2,5,6,7],
                    [8,5,9,7,6,1,4,2,3],
                    [4,2,6,8,5,3,7,9,1],
                    [7,1,3,9,2,4,8,5,6],
                    [9,6,1,5,3,7,2,8,4],
                    [2,8,7,4,1,9,6,3,5],
                    [3,4,5,2,8,6,1,7,9]]

removeOne' :: Grid -> Int -> [Grid] -> [Grid]
removeOne' g n gs | n < 81
                    = [(sud2grid (extend (grid2sud g) (((n `div` 9) + 1, (n `mod` 9) + 1), 0)))] ++ (removeOne' g (n+1) gs)
                  | otherwise = gs 

removeOne :: Grid -> [Grid]
removeOne g = filter (\x -> x /= g) (nub (removeOne' g 0 []))
