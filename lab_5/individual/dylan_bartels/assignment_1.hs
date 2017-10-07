module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (consistent, main, freeAtPos, sameblock)

main :: IO ()
main = do
  putStrLn $ id ("Solving given NRC sudoku")
  showGrid nrcSudoku
  putStrLn $ id ("Solution")
  solveAndShow nrcSudoku
  return ()

nrcSudoku :: Grid
nrcSudoku = [[0,0,0,4,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [0,0,3,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks

nrcGrid :: Sudoku -> (Row,Column) -> [Value]
nrcGrid s (r,c) =
  [ s (r',c') | r' <- nrcBl r, c' <- nrcBl c]

freeInNrcGrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcGrid s (r,c) = freeInSeq (nrcGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =
  (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c)
   `intersect` (freeInNrcGrid s (r,c)))

nrcGridSurjective :: Sudoku -> (Row,Column) -> Bool
nrcGridSurjective s (r,c) = injective vs where
  vs = filter (/= 0) (nrcGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]]
                ++
               [ nrcGridSurjective s (r,c) | r <- [2,6], c <- [2,6]]

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = (bl r == bl x && bl c == bl y) ||
                        (nrcBl r == nrcBl x && nrcBl c == nrcBl y)
