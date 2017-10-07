module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (consistent, main, freeAtPos)

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

type NrcSquare = Int

nrcBlocks :: [[Int]]
nrcBlocks = [[1..3],[4..6],[7..9]]

-- bl :: Int -> [Int]
-- bl x = concat $ filter (elem x) blocks

nrcBlock :: Int -> [Int]
nrcBlock x = concat $ filter (elem x) blocks

-- subGrid :: Sudoku -> (Row,Column) -> [Value]
-- subGrid s (r,c) =
--   [ s (r',c') | r' <- bl r, c' <- bl c ]

nrcGrid :: Sudoku -> (Row,Column) -> [Value]
nrcGrid s (r,c) =
  [ s (r',c') | r' <- bl r, c' <- bl c]

-- freeInSeq :: [Value] -> [Value]
-- freeInSeq seq = values \\ seq

-- freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
-- freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeInNrcGrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcGrid s (r,c) = freeInSeq ()

-- nrcGridSurjective :: Sudoku -> NrcSquare -> Bool
-- nrcGridSurjective s n = injective vs where
--   vs = filter (/= 0) [ s (r,i) | i <- positions ]

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =
  (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c)
   `intersect` (freeInNrcGrid s (r,c)))
-- > freeAtPos (grid2sud nrcSudoku) (2,1)

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]
                ++ []]
              --  [ nrcSquareSurjective s (r,c) | r <- [4,] ]]

main :: IO ()
main = do
  putStrLn $ id ("Solving given NRC sudoku")
  showGrid nrcSudoku
  return ()
