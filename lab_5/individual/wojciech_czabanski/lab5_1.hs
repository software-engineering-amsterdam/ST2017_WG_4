-- Assignment: Lab5
-- Exercise: 1
-- Student: Wojciech Czabanski
-- Time needed: 90 minutes
--------------------------------------------------------------------------

module Lab5_1

where   

import Data.List
import System.Random
import Lecture5 hiding (freeAtPos, consistent, prune)

------------------------------------------------------------
-- Assignment code
------------------------------------------------------------

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc 

subGridNrc :: Sudoku -> (Row, Column) -> [Value] 
subGridNrc s (r,c) = [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))

-- Override the `freeAtPos` function
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s r) 
      `intersect` (freeInColumn s c) 
      `intersect` (freeInSubgrid s (r,c))
      `intersect` (freeInSubgridNrc s (r,c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where 
          vs = filter (/= 0) (subGridNrc s (r,c))

-- Override the `prune` function
prune :: (Row,Column,Value)  -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
    | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
    | sameblockNrc (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
    | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y

-- Override the `consistent` function to check the constraints
consistent :: Sudoku -> Bool
consistent s = and $
          [ rowInjective s r |  r <- positions ]
          ++
          [ colInjective s c |  c <- positions ]
          ++
          [ subgridInjective s (r,c) | 
              r <- [1,4,7], c <- [1,4,7]]
          ++
          [ subgridInjectiveNrc s (r,c) |
              r <- [2,6], c <- [2,6] ]

-- A sudoku example from the Assignment
nrcSudokuExample :: Grid
nrcSudokuExample = [[0,0,0,3,0,0,0,0,0],
                    [0,0,0,7,0,0,3,0,0],
                    [2,0,0,0,0,0,0,0,8],
                    [0,0,6,0,0,5,0,0,0],
                    [0,9,1,6,0,0,0,0,0],
                    [3,0,0,0,7,1,2,0,0],
                    [0,0,0,0,0,0,0,3,1],
                    [0,8,0,0,4,0,0,0,0],
                    [0,0,2,0,0,0,0,0,0]]
                    