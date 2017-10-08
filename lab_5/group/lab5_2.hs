-- Assignment: Lab4
-- Exercise: 2
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time spent: 95 min
--------------------------------------------------------------------------
module Lab5_2 where
  
import Data.List
import System.Random
import Lecture5 hiding (freeAtPos, consistent, prune, succNode, solveAndShow, solveShowNs,
                        extendNode, solveNs)

type Position = (Row,Column)
type Constrnt = [[Position]]

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

rowConstrnt, columnConstrnt, blockConstrnt, blockNrcConstrnt :: Constrnt
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
blockNrcConstrnt = [[(r,c) | r <- b1, c <- b2] | b1 <- blocksNrc, b2 <- blocksNrc]

constrntDefinitions :: [Constrnt]
constrntDefinitions = [rowConstrnt, columnConstrnt, blockConstrnt, blockNrcConstrnt]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) blocksNrc

nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
nrcSubGrid s (r,c) = [ s (r',c') | r' <- nrcBl r, c' <- nrcBl c]

nrcGridInjective :: Sudoku -> (Row,Column) -> Bool
nrcGridInjective s (r,c) = injective vs where
  vs = filter (/= 0) (nrcSubGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | r <- [1,4,7], c <- [1,4,7]]
                ++
               [ nrcGridInjective s (r,c) | r <- [2,6], c <- [2,6]]

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =  foldl (\x a -> x `intersect` (freeAtPos' s (r,c) a)) positions constrntDefinitions 

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
  ys = filter (elem (r,c)) xs 
  in 
  foldl1 intersect (map ((values \\) . map s) ys)

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = (nrcBl r == nrcBl x && nrcBl c == nrcBl y)

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) =
    [(extend s ((r,c),v),
      sortBy length3rd $
          prune (r,c,v) constraints) | v <- vs ]
  
prune :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p

solveNs :: [Node] -> [Node]
solveNs = search succNode solved

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

nrcGrid :: Grid
nrcGrid =  [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

--------------------------------------------------------------------------
-- Report
--------------------------------------------------------------------------
-- Which of the two versions is easier to modify for NRC sudokus, and why?
--------------------------------------------------------------------------
-- The refactored version is easier to modify, because I only need to add the new constraint to the list
-- and update the consistency checking function through adding an new condition to the `consistent` function.
-- The latter could have been refactored as well by using an array of functions and inputs to validate
-- the consistency of a grid.
--------------------------------------------------------------------------
-- Which of the two versions is more efficient?
--------------------------------------------------------------------------
-- The first, unrefactored version is slightly more efficient. The execution times and memory usage
-- was measures by using the "+s" flag in ghci using the nrcGrid as input because the solution time
-- for the example was minimal (less than 0.03s)
-- The first solution (lab5_1): the execution times for 3 manual runs are all in range of 0.88s - 0.89s.
-- The memory usage was constant at 390MB.
-- The second solution (lab5_2): the execution times for 3 manual runs are all in range 0.93s - 0.95s.
-- The memory usage was constant at 400MB
-- Therefore, the less general first version is more effecient both in terms of time and memory usage. 
--------------------------------------------------------------------------
-- Test report
--------------------------------------------------------------------------
-- TODO:
-- 
--------------------------------------------------------------------------
