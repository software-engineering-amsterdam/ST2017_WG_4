-- Assignment: Lab5
-- Exercise: 2
-- Student: Wojciech Czabanski
-- Time needed: 50 minutes
--------------------------------------------------------------------------

module Lab5_2 where
 
import Data.List
import System.Random
import Lecture5 hiding (freeAtPos)

type Position = (Row,Column)
type Constrnt = [[Position]]

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

nrcGrid :: Grid
nrcGrid = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [2,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

rowConstrnt, columnConstrnt, blockConstrnt, blockNrcConstrnt :: Constrnt
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
blockNrcConstrnt = [[(r,c) | r <- b1, c <- b2] | b1 <- blocksNrc, b2 <- blocksNrc]

constrntDefinitions :: [Constrnt]
constrntDefinitions = [rowConstrnt, columnConstrnt, blockConstrnt, blockNrcConstrnt]
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =  foldl (\x a -> x `intersect` (freeAtPos' s (r,c) a)) positions constrntDefinitions 

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)

-- The refactored version is easier to modify, because I only need to add the constraint list
-- whereas the algorithm remains the same, as opposed to changing the algorithm (`consistent` function)
-- in the former case

-- use haskell profiler prof for memory & cpu time
