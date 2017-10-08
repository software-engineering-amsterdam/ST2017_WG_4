{--
Time spent:
Dylan:      1h
wojciech    1h
quinten     1.6h

Report at bottom of file

--}
module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (main, freeAtPos, constraints)

main :: IO ()
main = do y <- genRandomSudoku
          showNode y
          s  <- genProblem y
          showNode s

-- Code from assignment

type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt      = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt   = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt    = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

combinedConstrnt = rowConstrnt ++ columnConstrnt ++ blockConstrnt

freeAtPos :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos s (r,c) xs = let
   ys = filter (elem (r,c)) xs
 in
   foldl1 intersect (map ((values \\) . map s) ys)
-- > freeAtPos (grid2sud example1) (2,1) combinedConstrnt

-- Effect on Lecture 5 functions

constraints :: Sudoku -> [Constraint]
constraints s = sortBy length3rd
    [(r,c, freeAtPos s (r,c) combinedConstrnt) |
                       (r,c) <- openPositions s ]
-- > constraints (grid2sud example1)

{--
Report
------------------------------------------------------------------------
Which of the two versions is easier to modify for NRC sudokus, and why?
------------------------------------------------------------------------
The refactored version is easier to modify, because I only need to add the new constraint to the list
and update the consistency checking function through adding an new condition to the `consistent` function.
The latter could have been refactored as well by using an array of functions and inputs to validate
the consistency of a grid.
------------------------------------------------------------------------
Which of the two versions is more efficient?
------------------------------------------------------------------------
The first, unrefactored version is slightly more efficient. The execution times and memory usage
was measures by using the "+s" flag in ghci using the nrcGrid as input because the solution time
for the example was minimal (less than 0.03s)
The first solution (lab5_1): the execution times for 3 manual runs are all in range of 0.88s - 0.89s.
The memory usage was constant at 390MB.
The second solution (lab5_2): the execution times for 3 manual runs are all in range 0.93s - 0.95s.
The memory usage was constant at 400MB
Therefore, the less general first version is more effecient both in terms of time and memory usage.
------------------------------------------------------------------------
Test report
------------------------------------------------------------------------
TODO:

------------------------------------------------------------------------
--}
