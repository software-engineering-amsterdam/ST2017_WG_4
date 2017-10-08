{--
Time spent:
Dylan:      1h
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
