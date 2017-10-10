{--
Time spent:
Dylan:      3h
--}
module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (consistent, main, freeAtPos, sameblock, extendNode,
                        prune, succNode, solveShowNs, solveNs, solveAndShow)

main :: IO ()
main = do
  putStrLn $ id ("Solving given NRC sudoku")
  showGrid nrcGrid
  putStrLn $ id ("Solution")
  solveAndShow nrcGrid
  return ()

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

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks

nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
nrcSubGrid s (r,c) = [ s (r',c') | r' <- nrcBl r, c' <- nrcBl c]

freeInNrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcSubGrid s (r,c) = freeInSeq (freeInNrcSubGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) =
  (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInNrcSubGrid s (r,c))

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

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = (bl r == bl x && bl c == bl y) ||
                        (nrcBl r == nrcBl x && nrcBl c == nrcBl y)

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune (r,c,v) constraints) | v <- vs ]

prune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
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
