{--
Time spent:
Dylan:      1h
--}
module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (main, freeAtPos, constraints, sameblock, extendNode,
                        initNode, prune, emptyN, eraseN, rsuccNode, rsearch,
                        rsolveNs, genRandomSudoku, genProblem)

main :: IO ()
main = do y <- genRandomSudoku
          showNode y
          s  <- genProblem y
          showNode s

-- Code from assignment 2

type Position = (Row,Column)
type Constrnt = [[Position]]

rowConstrnt      = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt   = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt    = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcBlockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks]

combinedConstrnt = rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcBlockConstrnt

freeAtPos :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos s (r,c) xs = let
   ys = filter (elem (r,c)) xs
 in
   foldl1 intersect (map ((values \\) . map s) ys)

constraints :: Sudoku -> [Constraint]
constraints s = sortBy length3rd
    [(r,c, freeAtPos s (r,c) combinedConstrnt) |
                       (r,c) <- openPositions s ]

-- Code from assignment 1

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = (bl r == bl x && bl c == bl y) ||
                        (nrcBl r == nrcBl x && nrcBl c == nrcBl y)
{--
Code from Lecture 5 not changed but can't be imported and still use the changed
freeAtPos, constraints & sameblock (How do you use them from Lecture5?)
--}

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

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in
             if (not . consistent) s then []
             else [(s, constraints s)]

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs
                        then return []
                        else return
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

rsearch :: (node -> IO [node]) -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes =
  do xs <- ionodes
     if null xs
       then return []
       else
         if goal (head xs)
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys
                      then return [head ys]
                      else if null (tail xs) then return []
                           else
                             rsearch
                               succ goal (return $ tail xs)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s)
  where s = eraseS (fst n) (r,c)

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)
