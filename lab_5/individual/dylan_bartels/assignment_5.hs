module Lab5 where

import Data.List
import System.Random
import Lecture5 hiding (consistent, main, freeAtPos, sameblock, extendNode,
                        prune, succNode, solveShowNs, solveNs, solveAndShow,
                        genProblem, filledPositions, minimalize, eraseN,
                        eraseS, uniqueSol, genRandomSudoku, rsolveNs, rsearch,
                        randomize, emptyN, rsuccNode, getRandomCnstr, sameLen,
                        getRandomItem, getRandomInt)

main :: IO ()
main = do
  putStrLn $ id ("Generating random solved NRC sudoku")
  y <- genRandomSudoku
  showNode y
  putStrLn $ id ("Generating problem NRC sudoku out of solution")
  s <- genProblem y
  showNode s

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

prune :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
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

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs
                  if null y
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs)
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs
                        then return []
                        else return
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node])
            -> (node -> Bool) -> IO [node] -> IO [node]
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

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s)
  where s = eraseS (fst n) (r,c)

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)
