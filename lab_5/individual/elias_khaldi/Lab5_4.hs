-- Assignment: Lab5
-- Exercise: 4
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 
--------------------------------------------------------------------------
module Lab5_4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5

-- Since we'll be erasing Blocks, I have included a new type Block that contains all the positions (Row,Column) in a block. 
-- I included methods to find blocks based on index (1 to 9) and to get all the isFilledBlock in a Sudoku grid. 

type Block = [(Row,Column)]

allBlocks :: [Block]
allBlocks = map getBlock positions

getBlock :: Int -> Block
getBlock i = [ (x,y) | x <- (blocks !! (quot (i-1) 3)), y <- (blocks !! (mod (i-1) 3))]

isFilledBlock :: Sudoku -> Block -> Bool
isFilledBlock s [] = True
isFilledBlock s ((r,c):t) | s (r,c) == 0 = False | otherwise = True

filledBlocks :: Sudoku -> [Block]
filledBlocks s = filter (isFilledBlock s) allBlocks

eraseBlock :: Sudoku -> Block -> Sudoku
eraseBlock s [] = s
eraseBlock s (h:t) = eraseBlock (eraseS s h) t 					 

eraseNQ4 :: Node -> Block -> Node
eraseNQ4 n b = (s, constraints s) 
  where s = eraseBlock (fst n) b 

minimalizeQ4 :: Node -> [Block] -> Node
minimalizeQ4 n [] = n
minimalizeQ4 n (h:t) | uniqueSol n' = minimalizeQ4 n' t | otherwise  = minimalizeQ4 n t
  where n' = eraseNQ4 n h

genProblemQ4 :: Node -> IO Node
genProblemQ4 n = do 
 ys <- randomize (filledBlocks (fst n)) 
 return (minimalizeQ4 n ys)
 
-- When you run mainQ4 it functions the same as main, only now the blocks are erased to find a sudoku problem with a unique solution. 
 
mainQ4 :: IO ()
mainQ4 = do 
 [r] <- rsolveNs [emptyN]
 showNode r
 s  <- genProblemQ4 r
 showNode s