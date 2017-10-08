-- Assignment: Lab5
-- Exercise: 4
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 3~4 hours
--------------------------------------------------------------------------
module Assignment_4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5

-- Since we'll be erasing blocks, we included a new type Block that contains all the positions [(Row,Column)] in a block. 
-- We also included numerous methods that work with the Block type.
-- The blocks have been labelled by indexes in the following way:
-- 1 | 2 | 3 
-- 4 | 5 | 6 
-- 7 | 8 | 9 
type Block = [(Row,Column)]

-- Returns all possible blocks 
allBlocks :: [Block]
allBlocks = map getBlock positions

-- Returns block at index i  
getBlock :: Int -> Block
getBlock i = [ (x,y) | x <- (blocks !! (quot (i-1) 3)), y <- (blocks !! (mod (i-1) 3))]

-- Returns whether a block is completely filled
isFilledBlock :: Sudoku -> Block -> Bool
isFilledBlock s [] = True
isFilledBlock s ((r,c):t) | s (r,c) == 0 = False | otherwise = True

-- Returns the filled blocks in the grid of a sudoku
filledBlocks :: Sudoku -> [Block]
filledBlocks s = filter (isFilledBlock s) allBlocks

-- Erases an entire block from the grid of sudoku
eraseBlock :: Sudoku -> Block -> Sudoku
eraseBlock s [] = s
eraseBlock s (h:t) = eraseBlock (eraseS s h) t 					 

-- The functions that were used for standard sudoku generation have been adapted to fit the needs of assignment 4. 
eraseNQ4 :: Node -> Block -> Node
eraseNQ4 n b = (s, constraints s) 
  where s = eraseBlock (fst n) b 

-- The minimalize function now includes an Int argument for the iterations (number of blocks that have to be deleted). 
minimalizeQ4 :: Node -> Int -> [Block] -> Node
minimalizeQ4 n i [] = n
minimalizeQ4 n i (h:t) | i == 0 = n | uniqueSol n' = minimalizeQ4 n' (i-1) t | otherwise  = minimalizeQ4 n i t
  where n' = eraseNQ4 n h

-- The function genProblem has been adjusted to make a minimal sudoku problem in which exactly 3 blocks are missing. 
genProblemQ4 :: Node -> IO Node
genProblemQ4 n = do 
 ys <- randomize (filledBlocks (fst n)) 
 return (minimalizeQ4 n 3 ys)

-- When you run mainQ4 it functions the same as main, only now the blocks are erased to find a sudoku problem with a unique solution. 
mainQ4 :: IO ()
mainQ4 = do 
 [r] <- rsolveNs [emptyN]
 showNode r
 s  <- genProblemQ4 r
 showNode s
 
-- An example of this is:
-- *Lab5_4> mainQ4
-- +-------+-------+-------+
-- | 4 7 5 | 1 9 3 | 6 8 2 |
-- | 1 3 8 | 6 5 2 | 4 9 7 |
-- | 9 2 6 | 7 8 4 | 1 3 5 |
-- +-------+-------+-------+
-- | 3 8 4 | 2 6 7 | 9 5 1 |
-- | 7 5 2 | 3 1 9 | 8 4 6 |
-- | 6 9 1 | 5 4 8 | 7 2 3 |
-- +-------+-------+-------+
-- | 5 4 7 | 8 2 6 | 3 1 9 |
-- | 2 6 9 | 4 3 1 | 5 7 8 |
-- | 8 1 3 | 9 7 5 | 2 6 4 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- | 4 7 5 | 1 9 3 | 6 8 2 |
-- | 1 3 8 | 6 5 2 | 4 9 7 |
-- | 9 2 6 | 7 8 4 | 1 3 5 |
-- +-------+-------+-------+
-- | 3 8 4 |       | 9 5 1 |
-- | 7 5 2 |       | 8 4 6 |
-- | 6 9 1 |       | 7 2 3 |
-- +-------+-------+-------+
-- |       | 8 2 6 |       |
-- |       | 4 3 1 |       |
-- |       | 9 7 5 |       |
-- +-------+-------+-------+
 
-- When you remove the constraint of 3 iterations, you can see that it sometimes also occurs that 4 blocks get removed:
minimalizeQ4A :: Node -> [Block] -> Node
minimalizeQ4A n [] = n
minimalizeQ4A n (h:t) |  uniqueSol n' = minimalizeQ4A n' t | otherwise  = minimalizeQ4A n t
  where n' = eraseNQ4 n h

genProblemQ4A :: Node -> IO Node
genProblemQ4A n = do 
 ys <- randomize (filledBlocks (fst n)) 
 return (minimalizeQ4A n ys)

mainQ4A :: IO ()
mainQ4A = do 
 [r] <- rsolveNs [emptyN]
 showNode r
 s  <- genProblemQ4A r
 showNode s
 
-- An example of this is:
-- *Lab5_4> mainQ4A
-- +-------+-------+-------+
-- | 9 1 6 | 8 5 4 | 7 3 2 |
-- | 3 7 5 | 9 1 2 | 8 6 4 |
-- | 8 2 4 | 3 6 7 | 1 5 9 |
-- +-------+-------+-------+
-- | 2 5 7 | 6 9 1 | 3 4 8 |
-- | 6 9 3 | 4 7 8 | 2 1 5 |
-- | 4 8 1 | 5 2 3 | 9 7 6 |
-- +-------+-------+-------+
-- | 5 3 2 | 7 4 9 | 6 8 1 |
-- | 1 6 8 | 2 3 5 | 4 9 7 |
-- | 7 4 9 | 1 8 6 | 5 2 3 |
-- +-------+-------+-------+
-- +-------+-------+-------+
-- | 9 1 6 |       | 7 3 2 |
-- | 3 7 5 |       | 8 6 4 |
-- | 8 2 4 |       | 1 5 9 |
-- +-------+-------+-------+
-- |       | 6 9 1 |       |
-- |       | 4 7 8 |       |
-- |       | 5 2 3 |       |
-- +-------+-------+-------+
-- | 5 3 2 |       | 6 8 1 |
-- | 1 6 8 |       | 4 9 7 |
-- | 7 4 9 |       | 5 2 3 |
-- +-------+-------+-------+

-- However, it is not possible to have 5 blocks removed. A check that can show this can be made using the functions below. 
-- I have reprogrammed the functions to go through all branches of the tree if necessary to find a sudoku that misses exactly i blocks.
minimalizeQ4B :: Node -> Int -> [Block] -> Bool
minimalizeQ4B n 0 l = True
minimalizeQ4B n i [] =  False
minimalizeQ4B n i (h:t) | (uniqueSol n') && (minimalizeQ4B n' (i-1) t) = True | otherwise = minimalizeQ4B n i t
  where n' = eraseNQ4 n h

genProblemQ4B :: Node -> Int -> IO Bool
genProblemQ4B n i  = do 
 ys <- randomize (filledBlocks (fst n)) 
 return (minimalizeQ4B n i ys)

mainQ4B :: Int -> IO Bool
mainQ4B i = do 
 [r] <- rsolveNs [emptyN]
 genProblemQ4B r i
 