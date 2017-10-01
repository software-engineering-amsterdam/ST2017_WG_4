-- Assignment: Lab4
-- Exercise: 2
-- Student: Elias el Khaldi
-- Time needed: 30 minutes
--------------------------------------------------------------------------

module Assignment2 where

import SetOrd
import Lecture4    
import Data.List
import System.Random
import Test.QuickCheck  

probs :: Int -> IO [Int]
probs 0 = return []
probs n = do
 p <- getStdRandom (randomR (-50,50))
 ps <- probs (n-1) 
 return (p:ps)

-- Creates random set of random length between 0 and 50 containing integers between -50 and 50
randomSetScratch :: IO (Set Int)
randomSetScratch = do
 n <- getStdRandom (randomR (0,50))
 lst <- probs n
 let set = list2set lst
 return set

-- Creates random set of integers using QC
randomSetQC :: IO (Set Int)
randomSetQC = do
 lst <- generate arbitrary :: IO [Int]
 let set = list2set lst
 return set