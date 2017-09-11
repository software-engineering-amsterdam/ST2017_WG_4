module Lab2_1 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Random floating point number lists
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

assignToQuartile :: Float -> [Float] -> [Float]
assignToQuartile n [a,b,c,d] = if n >= 0.0 && n < 0.25 then [a+1,b,c,d]
                     else if n >= 0.25 && n < 0.5 then [a,b+1,c,d]
                     else if n >= 0.5 && n < 0.75 then [a,b,c+1,d]
                     else if n >= 0.75 && n < 1 then [a,b,c,d+1]
                     else [a,b,c,d]

getQuartiles :: [Float] -> Int -> [Float]
getQuartiles ns n = map (\x -> x / (fromIntegral n)) ns

getQuartileCounts :: [Float] -> [Float]
getQuartileCounts ns = getQuartiles (foldr (\n acc -> assignToQuartile n acc) [0, 0, 0, 0] ns) (length ns)

epsilon :: Float
epsilon = 0.0001
test :: [Float] -> Bool
test ns = foldr (\n acc -> acc && (abs(n - 0.25)) < epsilon) True (getQuartileCounts ns)

testCurry :: Int -> Bool
testCurry n = do probList <- (probs n)
                 return (test probList)

-- TODO: get rid of the IO monad in the testCurry call

-- Time spent: 70 minutes
