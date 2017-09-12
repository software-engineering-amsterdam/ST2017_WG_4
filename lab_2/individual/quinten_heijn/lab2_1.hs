-- Assignment: Lab2
-- Exercise: 1
-- Student: Quinten Heijn
-- Time needed: 2 hours +
--------------------------------------------------------------------------

module Lab2 where
import Data.List
import Test.QuickCheck
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

testProbs :: Int -> IO [Float]
testProbs n =
  do
    lst <- probs n
    let p1 = (fromIntegral (length $ filter (\ x -> x >= 0.0 && x < 0.25) lst)) / (fromIntegral (length lst))
    let p2 = (fromIntegral (length $ filter (\ x -> x >= 0.25 && x < 0.5) lst)) / (fromIntegral (length lst))
    let p3 = (fromIntegral (length $ filter (\ x -> x >= 0.5 && x < 0.75) lst)) / (fromIntegral (length lst))
    let p4 = (fromIntegral (length $ filter (\ x -> x >= 0.75 && x < 1.0) lst)) / (fromIntegral (length lst))
    return [p1, p2, p3, p4]
