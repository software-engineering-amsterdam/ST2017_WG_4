-- Assignment: Lab2
-- Exercise: 1
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------

module Lab2 where
  
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Given func
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

testProbs :: IO (Int, Int, Int, Int)
testProbs = do
  x <- probs 10000000
  let firstInterval = length $ filter (< 0.25) x
  let secondInterval = length $ filter (\y -> (y >= 0.25) && (y < 0.5)) x
  let thirdInterval = length $ filter (\y -> (y >= 0.5) && (y < 0.75)) x
  let fourthInterval = length $ filter (>= 0.75) x
  return (firstInterval, secondInterval, thirdInterval, fourthInterval)
