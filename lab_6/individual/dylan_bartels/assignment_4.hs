{--
Time spent:
Dylan:      2h

https://stackoverflow.com/questions/13337937/couldnt-match-expected-type-bool-with-actual-type-io-bool
--}
module Assignment_4 where

import Data.List
import System.Random
import Lecture6 hiding (main, composites, exM)
import Assignment_1 hiding (main)
import Assignment_3



leastComposite :: Integer -> [Integer] -> IO Integer
leastComposite n (x:xs) = do
   y <- primeTestsF n x
   if y then return x else leastComposite n xs

main :: IO ()
main = do
  let y = (map primeTestsF (fromInteger composites))
  -- id y
  -- x <- filter (== False) y
  -- let x = filter (==False) y
  return (y)

leastComposite :: Integer -> Integer
leastComposite x = if returnTrue (primeTestF
                      (fromInteger (composites !! (fromInteger x))))
                   then leastComposite (x+1)
                   else composites !! (fromInteger x)

leastComposite' :: Int -> IO Integer
leastComposite' start =
    fix $ \again -> do
        composite <- composites
        x <- (composite !! start)
        success <- primeTestF x
        if success then return x else again
