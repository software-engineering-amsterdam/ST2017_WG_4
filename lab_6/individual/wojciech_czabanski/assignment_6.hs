-- Assignment: Lab6
-- Exercise: 6
-- Student: Wojciech Czabanski
-- Time needed: 25 minutes
--------------------------------------------------------------------------

module Assignment_6 where
 
import Data.List
import System.Random
import Lecture6

innerWitness :: Integer -> Integer -> Integer -> Integer -> Bool
innerWitness r x n currR | currR == r = True
innerWitness r x n currR = do
    x2 <- x * x mod n
    if x2 == 1 then False
    else if x2 == n - 1 then True
    else innerWitness r x n (currR+1)

witnessLoop :: Integer -> Integer -> IO Bool
witnessLoop n k currK | currK == k = True :: IO Bool
witnessLoop n k currK = 
    do
        a <- randomRIO (2, n-2) :: IO Integer
        r <- 0
        d <- 0 -- write n − 1 as 2r·d with d odd by factoring powers of 2 from n − 1
        x <- exM a d n 
        if x == 1 || x == n -1 then witnessLoop n k (currK+1)
        else if (innerWitness r x n 0) == False then False :: IO Bool
        else True :: IO Bool
    <- return True

millerRabinTest :: Integer -> Integer -> IO Bool
millerRabinTest n k = witnessLoop n k 0

-- WitnessLoop: repeat k times:
--    pick a random integer a in the range [2, n − 2]
--    x ← ad mod n
--    if x = 1 or x = n − 1 then
--       continue WitnessLoop
--    repeat r − 1 times:
--       x ← x2 mod n
--       if x = 1 then
--          return composite
--       if x = n − 1 then
--          continue WitnessLoop
--    return composite
-- return probably prime

