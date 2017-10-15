-- Assignment: Lab6
-- Exercise: 7
-- Student: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 60 minutes
--------------------------------------------------------------------------

module Assignment_7 where
 
import Data.List
import System.Random
import Lecture6
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List.Split
 
-- Randomly shuffle a list
-- This method will be used for finding a pair of primes in a range
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

--

-- The findPair method looks for random pairs of primes both having n digits
-- Example output:
-- *Assignment_7> findPair 6
-- (252779,883969)

findPair :: Integer -> IO(Integer,Integer)
findPair n = do
 lst <- shuffle [(10 ^ (n-1)) .. ((10 ^ (n)) - 1)]
 a <- findPrime lst
 b <- findPrime ((splitOn [a] lst) !! 1)
 return (a,b)
 
findPrime :: [Integer] -> IO Integer
findPrime [] = error "no primes found"
findPrime (h:t) = do
 a <- primeMR 3 h
 if a
  then return h
  else (findPrime t)

-- A demonstration of how the random pair can be used for RSA encryption is given below:
-- For this example we chose to use 4-digit primes for p and q

-- *Assignment_7> originalMsg = 15102017
--
-- *Assignment_7> (p,q) <- findPair 4
-- *Assignment_7> (p,q)
-- (9323,4987)
--
-- *Assignment_7> pubK = rsaPublic p q
-- *Assignment_7> pubK
-- (5,46493801)
--
-- *Assignment_7> priK = rsaPrivate p q
-- *Assignment_7> priK
-- (18591797,46493801)
--
-- *Assignment_7> encMsg = rsaEncode pubK originalMsg
-- *Assignment_7> encMsg
-- 42448179
--
-- *Assignment_7> decMsg = rsaDecode priK encMsg
-- *Assignment_7> decMsg
-- 15102017
--
-- *Assignment_7> decMsg == originalMsg
-- True
