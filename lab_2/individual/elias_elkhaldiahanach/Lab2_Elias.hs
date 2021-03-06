module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Exercise 1
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
 p <- getStdRandom random
 ps <- probs (n-1) 
 return (p:ps)

checkIntervals :: [Float] -> [Int]
checkIntervals list = countInInterval list 0.0 0.25 : countInInterval list 0.25 0.5 : countInInterval list 0.5 0.75 : countInInterval list 0.75 1.0 : [] 

countInInterval ::  [Float] -> Float -> Float -> Int
countInInterval [] a b = 0
countInInterval (h:t) a b | (h >= a) && (h < b) = 1 + countInInterval t a b | otherwise = countInInterval t a b

calculateMean :: [Int] -> Float
calculateMean l = fromIntegral (sum l) / (fromIntegral (length l))

-- test using 1% error allowance (1% of n=1000 = 10)
zTest :: [Int] -> Bool
zTest l = calculateMean l < (250 + 10) && calculateMean l > (250 - 10)

test :: Int -> IO Bool
test a = do 
 x = probs a 
 return True

--Exercise 5
--Implementation rot13 
rot13 :: String -> String
rot13 [] = []
rot13 (h:t) = (shift h 13) : rot13 t

shift :: Char -> Int -> Char
shift c n | c <= 'z' && c >= 'a' = chr ((ord 'a') + (mod ((ord c) - (ord 'a') + n) 26)) | c <= 'Z' && c >= 'A' = chr ((ord 'A') + (mod ((ord c) - (ord 'A') + n) 26)) | otherwise = error "no letters" 

distance :: Char -> Char -> Int
distance c1 c2 | c1 >= c2 = (ord c1) - (ord c2) | otherwise = 26 - (distance c2 c1)

--test for interval 
intervalArray :: String -> [Int]
intervalArray [] = []
intervalArray [a] = []
intervalArray (h1:h2:t) = (distance h1 h2) : intervalArray (h2:t)

intervalTest :: String -> Bool
intervalTest s = (intervalArray s) == (intervalArray (rot13 s))

--test for difference
differenceTest :: String -> Bool
differenceTest s | length s > 0 = not (rot13 s == s) | otherwise = True

--test for symmetry
symmetryTest :: String -> Bool
symmetryTest s = rot13 (rot13 s) == s

--test for frequency
alphabet = ['a'..'z']

freqArray :: String -> String -> [Int]
freqArray s [] = []
freqArray s (h:t) = (findChar h s) : freqArray s t

freqAlphabet :: String -> [Int]
freqAlphabet s = freqArray s alphabet

findChar :: Char -> String -> Int
findChar c [] = 0
findChar c (h:t) | toLower h == c = 1 + findChar c t | otherwise = findChar c t

frequencyTest :: String -> Bool
frequencyTest s = sort(filter (>0) (freqAlphabet s)) == sort(filter (>0) (freqAlphabet (rot13 s)))

-- Filters the letters out the String for use in quickCheck
filtLett :: String -> String
filtLett s = filter (\x -> x >= 'a' && x <= 'z' && x >= 'A' && x <= 'Z') s

--test using quickcheck : quickCheck (\x -> frequencyTest (filtLett x) && symmetryTest (filtLett x) && differenceTest (filtLett x) && intervalTest (filtLett x))