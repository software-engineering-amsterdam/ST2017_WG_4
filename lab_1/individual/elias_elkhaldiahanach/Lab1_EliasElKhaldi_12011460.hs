module Lab1 where
import Data.List
import Test.QuickCheck    

{-
Name:           
	Elias el Khaldi Ahanach
Student number: 
	12011460
-}

-- Exercise 1
naturals = [1..]

-- i)
squaredsum :: Int -> Int -> Int
squaredsum n x | x > n = 0 | otherwise = x^2 + (squaredsum n (x+1))

q1 :: Int -> Float
q1 n | n < 0 = 0 | otherwise = fromIntegral (squaredsum n 1)

q2 :: Int -> Float
q2 n | n < 0 = 0 | otherwise = (fromIntegral (n * (n+1) * (2 * n +1))) / 6.0

-- (for quickCheck qTest)
qTest :: [Int] -> Bool
qTest [] = True
qTest (h:t) = ((q1 h) == (q2 h)) && qTest t

-- ii)
cubedsum :: Int -> Int -> Int
cubedsum n x | x > n = 0 | otherwise = x^3 + (cubedsum n (x+1))

z1 :: Int -> Float
z1 n | n < 0 = 0 | otherwise = fromIntegral (cubedsum n 1)

z2 :: Int -> Float
z2 n | n < 0 = 0 | otherwise = ((fromIntegral (n * (n+1))) / 2.0) ^ 2

-- (for quickCheck zTest)
zTest :: [Int] -> Bool
zTest [] = True
zTest (h:t) = ((z1 h) == (z2 h)) && zTest t

-- Exercise 2
s1 :: [Int] -> Int
s1 x = length (subsequences x)

s2 :: [Int] -> Int 
s2 x = 2 ^ (length x)

-- (for quickCheck sTest)
sTest :: [Int] -> Bool
sTest x = (s1 x) == (s2 x) 

-- Exercise 3
numberOfPermutations1 :: [Int] -> Int
numberOfPermutations1 x = length (permutations x)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

numberOfPermutations2 :: [Int] -> Int 
numberOfPermutations2 x = factorial (length x) 

-- (for quickCheck pTest)
pTest :: [Int] -> Bool
pTest x = numberOfPermutations1 x == numberOfPermutations2 x 

-- Exercise 4
divides :: Int -> Int -> Bool
divides d n = rem n d == 0

ldf :: Int -> Int -> Int
ldf k n | divides k n = k | k * 2 > n = n | otherwise = ldf (k+1) n

ld :: Int -> Int
ld n = ldf 2 n

isAPrime :: Int -> Bool
isAPrime n | n < 1 = error "not a positive integer" | n == 1 = False | otherwise = ld n == n

allPrimes :: [Int]
allPrimes = filter isAPrime naturals

reversal :: Int -> Int
reversal = read . reverse . show

isAPrimeRev :: Int -> Bool
isAPrimeRev x = isAPrime (reversal x)

findUnder10000 :: [Int] -> [Int]
findUnder10000 (h:t) | h >= 10000 = [] | otherwise = h : (findUnder10000 t)

-- findPrimeRevsU10000 can be tested by testing the individual parts this function is made up of (reversal, isAPrime, filter)
findPrimeRevsU10000 :: [Int]
findPrimeRevsU10000 = findUnder10000 (filter isAPrimeRev allPrimes)

-- Exercise 5
listSum :: [Int] -> Int
listSum [] = 0
listSum (h:t) = h + listSum t

listSumIsPrime :: [Int] -> Bool
listSumIsPrime x = isAPrime (listSum x) 

findSumPrime :: [Int] -> Int -> [Int]
findSumPrime (h:t) x | listSumIsPrime (take x (h:t)) = (take x (h:t)) | otherwise = findSumPrime t x 

find101 :: [Int]
find101 = findSumPrime allPrimes 101

-- Exercise 6
listProduct :: [Int] -> Int
listProduct [a] = a
listProduct (h:t) = h * listProduct t

findCounterExamples :: Int -> [Int] -> ([Int],Int)
findCounterExamples n (h:t) | isAPrime ((listProduct (take n (h:t))) + 1) = findCounterExamples n t | otherwise = ((take n (h:t)),listProduct (take n (h:t)))

counterExampleList :: Int -> [([Int],Int)]
counterExampleList 0 = [];
counterExampleList length = (findCounterExamples (length + 1) allPrimes) : (counterExampleList (length - 1))

-- Exercise 7
reverseToList :: Integer -> [Integer]
reverseToList 0 = []
reverseToList n | n < 0 = []
reverseToList n = lastNum n : (reverseToList (removeLast n))

lastNum :: Integer -> Integer
lastNum 0 = 0
lastNum n = mod n 10

removeLast :: Integer -> Integer
removeLast 0 = 0
removeLast n = div(n - (lastNum n)) 10

doublePer2 :: [Integer] -> [Integer]
doublePer2 [] = []
doublePer2 [h] = [h]
doublePer2 (h1:h2:t) = h1 : h2 * 2 : doublePer2 t

correctDoubles :: [Integer] -> [Integer]
correctDoubles [] = []
correctDoubles (h:t) | h > 10 = (h-9) : correctDoubles t | otherwise = h : correctDoubles t

theSum :: [Integer] -> Integer
theSum [] = 0
theSum (h:t) = h + theSum t

luhnSum :: Integer -> Integer
luhnSum x = theSum(correctDoubles(doublePer2(reverseToList x))) - (lastNum x)

luhn :: Integer -> Bool
luhn x = mod ((luhnSum x) + (mod ((luhnSum x)  * 9) 10)) 10 == 0

sharesPrefix :: Integer -> [Integer] -> Bool
sharesPrefix x []  = False
sharesPrefix x (h:t) | (take (length (show h)) (show x)) == (show h) = True | otherwise = sharesPrefix x t 

isAmericanExpress, isMaster, isVisa :: Integer -> Bool 
isAmericanExpress x = (luhn x) && ((sharesPrefix x [34]) || (sharesPrefix x [37])) 
isMaster x = (luhn x) && ((sharesPrefix x [51..55]) || (sharesPrefix x [2221..2720])) 
isVisa x = (luhn x) && (sharesPrefix x [4]) 

-- Exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]

xOr :: Bool -> Bool -> Bool
xOr a b = ((not a) && b) || (a && (not b))

accuses :: Boy -> Boy -> Bool
accuses Matthew x = x == Peter || x == Arnold || x == Jack
accuses Peter x = x == Matthew || x == Jack
accuses Jack x = (not (accuses Matthew x)) && (not (accuses Peter x))
accuses Arnold x = (accuses Matthew x) `xOr` (accuses Peter x)
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers b = filter (\x -> accuses b x) boys

contains :: Boy -> [Boy] -> Bool
contains b l = (length (filter (== b) l)) > 0 

truthSpeakers :: [Boy] -> Bool
truthSpeakers x | (contains Jack x) && ((contains Matthew x) || (contains Peter x)) = False | (contains Arnold x) && (not ((contains Matthew x) `xOr` (contains Peter x))) = False | (contains Carl x) && (contains Arnold x) = False | otherwise = True

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

findHonest :: [[Boy]] -> [[Boy]]
findHonest [] = []
findHonest (h:t) | truthSpeakers h = h : findHonest t | otherwise = findHonest t   

findGuilty :: [Boy] -> [Boy]
findGuilty [h] = accusers h
findGuilty (h:t) = (accusers h) `intersect` findGuilty t

honest | length (findHonest (subsets 3 boys)) == 1 = (findHonest (subsets 3 boys)) !! 0 
guilty = findGuilty honest


