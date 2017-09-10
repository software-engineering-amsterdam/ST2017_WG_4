module Lab1_7 where
import Data.List
import Test.QuickCheck
import Data.Digits

interleave :: ([Int], [Int]) -> [Int]
interleave (a, b) = concat (zipWith (\x y -> [x]++[y]) a b)
-- https://stackoverflow.com/questions/22702064/how-to-interleave-two-lists-in-haskell-in-one-line-with-higher-order-functions

splitListsIntoTwo :: [Int] -> ([Int], [Int])
splitListsIntoTwo xs = (odds xs, evens xs)
odds (x:xs) = x : evens xs
odds xs = []
evens xs = odds (drop 1 xs)
-- https://stackoverflow.com/questions/7410989/haskell-splitting-list-into-tuple-of-two-new-lists

transformDigits :: [Int] -> [Int]
transformDigits ns = map (\x -> if x > 9 then x - 9 else x) (map (\x -> x*2 ) ns)

transformList :: [Int] -> [Int]
transformList xs = interleave (fst (splitListsIntoTwo xs), transformDigits (snd (splitListsIntoTwo xs)))

computeCheckDigit :: [Int] -> Int
computeCheckDigit ns = sum ns * 9 `mod` 10

validityCheck :: [Int] -> Bool
validityCheck ns = sum ns `mod` 10 == 0

luhn :: Int -> Bool
luhn n = validityCheck (transformList (init (digits 10 n)) ++ [last (digits 10 n)])

-- Test data
-- 79927398713
-- [7,9,9,4,7,6,9,7,7,2]

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = True
isMaster n = True
isVisa n = True

-- using library Digits

-- Time: 2.5 hours
