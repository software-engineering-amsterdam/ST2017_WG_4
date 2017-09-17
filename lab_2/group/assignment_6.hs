-- Assignment: Lab2
-- Exercise: 6
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
--------------------------------------------------------------------------------

module Lab2_6 where
  
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

{--
Implementing and testing ROT13 encoding

Specification
-
- Spec 2
--}

-- Simple implementation

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char ((char2int c + n) `mod` 26)
          | otherwise = c

-- Encode is set to shift 13 due to ROT13
encode :: String -> String
encode xs = [shift 13 x | x <- xs]

{--
Series of QuickCheck testable properties
- Length should be the samelength
- postencoded is different than preencoded
- sum (postencoded) - sum (preencoded) mod 13 == 0
- Length of alphabet is 26 so ROT13 is a encoding which is shifts the cipher
  half of the positions of the alphabet. That means that a double shift would
  also decode the cipher. [C1, C2, .. , C26] The testale propertie:
    - encode 13 + encode 13 == encode 0
- Frequency of letters should preencoded should be same with postencoded
  frequency
- Integer interval inbetween letters is the same preencoding as postencoding

--}

-- Test implementation

testROT13 :: String -> Bool
testROT13 xs = encode (encode xs) == xs

-- Frequency table of the alphabet
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7,
          7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- proof xs
--     = forAll (elements xs) $ \c -> encode (encode xs) == xs
--
-- main = QuickCheck proof
