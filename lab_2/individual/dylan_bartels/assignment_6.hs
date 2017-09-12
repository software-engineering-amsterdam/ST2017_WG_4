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
- Length of alphabet is 26 so ROT13 is a encoding which is shifts the cipher
  half of the positions of the alphabet. That means that a double shift would
  also decode the cipher. [C1, C2, .. , C26] The testale propertie:
    - encode 13 + encode 13 == encode 0
--}

-- Test implementation

testROT13 :: String -> Bool
testROT13 xs = encode (encode xs) == xs

-- proof xs
--     = forAll (elements xs) $ \c -> encode (encode xs) == xs
--
-- main = QuickCheck proof
