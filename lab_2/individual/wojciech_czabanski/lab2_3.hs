module Lab2_3 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Testing properties strength

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1, p2, p3, p4 :: Int -> Bool
p1 = (\x -> even x && x > 3) even
p2 = (\x -> even x || x > 3) even
p3 = (\x -> (even x && x > 3) || even x) even
p4 = even (\x -> (even x && x > 3) || even x)

-- TODO: Review the properties from the workshop
-- See if I can make the properties execute
-- Order the properties by strength on a small domain [(-10)..10]

-- Time spent: 5 minutes
