-- Assignment: Lab4
-- Exercise: 1
-- Student: Wojciech Czabanski
-- Time needed: 120 minutes
--------------------------------------------------------------------------

module Assignment1 where
 
import Data.List
import System.Random
import Test.QuickCheck  

-- Questions:
-- 1. In example 4.6, why does F not belong to F in the example?
-- 2. Why does `halts` take 2 arguments: `funny` and `funny`? What are the types of arguments in this reasoning?
-- 3. How to show that a set containing an empty set ({0}) and a set containing a set, containing an empty set ({{0}}) are different?
-- 4. How to determine how many elements a power set has of a set with n elements?
-- 5. Would it be possible to prove that if every number in the domain of input for the program,
-- after transforming using the following formula: n1 = 3n0 + 1, converges, at some point to a power of 2
-- then the program can halt?
