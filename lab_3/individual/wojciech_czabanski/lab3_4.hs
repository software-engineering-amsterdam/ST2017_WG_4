-- Assignment: Lab3
-- Exercise: 4
-- Student: Wojciech Czabanski
-- Time needed: 25 minutes
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

-- TODO: create a form generator

-- Properties
-- go/fold recursively through the form to see if there are 
-- any Impl or Equiv forms in the 
isArrowFree :: Form -> String -- Bool
isArrowFree f = show f

-- go/fold recursively through the form to see if the
-- negations are only applied to atomic forms (Prop)
isNegationNormalForm :: Form -> String -- Bool
isNegationNormalForm = show f

-- Borrowed from Exercise 1
isLogicallyEquiv :: Form -> Form -> Bool
isLogicallyEquiv f1 f2 =  all (\v -> (evl (fst v) f1) == (evl (snd v) f2) ) (zipWith (\x y -> (x, y)) (allVals f1) (allVals f2))


-- domain: 
-- subsequences of forms
-- subsequences of operators
-- Input: number of atomic forms
-- Operators: +, *, ==>, -, <=>
-- ex: 1 form
-- p, -p
-- ex: 2 forms
-- (4) p, -p, q, -q
-- (8) p & q, q & p, p & p, q & q, -p & q, -p & -q, p & -q, -p & -q
-- +8 (+), +8 (==>), +8 (<=>)
-- total: 36

