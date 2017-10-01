-- Assignment: Lab3
-- Exercise: 1
-- Student: 
-- Time needed: 
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

-- Test whether a form has the operators allowed for CNF. Also checks whether the only Conjunction is the one in the beginning ("*(...)")
testOperators :: Form -> Bool
testOperators (Cnj []) = True
testOperators (Cnj (h:t)) = testOperators2 h && testOperators2 (Cnj t) 
testOperators x = testOperators2 x

-- Tests whether the inside ("*(..inside..)") of the formula does not contain forbidden operators
testOperators2 :: Form -> Bool
testOperators2 (Cnj x) = False
testOperators2 (Dsj []) = True
testOperators2 (Dsj (h:t)) = testOperators2 h && testOperators2 (Dsj t) 
testOperators2 (Neg (Prop x)) = True
testOperators2 (Neg (x)) = False
testOperators2 (Prop x) = True
testOperators2 (Impl x y) = False
testOperators2 (Equiv x y) = False
