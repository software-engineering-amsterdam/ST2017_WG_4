-- Assignment: Lab3
-- Exercise: 1
-- Student: Wojciech Czabanski
-- Time needed: 55 minutes
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

-- For checking the contradiction I reuse the 'satisfiable' function from the lecture module.
-- If the formula is not satisfiable then it is a contradiction.
contradiction :: Form -> Bool
contradiction f = (satisfiable f) == False

tautology :: Form -> Bool 
tautology f = all (\v -> evl v f) (allVals f)

-- For checking the logical entailment I evaluate and compare all the valuations of the forms using a logical equivalent of the implies operator
-- (p ==> q) <=> (~p || q)
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> (not (evl (fst v) f1)) || (evl (snd v) f2) ) (zipWith (\x y -> (x, y)) (allVals f1) (allVals f2)) 

-- For checking the logical equivalent I evaluate and compare all the valuations of the forms
-- and check if for every valuation of each form the values are the same
equiv :: Form -> Form -> Bool
equiv f1 f2 =  all (\v -> (evl (fst v) f1) == (evl (snd v) f2) ) (zipWith (\x y -> (x, y)) (allVals f1) (allVals f2))

-- A manual test for the defined functions.
pt, qt :: Form
pt = Prop 1
qt = Prop 2
contrTestTrue, contrTestFalse, tautologyTestTrue, tautologyTestFalse, entailsTestTrue, entailsTestFalse, equivTestTrue, equivTestFalse :: Bool
contrTestTrue = contradiction (Cnj [Neg pt, pt])                        -- It is a contradiction so the result should be True
contrTestFalse = contradiction (Dsj [Neg pt, pt])                       -- It is a tautology so the result should be False
tautologyTestTrue = tautology (Dsj [Neg pt, pt])                        -- It is a tautology so the result should be True
tautologyTestFalse = tautology (Cnj [pt, pt])                           -- It is satisfiable, but not a tautology so it should be False
entailsTestTrue = entails pt (Neg (Neg pt))                             -- The forms are logically entailed (and equivalent) so the result should be True
entailsTestFalse = entails pt (Neg pt)                                  -- The forms are no logically entailed (in fact, they contradict) so the result should be False
equivTestTrue = equiv (Neg (Cnj [pt, qt])) (Dsj [Neg pt, Neg qt])       -- The forms are logically equivalent (de Morgan's law), so the result should be True
equivTestFalse = equiv (Neg (Cnj [pt, qt])) (Dsj [pt, Neg qt])          -- The forms are not logically equivalent, so the result should be False
