-- Assignment: Lab3
-- Exercise: 1
-- Student: Wojciech Czabanski
-- Time needed: 40 minutes
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction f = (satisfiable f) == False

tautology :: Form -> Bool 
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> (not (evl (fst v) f1)) || (evl (snd v) f2) ) (zipWith (\x y -> (x, y)) (allVals f1) (allVals f2)) 

equiv :: Form -> Form -> Bool
equiv f1 f2 =  all (\v -> (evl (fst v) f1) == (evl (snd v) f2) ) (zipWith (\x y -> (x, y)) (allVals f1) (allVals f2))

pt = Prop 1
qt = Prop 2
contrTest, tautologyTest, entailsTest1, entailsTest2, equivTest1, equivTest2 :: Form
contrTest = Cnj [Neg pt, pt]
tautologyTest = Dsj [Neg pt, pt]
equivTest = Equiv pt (Neg (Neg pt))
entailsTest1 = pt
entailsTest2 = Neg (Neg pt)
equivTest1 = Neg (Cnj [pt, qt])
equivTest2 = Dsj [Neg pt, Neg qt]
