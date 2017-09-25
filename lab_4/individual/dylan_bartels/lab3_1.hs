-- Assignment: Lab3
-- Exercise: 1
-- Student:
-- Time needed:
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction f = ()

-- tautology :: Form -> Bool
-- tautology

nnf :: Form -> Form
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)
