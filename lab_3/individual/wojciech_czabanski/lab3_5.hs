-- Assignment: Lab3
-- Exercise: 5
-- Student: Wojciech Czabanski
-- Time needed: 70 minutes
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

cnf2clsDsj :: Form -> Clause
cnf2clsDsj (Prop x) = [read (show x)]
cnf2clsDsj (Neg x) = [-read (show x)]
cnf2clsDsj (Dsj fs) = map (\x -> read (show x)) fs 

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[read (show x)]]
cnf2cls (Neg x) = [[-read (show x)]]
cnf2cls (Dsj fs) = [map (\x -> read (show x)) fs]
cnf2cls (Cnj fs) = map (\x -> cnf2clsDsj x) fs

-- Test Cases
pt = Prop 1
qt = Prop 2
rt = Prop 3

-- Expected output: [[1]]
testProp :: Form
testProp = pt

-- Expected output: [[-1]]
testNegProp :: Form
testNegProp = Neg pt

-- Expected output: [[1,2,3]]
testDsj :: Form
testDsj = Dsj [pt, Neg qt, rt]

-- Expected output: [[1], [2], [3]]
testCnj :: Form
testCnj = Cnj [pt, qt, rt]

-- Expected output: [[1], [1, -2]]
testComplex :: Form
testComplex = Cnj [pt, Dsj [pt, Neg qt]]

-- Expected output: [[1], [1, 2], [-3], [-3, 2]]
testAll :: Form
testAll = Cnj [pt, Dsj [pt, qt], Neg rt, Dsj [Neg rt,qt]]

