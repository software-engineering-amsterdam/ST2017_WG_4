-- Assignment: Lab3
-- Exercise: 5
-- Student: Wojciech Czabanski
-- Time needed: 
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

-- CNF: (p1 | p2) ^ (p2 | p3) ^ (p1 | p3) ...

pt = Prop 1
qt = Prop 2
testForm :: Form
testForm = (Cnj [pt, Dsj [pt, Neg qt]]) 

-- expected output: [[1], [1, -2]]

cnf2cls :: Form -> Clauses
cnf2cls f = []
