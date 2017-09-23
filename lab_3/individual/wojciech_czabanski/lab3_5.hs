-- Assignment: Lab3
-- Exercise: 5
-- Student: Wojciech Czabanski
-- Time needed: 90 minutes
-- Inspirations: https://stackoverflow.com/questions/6187679/how-to-create-haskell-permutation
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Data.Bits
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

-- Clause evaluations
encode :: Int -> Int -> [Bool]
encode bitSize value = map (testBit value) [0..bitSize-1]

genSubstitutions :: Int -> [[Bool]]
genSubstitutions n = map (encode n) [0..2^n-1]

flatten :: Clauses -> [Int]
flatten [] = []
flatten c = head c ++ (flatten (tail c))

numFormsInClauses :: Clauses -> Int
numFormsInClauses c = length (nub (map (\x -> if x < 0 then -x else x) (flatten c)))

evalClause :: Clause -> [Bool] -> Bool
evalClause c bs = foldr (\x acc -> acc || (bs !! (x-1))) False c

evalClauses :: Clauses -> [Bool] -> Bool
evalClauses c bs =  foldr (\x acc -> acc && (evalClause x bs)) True c

toValuation :: [Bool] -> Valuation
toValuation bs = zipWith (\x y -> (x,y)) [1..((length bs)+1)] bs

evalForm :: Form -> [Bool] -> Bool
evalForm f bs = evl (toValuation bs) f

compareFormWithClauses :: Form -> Clauses -> [[Bool]] -> Bool
compareFormWithClauses f c subst = foldl (&&) True (map (\x -> (evalForm f x) == (evalClauses c x)) subst)

checkEquivalence :: Form -> Clauses -> Bool
checkEquivalence f c = compareFormWithClauses f c (genSubstitutions (numFormsInClauses c))

-- Testing process
-- 1. The input can be generated by the formula generator from
-- the previous exercise: [Form]
-- 2. The output would be defined as follows: cnf2cls.cnf 
-- 3. The proposed test properties:
--   a) logical equivalence of the cnf formula and the clause -> 
--    in this case a way to evaluate the clauses would be needed
-- 4. Test usage
-- testEquiv :: Form -> Bool
-- testEquiv f = (compareFormWithClauses f (cnf2cls f))
