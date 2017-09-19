-- Assignment: Lab3
-- Exercise: 3
-- Student: Wojciech Czabanski
-- Time needed: 10 minutes
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3


cnf :: Form -> Form
cnf = nnf . arrowfree

-- According to the lecture notes the two steps needed to convert a formula 
-- to CNF are making sure that it is arrow free and converting it to negative 
-- normal form. This means that applying both functions should result in
-- a form conforming to CNF
