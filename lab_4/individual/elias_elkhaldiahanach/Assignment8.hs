-- Assignment: Lab4
-- Exercise: 8
-- Student: Elias el Khaldi
-- Time needed: 30 minutes
--------------------------------------------------------------------------

module Assignment8 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import Assignment5
import Assignment6

-- They are not the same! Take counterexample: 
-- x = [(1,2)]
-- trClos (symClos x) = [(1,1),(1,2),(2,1),(2,2)]
-- symClos (trClos x) = [(1,2),(2,1)]

cntrExample = (trClos (symClos [(1,2)])) == (symClos (trClos [(1,2)]))
