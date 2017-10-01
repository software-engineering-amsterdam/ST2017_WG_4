-- Assignment: Lab4
-- Exercise: 8
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi
-- Time needed: 30 minutes
--------------------------------------------------------------------------
-- Is there a difference between the symmetric closure of the transitive closure of a relation RR and the transitive closure of the symmetric closure of RR?
-- Deliverable: If your answer is that these are the same, you should give an argument, if you think these are different you should give an example that illustrates the difference.
--------------------------------------------------------------------------

module Lab4_8 where
    
import Data.List
import System.Random
import Test.QuickCheck  
import Lab4_5
import Lab4_6

-- They are not the same! Take counterexample: 
-- x = [(1,2)]
-- trClos (symClos x) = [(1,1),(1,2),(2,1),(2,2)]
-- symClos (trClos x) = [(1,2),(2,1)]

testedRel :: Rel Int
testedRel = [(1,2)]

cntrExample :: Bool
cntrExample = (trClos (symClos testedRel)) == (symClos (trClos testedRel))
