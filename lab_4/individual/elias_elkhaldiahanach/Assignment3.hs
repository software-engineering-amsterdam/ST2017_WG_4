-- Assignment: Lab4
-- Exercise: 3
-- Student: Elias el Khaldi
-- Time needed:  minutes
--------------------------------------------------------------------------

module Assignment3 where
    
import SetOrd
import Lecture4    
import Data.List
import System.Random
import Test.QuickCheck  
      
intersectionS :: Set Int -> Set Int -> Set Int
intersectionS set1 (Set lst2) = list2set (filter (\x -> inSet x set1) lst2)

differenceS :: Set Int -> Set Int -> Set Int
differenceS (Set lst1) set2 = list2set (filter (\x -> not (inSet x (intersectionS (Set lst1) set2))) lst1)

unionS :: Set Int -> Set Int -> Set Int
unionS x y = unionSet x y
