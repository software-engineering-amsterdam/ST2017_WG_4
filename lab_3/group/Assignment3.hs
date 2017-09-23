-- Assignment: Lab3
-- Exercise: 3
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 3~4h
--------------------------------------------------------------------------

module Assignment3 where
import Data.List
import Test.QuickCheck
import Lecture3

cnf :: Form -> Form
cnf x = convertCNF (nnf (arrowfree (x)))

-- Example:
-- Lab3> cnf (head (parse "- +(*(1 2) *(3 4))"))
-- *(+(-1 -2) +(-3 -4))

convertCNF :: Form -> Form
convertCNF (Cnj (h:t)) = formatF (Cnj (map convertCNF (h:t)))
convertCNF (Dsj [x]) = convertCNF x
convertCNF (Dsj (h:t)) = formatF (distr (convertCNF h) (convertCNF (Dsj t))) 
convertCNF x = x

distr :: Form -> Form -> Form
distr (Cnj [h]) y = Dsj [h,y]
distr (Cnj (h:t)) y = Cnj [(distr h y),(distr (Cnj t) y)]
distr x (Cnj [h]) = Dsj [x,h]
distr x (Cnj (h:t)) = Cnj [(distr x h),(distr x (Cnj t))]
distr x y = Dsj [x,y]

-- The formatting functions help replace cases like 
-- *(a *(b *(c d))) to
-- *(a b c d)
-- Even though both are correct, the second is easier to interpret

formatC :: Form -> [Form]
formatC (Cnj []) = []
formatC (Cnj (h:t)) = (formatC h) ++ (formatC (Cnj t))
formatC x = [x]

formatD :: Form -> [Form]
formatD (Dsj []) = []
formatD (Dsj (h:t)) = ((formatD h) ++ (formatD (Dsj t)))
formatD x = [x]

formatF :: Form -> Form
formatF (Cnj lst) = Cnj (formatC (Cnj lst))
formatF (Dsj lst) = Dsj (formatD (Dsj lst))
formatF x = x

-- Can be used to check whether the cnf output actually is equivalent to its input
checkCNF :: Form -> Bool
checkCNF f = equiv f (cnf f)

-- Code for equivalence function (Assignment 1)
equiv :: Form -> Form -> Bool
equiv f1 f2 =  all (\v -> (evl (fst v) f1) == (evl (snd v) f2) ) (zipWith (\x y -> (x, y)) (allVals f1) (allVals f2))





