-- Assignment: Lab6
-- Exercise: 2
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time spent: 4h
--------------------------------------------------------------------------
module Assignment_2 where

import Data.List
import System.Random
import Lecture6 hiding (exM)
import Assignment_1

-- See assignment_2.png for the results.

-- The test were done using ":set +s" in ghci.
-- The parameter for the function were '10 exp 111', with 'exp' being variable
-- since the exponent has the biggest impact on the execution time.

-- exM is clearly a lot faster than expM. So much so that it was difficult to test
-- the differences between them using ghci tool. Once expM was getting too slow
-- to test, exM was still running within miliseconds.

-- exM2, that does not use squiring, was still a lot faster than expM, altough being
-- of the same complexity: roughly twice as fast.
