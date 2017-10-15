-- Assignment: Lab6
-- Exercise: 3
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time spent:
-- Quinten:    2.0h
-- Dylan:      0.5h
-- Wojciech:   0.75h
-- Total:      3.0h
--------------------------------------------------------------------------
module Assignment_3 where

import Data.List
import System.Random
import Lecture6 hiding (composites)

-- Retrieve composite numbers by checking that they are not prime
composites :: [Integer]
composites = filter (not . prime) [2..]
