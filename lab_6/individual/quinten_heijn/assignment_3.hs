{--
Time spent:
Quinten:      h
--}
module Assignment_3 where

import Data.List
import System.Random
import Lecture6 hiding (composites)

composites :: [Integer]
composites = filter (not . prime) [2..]
