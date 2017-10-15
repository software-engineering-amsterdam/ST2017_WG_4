{--
Time spent:
30 min
--}
module Assignment_3 where

import Data.List
import System.Random
import Lecture6 hiding (composites)

-- sieve
composites :: [Integer]
composites = filter (not . prime) [2..]
