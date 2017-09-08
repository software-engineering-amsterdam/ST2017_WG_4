import Test.QuickCheck

-- Assignment 8.

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew Peter  = True
accuses Matthew Jack   = True
accuses Matthew Arnold = True
accuses Peter Matthew  = True
accuses Peter Jack     = True
accuses Jack Matthew   = True
accuses Jack Carl      = True
accuses Arnold Carl    = True
accuses Arnold Matthew = True
accuses Arnold Jack    = True
accuses Carl Arnold    = True
accuses Carl Peter     = True
accuses _ _            = False

accusers :: Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Peter   = [Matthew, Carl]
accusers Jack    = [Matthew, Peter, Arnold]
accusers Arnold  = [Matthew, Carl]
accusers Carl    = [Jack, Arnold]

-- guilty :: [Boy]
-- guilty = permutations [True, True, True, False, False] -- Has duplicates
