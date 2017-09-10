-- Assignment: Lab1
-- Exercise: 8
-- Authors: Quinten Heijn, Dylan Bartels, Wojciech Czabański
-- [1] Inspired by: Anne van Kesteren https://annevankesteren.nl/2007/02/haskell-xor
-------------------------------------------------------------------------------

module Lab1 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

-- A list of all the boys.
boys = [Matthew, Peter, Arnold, Carl, Jack]

-- All the staments made by the boys.
mat, peter, jack, arnold, carl :: Boy -> Bool
mat b = b /= Carl && b /= Matthew
peter b = b == Matthew || b == Jack
jack b = not (mat b) && not (peter b)
arnold b = mat b `xor` peter b
carl b = not (arnold b)

-- A list of all the staments.
testimonies :: [Boy -> Bool]
testimonies = [mat, peter, jack, arnold, carl]

-- A list possible distributions of truth.
-- Used to make all possible permutations of truth.
testimonyValidities :: [Bool]
testimonyValidities = [True, True, True, False, False]

-- invert negates a statement when it's a lie.
invert :: Bool -> Bool -> Bool
invert x y = if x == True then y else not y

-- evaluateSentences evaluates all the statements using a certain configuration
-- i.e. given that b is guilty and the staments are true according to lst:
-- do the statements form a contradiciton.
evaluateSentences :: Boy -> [Bool] -> Bool
evaluateSentences b lst = foldl (&&) True (zipWith invert lst (map ($ b) testimonies))

-- evaluateAll applies evaluateSentences to every possible configuration.
-- Trying all truth permutations for every boy.
evaluateAll :: [Boy] -> [[Bool]] -> (Boy, [Bool])
evaluateAll (x:xs) [] = evaluateAll xs $ nub $ permutations testimonyValidities
evaluateAll (x:xs) (y:ys) = if evaluateSentences x y
                            then (x, y)
                            else evaluateAll ([x]++xs) ys

-- guilty finds the boy who's guilty.
guilty :: Boy
guilty = fst (evaluateAll boys $ permutations testimonyValidities)

-- honest show which of the boys we're honest.
honest :: [Boy]
honest = map (\x -> fst x) (filter (\x -> snd x == True) (zipWith (\x y -> (x, y)) boys (snd (evaluateAll boys $ permutations testimonyValidities))))

-- xor is an implemantation of a logical xor.
xor :: Bool -> Bool -> Bool -- [1]
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False
