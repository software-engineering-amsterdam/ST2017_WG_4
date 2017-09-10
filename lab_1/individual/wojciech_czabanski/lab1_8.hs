module Lab1_8 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

-- https://annevankesteren.nl/2007/02/haskell-xor

mat, peter, jack, arnold, carl :: Boy -> Bool
mat b = b /= Carl && b /= Matthew
peter b = b == Matthew || b == Jack
jack b = not (mat b) && not (peter b)
arnold b = mat b `xor` peter b
carl b = not (arnold b)

testimonies :: [Boy -> Bool]
testimonies = [mat, peter, jack, arnold, carl]

testimonyValidities :: [Bool]
testimonyValidities = [True, True, True, False, False]

invert :: Bool -> Bool -> Bool
invert x y = if x == True then y else not y

evaluateSentences :: Boy -> [Bool] -> Bool
evaluateSentences b lst = foldl (&&) True (zipWith invert lst (map ($ b) testimonies))

evaluateAll :: [Boy] -> [[Bool]] -> (Boy, [Bool])
evaluateAll (x:xs) [] = evaluateAll xs $ permutations testimonyValidities
evaluateAll (x:xs) (y:ys) = if evaluateSentences x y
                            then (x, y)
                            else evaluateAll ([x]++xs) ys

guilty :: Boy
guilty = fst (evaluateAll boys $ permutations testimonyValidities)
honest :: [Boy]
honest = map (\x -> fst x) (filter (\x -> snd x == True) (zipWith (\x y -> (x, y)) boys (snd (evaluateAll boys $ permutations testimonyValidities))))

-- Time: 3 hours
