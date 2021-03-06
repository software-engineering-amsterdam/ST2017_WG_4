-- Assignment: Lab4
-- Exercise: 9
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi
-- Time needed: 90 minutes
--------------------------------------------------------------------------

module Assignment9 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4

-- Show functions
-------------------------------------------------------------------------------
instance Show Statement where
  show (Ass var expr)         = (show var) ++ " = " ++ (show expr)
  show (Cond condition s1 s2) = "If " ++ (show condition) ++ " {\n" ++ (show s1) ++ "\n} else {\n" ++ (show s2) ++ "\n}"
  show (Seq [])               = ""
  show (Seq xs)               = show (head xs) ++ "\n" ++ (show (Seq (tail xs)))
  show (While condition xs)   = "While " ++ (show condition) ++ " {\n" ++ (show xs) ++ "}"

instance Show Condition where
  show (Prp var)        = show var
  show (Eq expr1 expr2) = "(" ++ (show expr1) ++ " == " ++ (show expr2) ++ ")"
  show (Lt expr1 expr2) = "(" ++ (show expr1) ++ " < " ++ (show expr2) ++ ")"
  show (Gt expr1 expr2) = "(" ++ (show expr1) ++ " > " ++ (show expr2) ++ ")"
  show (Ng condition)   = "!" ++ (show condition)
  show (Cj xs)          = "(Cj " ++ show xs ++ ")"
  show (Dj xs)          = "(Dj " ++ show xs ++ ")"

instance Show Expr where
  show (I int)              = "Int " ++ (show int)
  show (V var)              = "Var " ++ (show var)
  show (Add expr1 expr2)    = "(" ++ (show expr1) ++ " + " ++ (show expr2) ++ ")"
  show (Subtr expr1 expr2)  = "(" ++ (show expr1) ++ " - " ++ (show expr2) ++ ")"
  show (Mult expr1 expr2)   = "(" ++ (show expr1) ++ " * " ++ (show expr2) ++ ")"

-- Two test functions for the new 'show' functions.
test1 :: IO ()
test1 =  putStr (show fib)

test2 :: IO ()
test2 =  putStr (show condition)

condition :: Statement
condition = Seq [Cond (Dj [Gt (V "n") (I 0), Lt (V "n") (I 0)]) (Ass "y" (I 1)) (Ass "x" (I 1))]
