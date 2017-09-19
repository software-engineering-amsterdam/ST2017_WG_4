-- Assignment: Lab3
-- Exercise: 2
-- Student: Wojciech Czabanski
-- Time needed: 
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Test.QuickCheck
import Lecture3

-- Propsed properties
-- 1. Valid characters: (), Int, +, *, -, =, >, <
-- 2. Matching brckets
-- 3. No empty brackets
-- 4. show (parse A) == A
-- 5. Random formulas

-- Incorrect input past incorrect brackets is discarded
parseMalformedBrackets :: String -> Bool
parseMalformedBrackets str = (parse str) == []
parseMalformedBracketsTest :: Bool
parseMalformedBracketsTest = parseMalformedBrackets "*(1 2"

-- Output ignored after the empty bracket
parseEmptyBrackets :: String -> Bool
parseEmptyBrackets str = False
parseEmptyBracketsTest :: Bool
parseEmptyBracketsTest = parseEmptyBrackets "+(1 2)()*(2 1)"

-- Unknown operator raises an error - needs to be caught
parseUnknownOperator :: String -> Bool
parseUnknownOperator str = False
parseUnknownOperatorTest :: Bool
parseUnknownOperatorTest = parseUnknownOperator "@(1 2)"

-- create ambiguous input examples
-- should generate multiple interpretations
parseAmbiguousInput :: String -> Bool
parseAmbiguousInput str = False
parseAmbiguousInputTest :: Bool
parseAmbiguousInputTest = parseAmbiguousInput ""

-- The textual representation of the parsed input should be 
-- exactly the same as the text input
parseCorrectInput :: String -> Bool
parseCorrectInput str = str == (show (head (parse str)))
parseCorrectInputTest :: Bool
parseCorrectInputTest = parseCorrectInput "+(1 2)"
