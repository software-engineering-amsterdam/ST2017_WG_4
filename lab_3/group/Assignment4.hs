-- Assignment: Lab3
-- Exercise: 4
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 5 hours
--------------------------------------------------------------------------

module Assignment4 where
import Data.List
import Control.Monad
import Test.QuickCheck
import Assignment3
import Lecture3

{--
testable properties are:
- equivalence between generated form and CNF form.

To run all the tests:
main
--}


-- Generate bound form:
arbitrarySizedForm :: Int -> Gen Form
arbitrarySizedForm n
  | n <= 0    = do
    t  <- arbitrary
    return (Prop t)
  | n > 0     =  oneof [liftM Neg subForm,
         liftM Cnj subFormBracets,
         liftM Dsj subFormBracets,
         liftM2 Impl subForm subForm,
         liftM2 Equiv subForm subForm]
       where subForm        = arbitrarySizedForm (n `div` 2)
             subFormBracets = listOf1 (arbitrarySizedForm (n `div` 4))

instance Arbitrary Form where
  arbitrary = sized arbitrarySizedForm
-- > generate arbitrary :: IO Form
-- > sample $ (arbitrary :: Gen Form)

-- Testable properties.
prop_checkCNF :: Form -> Bool
prop_checkCNF x = equiv x $ cnf x
-- > quickCheck (forAll (arbitrary :: Gen Form) prop_isSatisfiable)

-- Test for testable properties.
test :: IO ()
test =
 do quickCheck (withMaxSuccess 10 prop_checkCNF)
    return ()

main :: IO ()
main =
 do test
    return ()
