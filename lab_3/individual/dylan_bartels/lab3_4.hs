-- Assignment: Lab3
-- Exercise: 4
-- Student:
-- Time needed:
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Control.Monad
import Test.QuickCheck
import Lecture3

-- Generate bound form:
arbitrarySizedForm :: Int -> Gen Form
arbitrarySizedForm 0       = do
  t  <- arbitrary
  return (Prop t)
arbitrarySizedForm n | n>0 =
  oneof [liftM Neg subForm,
         liftM Cnj subFormBracets,
         liftM Dsj subFormBracets,
         liftM2 Impl subForm subForm,
         liftM2 Equiv subForm subForm]
       where subForm        = arbitrarySizedForm (n `div` 2)
             subFormBracets = listOf (arbitrarySizedForm (n `div` 4))


instance Arbitrary Form where
  arbitrary = sized arbitrarySizedForm
-- generate arbitrary :: IO (Form Int)
-- sample $ (arbitrary :: Gen (Form Int))

-- Testable Propertys

prop_isSatisfiable :: Form -> Bool
prop_isSatisfiable x = satisfiable x
-- > quickCheck (forAll (arbitrary :: Gen Form) prop_isSatisfiable)

test :: IO ()
test =
 do quickCheck(prop_isSatisfiable)

     -- Other Tests


    return ()

main :: IO ()
main =
 do test
    return ()
