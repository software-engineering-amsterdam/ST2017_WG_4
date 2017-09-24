-- Assignment: Lab3
-- Exercise: 4
-- Student:
-- Time needed:
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Control.Monad
import Test.QuickCheck

type Name = Int

-- Needs to have 'a' to scale length of generated Form with input from quickcheck
data Form a = Prop Name
          | Neg (Form a)
          | Cnj (Form a)
          | Dsj (Form a)
          | Impl (Form a) (Form a)
          | Equiv (Form a) (Form a)
          deriving (Eq,Ord)

instance Show (Form a) where
  show (Prop x)   = show x
  show (Neg x)    = '-' : show x
  show (Cnj fs)     = "*(" ++ showLst [fs] ++ ")"
  show (Dsj fs)     = "+(" ++ showLst [fs] ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"

showLst,showRest :: [Form a] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

-- Generate bound form:
arbitrarySizedForm :: Arbitrary a => Int -> Gen (Form a)
arbitrarySizedForm 0       = do
  t  <- arbitrary
  return (Prop t)
arbitrarySizedForm n | n>0 =
  oneof [liftM Neg subForm,
         liftM Cnj subForm,
         liftM Dsj subForm,
         liftM2 Impl subForm subForm,
         liftM2 Equiv subForm subForm]
       where subForm = arbitrarySizedForm (n `div` 2)


instance (Arbitrary a) => Arbitrary (Form a) where
  arbitrary = sized arbitrarySizedForm
-- generate arbitrary :: IO (Form Int)
-- sample $ (arbitrary :: Gen (Form Int))
