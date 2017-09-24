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
          | Neg a (Form a)
          | Cnj a (Form a)
          | Dsj a (Form a)
          | Impl a (Form a) (Form a)
          | Equiv a (Form a) (Form a)
          deriving (Eq,Ord)

instance Show (Form a) where
  show (Prop x)   = show x
  show (Neg f x)    = '-' : show x
  show (Cnj x fs)     = "*(" ++ showLst [fs] ++ ")"
  show (Dsj x fs)     = "+(" ++ showLst [fs] ++ ")"
  show (Impl x f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equiv x f1 f2)  = "(" ++ show f1 ++ "<=>"
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
  oneof [liftM2 Neg arbitrary subForm,
         liftM2 Cnj arbitrary subForm,
         liftM2 Dsj arbitrary subForm,
         liftM3 Impl arbitrary subForm subForm,
         liftM3 Equiv arbitrary subForm subForm]
       where subForm = arbitrarySizedForm (n `div` 2)


instance (Arbitrary a) => Arbitrary (Form a) where
  arbitrary = sized arbitrarySizedForm
-- generate arbitrary :: IO (Form Int)
-- sample $ (arbitrary :: Gen (Form Int))



-- satisfiable :: Form a -> Bool
-- satisfiable f = any (\ v -> evl v f) (allVals f)
--
-- evl :: Valuation -> Form a -> Bool
-- evl [] (Prop c)    = error ("no info: " ++ show c)
-- evl ((i,b):xs) (Prop c)
--      | c == i    = b
--      | otherwise = evl xs (Prop c)
-- evl xs (Neg f)  = not (evl xs f)
-- evl xs (Cnj fs) = all (evl xs) fs
-- evl xs (Dsj fs) = any (evl xs) fs
-- evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
-- evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2
--
-- -- Testable Propertys
--
-- prop_isSatisfiable :: Form a -> Bool
-- prop_isSatisfiable x = satisfiable x
-- > quickCheck (forAll (arbitrary :: Gen Form) prop_isSatisfiable)
