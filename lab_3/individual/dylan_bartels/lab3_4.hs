-- Assignment: Lab3
-- Exercise: 4
-- Student:
-- Time needed:
--------------------------------------------------------------------------

module Lab3 where
import Data.List
import Control.Monad
import Test.QuickCheck

data Point = Pt Int Int

instance Show (Point) where
  show (Pt x y) = "{" ++ show x ++ "," ++ show y ++ "}"

instance Arbitrary Point where
   arbitrary = do
     x <- arbitrary
     y <- arbitrary
     return $ Pt x y
-- > sample $ (arbitrary :: Gen Point)

data Set a = Set [a]

instance (Show a) => Show (Set a) where
    show s = showSet s where
         showSet (Set []) = "{}"
         showSet (Set (x:xs)) = "{" ++ show x ++ showSubSet xs ++ "}" where
              showSubSet [] = ""
              showSubSet (x:xs) = "," ++ show x ++ showSubSet xs

instance (Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
                list <- arbitrary
                return $ Set list
-- > sample $ (arbitrary :: Gen (Set Int))
-- > sample $ (arbitrary :: Gen (Set Point))

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form

instance Show Form where
  show (Prop x)       = show x
  show (Neg f)        = '-' : show f
  show (Cnj fs)       = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)       = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)   = "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

-- Generate unbounded forms:
instance Arbitrary Form where
   arbitrary = oneof [arbitraryProp, arbitraryNeg, arbitraryCnj, arbitraryDsj,
                      arbitraryImpl, arbitraryEquiv]
      where arbitraryProp = do
              x <- arbitrary
              return $ Prop x
            arbitraryNeg = do
              x <- (arbitrary :: Gen Form)
              return $ Neg x
            arbitraryCnj = do
              x <- (arbitrary :: Gen Form)
              return $ Cnj [x]
            arbitraryDsj = do
              x <- (arbitrary :: Gen Form)
              return $ Dsj [x]
            arbitraryImpl = do
              x <- (arbitrary :: Gen Form)
              y <- (arbitrary :: Gen Form)
              return $ Impl x y
            arbitraryEquiv = do
              x <- (arbitrary :: Gen Form)
              y <- (arbitrary :: Gen Form)
              return $ Equiv x y
-- -- > sample $ (arbitrary :: Gen Form)

-- Generate bound form:
arbitrarySizedForm :: Gen (Form)
arbitrarySizedForm = sized form where
  form :: Int -> Gen (Form)
  form 0       = return Prop 1
  form n | n>0 = oneof [return Prop subform,
                        return Neg subform,
                        return Cnj [subform]]
                  where subform = form (n `div` 2)
