-- Assignment: Lab3
-- Exercise: 4
-- Authors: Quinten Heijn, Dylan Bartels,
--          Wojciech Czabański, Elias El Khaldi Ahanach
-- Time needed: 
--------------------------------------------------------------------------

module Assignment4 where
import Data.List
import Control.Monad
import Test.QuickCheck
import Lecture3

instance Arbitrary Form where
    arbitrary = oneof [arbitraryProp, arbitraryNeg,
                       arbitraryProp, arbitraryCnj,
                       arbitraryProp, arbitraryDsj,
                       arbitraryProp, arbitraryImpl,
                       arbitraryEquiv]
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
               