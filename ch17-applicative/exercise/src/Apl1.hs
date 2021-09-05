module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Semigroup a => Semigroup (ZipList a) where
    (<>) = liftA2 (<>) -- Q. liftA

instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList (repeat mempty)

--instance Arbitrary a => Arbitrary (ZipList a) where
    --arbitrary = ZipList <$> arbitrary

--instance Arbitrary a => Arbitrary (Sum a) where
    --arbitrary = Sum <$> arbitrary

--instance Eq a => EqProp (ZipList a) where
    --(=-=) = eq

main = do
    let zl = ZipList [1 :: Sum Int]
    quickBatch $ monoid zl
