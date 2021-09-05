{-# LANGUAGE PackageImports #-}

module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import "checkers" Test.QuickCheck.Classes

data CountMe a =
    CountMe Integer a
    deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
    return = pure
    CountMe n a >>= f =
        let CountMe m b = f a in
            CountMe (n + m) b

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq


-- All pass
main = do
    let trigger :: CountMe (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
