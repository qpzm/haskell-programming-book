{-# LANGUAGE PackageImports #-}

module ChapterExercise2 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import "checkers" Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (Two a f) <*> (Two b x) = Two (a <> b) (f x)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three x y <$> arbitrary

data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    Three x y f <*> Three u v z = Three (x <> u) (y <> v) (f z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' a f g) <*> (Three' b x y) = Three' (a <> b) (f x) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three' x y z

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        u <- arbitrary
        return $ Four x y z u

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    Four' a a' a'' f <*> Four' b b' b'' x = Four' (a <> b) (a' <> b') (a'' <> b'') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        u <- arbitrary
        return $ Four' x y z u

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

x = ("a", "a", "a")

main :: IO ()
main = do
    quickBatch $ applicative (Pair x x)
    quickBatch $ functor (Pair x x)
    quickBatch $ applicative (Two x x)
    quickBatch $ functor (Two x x)
    quickBatch $ functor (Three x x x)
    quickBatch $ applicative (Three x x x)
    quickBatch $ functor (Four x x x x)
    quickBatch $ applicative (Four x x x x)
    quickBatch $ functor (Four' x x x x)
    quickBatch $ applicative (Four' x x x x)
