module Semigroup where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)

-- Semigroup
semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

-- Trivial
data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
    Trivial <> Trivial = Trivial
instance Arbitrary Trivial where
    arbitrary = return Trivial
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Two
data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two u v) = Two (x <> u) (y <> v)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)
type TwoAssoc a b = Two a b-> Two a b -> Two a b -> Bool

-- Three
data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x y z) <> (Three a b c) = Three (x <> a) (y <> b) (z <> c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four w x y z) <> (Four a b c d) = Four (w <> a) (x <> b) (y <> c) (z <> d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)
type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- TODO
-- BoolConj : <> works like And
newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)
instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return (BoolConj a)
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj : <> works like Or
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)
instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        return (BoolDisj a)
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Fst x) <> (Fst y) = Fst (x <> y)
    (Snd x) <> (Snd y) = Snd (x <> y)
    _ <> (Snd y) = Snd y
    (Snd x) <> _ = Snd x
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a , return $ Snd b]
type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

newtype Combine a b = Combine { unCombine :: a -> b }
instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine $ \x -> f x <> g x

----------------------------------------------
    -- TODO quickCheck Combine
genFunc :: (CoArbitrary a, Arbitrary a) => Gen (a -> a)
genFunc = arbitrary

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- genFunc
  return (Comp { unComp = f })

type CombAssoc = Combine Int (String -> String)
              -> Combine Int (String -> String)
              -> Combine Int (String -> String)
              -> Bool
----------------------------------------------
newtype Comp a = Comp { unComp :: a -> a }
instance (Semigroup a) => Semigroup (Comp a) where
    Comp f <> Comp g = Comp $ \x -> f x <> g x

data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
    (Success a) <> (Success b) = Success a
    (Success a) <> (Failure b) = Success a
    (Failure a) <> (Success b) = Success b
    (Failure a) <> (Failure b) = Failure (a <> b)
