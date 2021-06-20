module Main where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

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
    (Two x y) <> (Two u v) = Two (x <> u) (y <> y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)
type TwoAssoc a b = Two a b-> Two a b -> Two a b -> Bool

-- TODO
-- BoolConj : <> works like And
newtype BoolConj = BoolConj Bool

-- BoolDisj : <> works like Or
newtype BoolDisj = BoolDisj Bool

data Or a b = Fst a | Snd b

newtype Combine a b = Combine { unCombine :: (a -> b) }

newtype Comp a = Comp { unComp :: (a -> a) }

data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where 
    (<>) = undefined

main :: IO ()
main = do
    print "Check Semigroup"
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: (IdentityAssoc String))
    quickCheck (semigroupAssoc :: (TwoAssoc String (Sum Int)))
