module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Data.Bifunctor

-- How to run
-- $ ghci
-- > :l filename
-- Main> main

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen


data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

-- Arbitrary Products
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

-- Sum
data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof $ map return [First a, Second b]

-- Q. 왜 instance Arbtrary 안 만들어도 되지?

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
    a <- arbitrary
    b <- arbitrary
    -- second return == bimap id return
    frequency $ fmap (second return) [(10, First a), (1, Second b)]

main = do
    sample trivialGen
    sample (identityGen :: Gen (Identity Int))
    sample (pairGen :: Gen (Pair Int String))
    sample (sumGenEqual :: Gen (Sum Char Int))
    sample (sumGenFirstPls :: Gen (Sum Char Int))
