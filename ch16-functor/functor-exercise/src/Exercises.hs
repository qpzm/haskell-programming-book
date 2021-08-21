module Exercises (BoolAndSomethingElse) where

import Data.Array
import Test.QuickCheck

data Bool' = False' | True' deriving (Eq, Show)
-- No functor instance. Error message: Expected kind * -> * but Bool' is *
-- Functor f, Eq (f a) 이렇게 constraint가 있으므로 Functor f 의 kind는 * -> *
--instance Functor Bool' where
    --fmap f True' = True'
    --fmap f False' = False'

data BoolAndSomethingElse a = False'' a | True'' a deriving (Eq, Show)
instance Functor BoolAndSomethingElse where
    fmap f (False'' a) = False'' (f a)
    fmap f (True'' a) = True'' (f a)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
      oneof [True'' <$> arbitrary, False'' <$> arbitrary]

data BoolAndMaybeSomethingElse a = Falsish | Truish a

newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int
