module Exercises (BoolAndSomethingElse, BoolAndMaybeSomethingElse) where

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


data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)
instance Functor BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish x) = Truish (f x)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary = do
        oneof [return Falsish, Truish <$> arbitrary]


newtype Mu f = InF { outF :: f (Mu f) }


data D = D (Array Word Word) Int Int
instance Functor


-- 2-1.
-- Q. 왜 순서가 중요하지?
data Sum b a = First a | Second b
instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

-- 2-2.
data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 2-3.
data More b a = L a b a | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


-- 3-1.
data Quant a b = Finance | Desk a | Bloor b

--instance Functor Quant a

-- 3-2.
data K a b = K a

