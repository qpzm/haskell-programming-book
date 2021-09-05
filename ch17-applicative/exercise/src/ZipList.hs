module ZipList' where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 l = Nil
take' n Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h $ fold f b t

concat' :: List (List a) -> List a
concat' l = fold append Nil l

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

-- https://stackoverflow.com/a/45596657/9379031
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = take' 3000 xs
              ys' = take' 3000 ys

--------------------------------------------------------

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)


zipWith' :: Semigroup a => List a -> List a -> List a
zipWith' Nil _ = Nil
zipWith' _ Nil = Nil
zipWith' (Cons x xs) (Cons y ys) = Cons (x <> y) (zipWith' xs ys)

zipWith'' :: List (a -> b) -> List a -> List b
zipWith'' Nil _ = Nil
zipWith'' _ Nil = Nil
zipWith'' (Cons f fs) (Cons x xs) = Cons (f x) (zipWith'' fs xs)


instance Semigroup a => Semigroup (ZipList' a) where
    ZipList' l1 <> ZipList' l2 = ZipList' $ zipWith' l1 l2 -- Q. liftA

instance Monoid a => Monoid (ZipList' a) where
    mempty = ZipList' (toList $ repeat mempty)

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

--instance Arbitrary a => Arbitrary (Sum a) where
    --arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

-- identity, composition, functor 만족 안 함. 왜 functor 도 만족 안 하지?
instance Applicative ZipList' where
    pure x = ZipList' (toList $ repeat x)
    ZipList' l1 <*> ZipList' l2 = ZipList' $ zipWith'' l1 l2

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

-- Wrong answer
-- https://stackoverflow.com/a/58597711/9379031
--instance Applicative ZipList' where
  --pure x = ZipList' (Cons x Nil)
  --(<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ (zipListWith fs xs)

--zipListWith :: List (a -> b) -> List a -> List b
--zipListWith _ Nil = Nil
--zipListWith Nil _ = Nil
--zipListWith (Cons f Nil) (Cons x xs) = Cons (f x) (pure f <*> xs)
--zipListWith (Cons f fs) (Cons x Nil) = Cons (f x) (fs <*> pure x)
--zipListWith (Cons f fs) (Cons x xs)  = Cons (f x) (zipListWith fs xs)

append' :: ZipList' a -> ZipList' a -> ZipList' a
append' (ZipList' l1) (ZipList' l2) = ZipList' $ append l1 l2

test =
    let zl' = ZipList'
        z = zl' $ toList [(+9), (*2), (+8)]
        z' = zl' $ toList (repeat 1)
      in z <*> z' -- [10, 2, 9]

main = do
     quickBatch $ applicative (undefined :: List (String, String, Int))
     quickBatch $ applicative (undefined :: ZipList' (String, String, Int))
