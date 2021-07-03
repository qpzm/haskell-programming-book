{-# LANGUAGE NoImplicitPrelude #-}

module PrettyReader where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const :: a -> b -> a
const a b = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \a -> f (g a)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
    return :: a -> f a
    (>>=) :: f a -> (a -> f b) -> f b

instance Functor ((->) r) where
    fmap = (.)

instance Applicative ((->) r) where
    pure = const
    f <*> a = \r -> f r (a r)

-- 함수 타입의 경우 Applicative 로 Monad를 만들 수 있다.
-- 따라서 Reader Monad 자체는 Applicative 를 넘어서는 기능이 없다.
-- m :: r -> a
-- k :: a -> r -> b
-- flip k <*> m
-- :: r  -> a -> b <*> r -> a
-- :: r -> b
instance Monad ((->) r) where
    return = pure
    m >>= k = flip k <*> m
