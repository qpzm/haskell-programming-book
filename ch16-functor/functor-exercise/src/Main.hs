module Exercises where

import Data.Array

data Bool' = False' | True'
-- No functor instance. Error message: Expected kind * -> * but Bool' is *
-- Functor f, Eq (f a) 이렇게 constraint가 있으므로 Functor f 의 kind는 * -> *
--instance Functor Bool' where
    --fmap f True' = True'
    --fmap f False' = False'

data BoolAndSomethingElse a = False'' a | True'' a
instance Functor BoolAndSomethingElse where
    fmap f (False'' a) = False'' (f a)
    fmap f (True'' a) = True'' (f a)

data BoolAndMaybeSomethingElse a = Falsish | Truish a

newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int

main :: IO ()
main = do
  putStrLn "hello world"
  quickCheck (functorIdentity (f :: [Int] -> Bool))
