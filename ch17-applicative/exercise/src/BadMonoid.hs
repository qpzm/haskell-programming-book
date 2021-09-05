{-# LANGUAGE PackageImports #-}

module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools


instance Monoid Bull where
    mempty = Fools

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = do
    quickBatch (monoid Twoo) -- Twoo 라는 값이 쓰이는 게 아니라 해당 Type 정보만 사용
    let xs = [("b", "w", 1)] :: [(String, String, Int)] in
        quickBatch $ applicative xs
