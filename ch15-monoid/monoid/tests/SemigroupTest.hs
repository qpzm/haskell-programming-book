module Main where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)
import Monoid

main :: IO ()
main = do
    print "Check Semigroup"
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: (IdentityAssoc String))
    quickCheck (semigroupAssoc :: (TwoAssoc String (Sum Int)))
    quickCheck (semigroupAssoc :: (ThreeAssoc String (Sum Int) (Product Int)))
    quickCheck (semigroupAssoc :: (FourAssoc String (Sum Int) (Product Int) String))
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: (OrAssoc String (Sum Int)))
    -- TODO quickCheck Combine, Comp
    -- quickCheck (semigroupAssoc :: CombAssoc)
    checkCombine
    checkValidation

checkCombine :: IO ()
checkCombine = do
    let f = Combine $ \n -> Sum (n + 1)
        g = Combine $ \n -> Sum (n - 1)
    print $ unCombine (f <> g) 0 == (Sum 0 :: Sum Int)
    print $ unCombine (f <> g) 1 == Sum 2
    print $ unCombine (f <> f) 1 == Sum 4
    print $ unCombine (g <> f) 1 == Sum 2


checkValidation :: IO( )
checkValidation = do
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
