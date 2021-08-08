module Main where

import Test.QuickCheck
import Monoid
import Test.Hspec

main :: IO ()
main = do
    print "Check monoidAssoc"
    quickCheck (semigroupAssoc :: S -> S -> S -> B)
    quickCheck (monoidLeftIdentity :: S -> B)
    quickCheck (monoidRightIdentity :: S -> B)

    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

    quickCheck (semigroupAssoc :: IdentityAssoc S)
    quickCheck (monoidLeftIdentity :: Identity S -> Bool)
    quickCheck (monoidRightIdentity :: Identity S -> Bool)

    quickCheck (semigroupAssoc :: TwoAssoc S S)
    quickCheck (monoidLeftIdentity :: Two S S -> Bool)
    quickCheck (monoidRightIdentity :: Two S S -> Bool)

    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

    -- TODO quickCheck Comp, Comb

    hspec $ do
        it "Mem combines functions correctly" $ do
            let f' = Mem $ \x -> ("Hi", x + 1)
            runMem (f' <> mempty) 0 `shouldBe` ("Hi", 1)
            runMem (mempty <> f') 0 `shouldBe` ("Hi", 1)
            (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0)
            runMem (f' <> mempty) 0 == runMem f' 0 `shouldBe` True
            runMem (mempty <> f') 0 == runMem f' 0 `shouldBe` True
    -- verboseCheck (monoidAssoc :: S -> S -> S -> B)
    -- 아래처럼 어떤 인자가 들어왔는지 표시
    --Passed:
    --"\v.*\FSJ\v\ETX"
    --"\fC\SUB\ACK{C(a\1095968\&5\49407\SYN.\132039+\1003278q]s"
    --"\183537\995864~3\45834%M\1100928\100080z\1038853H3).V\101087?n7\1014289"
